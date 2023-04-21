(****************************************************************************)
(*                           The diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2023-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

module
  Make
    (S:SemExtra.S with type A.reg = AArch64Base.reg)
    (U:sig
         open S

         val mops_size : MachSize.sz
         val endian : Endian.t

         val nzcv : A.reg

         val read_reg_ord :
           A.reg -> A.inst_instance_id -> A.V.v M.t
         val read_reg_data :
           MachSize.sz -> A.reg -> A.inst_instance_id -> A.V.v M.t
         val write_reg :
           A.reg -> A.V.v -> A.inst_instance_id -> unit M.t
         val write_reg_dest :
           A.reg -> A.V.v -> A.inst_instance_id -> A.V.v M.t
         val do_append_commit :
          'a M.t -> string option -> A.inst_instance_id -> 'a M.t
         val is_this_reg :
           A.reg -> event -> bool

         val read_mem :
           MachSize.sz -> A.V.v -> A.inst_instance_id -> A.V.v M.t
         val write_mem :
           MachSize.sz -> A.V.v -> A.V.v -> A.inst_instance_id
           -> unit M.t
       end)
  =  struct
  open S
  module C = O
  module AArch64 = A
  open A

  let debug_mops = C.debug.Debug_herd.mops

  let (>>=) = M.(>>=)
  let (>>==) = M.(>>==)
  let (>>!) = M.(>>!)
  let (>>|) = M.(>>|)

  let are_this_reg_read2 r1 r2 e =
    E.is_load e
    && begin
        match E.location_reg_of e with
        | None -> false
        | Some r ->
           AArch64.reg_compare r1 r=0
           || AArch64.reg_compare r2 r=0
      end

(*************)
(* FEAT_MOPS *)
(*************)

  type opt = OptA | OptB

  let pp_opt = function OptA -> "A" | OptB -> "B"

  let opt = if C.variant Variant.SwitchMops then OptA else OptB

  let do_set_nzcv v ii =
    U.write_reg_dest U.nzcv (V.intToV v) ii

  let set_nzcv ii =
    let v =
      match opt with OptA -> 0b0100 | OptB -> 0b0000 in
    do_set_nzcv v ii


  let fail_address ii v =
    Warn.user_error
      "%s instruction cannot operate on address %s"
      (AArch64.dump_instruction ii.AArch64.inst)
      (A.V.pp_v v)

  let fail_count ii v =
    Warn.user_error
      "%s instruction cannot operate with count %s"
      (AArch64.dump_instruction ii.AArch64.inst)
      (V.pp false v)

  let fail_flags ii v =
    Warn.user_error
      "%s instruction cannot operate with NZCV register %s"
      (AArch64.dump_instruction ii.AArch64.inst)
      (V.pp false v)

  let as_constant asp = function
    | V.Var _ -> None
    | V.Val cst -> asp cst

  let as_integer n =
    as_constant
      (fun cst ->
        match cst with
        | Constant.Concrete i ->
           let i = V.Cst.Scalar.to_int i in
           Some i
        | _ -> None)
      n

  let value_offset v =
    as_constant
      (fun cst ->
        let open Constant in
        as_symbolic_data cst |>
          Misc.app_opt (fun {offset; _} -> offset))
      v |> Misc.as_some

(*
 * Add commit and control dependencies, only when stores are performed,
 * as conveyed by result "b" of m2.
*)
  let mk_mops_combi msg ii m1 m2 =
    M.delay_kont "M1" m1
      (fun r1 m1 ->
        M.delay_kont "M2" (M.unitT r1 >>= m2)
          (fun (r2,b) m2 ->
            begin
              if b then
                M.remove_control
                  (fun e1 e2 ->
                    E.is_commit e1 && E.is_reg_store_any e2)
                  (M.bind_ctrldata
                     (U.do_append_commit m1 (Some ("MOPS: "^msg)) ii)
                     (fun _ -> m2))
              else
                m1 >>= fun _ -> m2
            end >>! r2))

(* Copy combinator, with corrections *)
  let cpy_combine tag rd rs rn ii m1 m2 =
    mk_mops_combi tag ii m1 m2
    |> M.add_data
         (fun e1 e2 ->
           E.is_reg_load_any e1 &&
             begin
               (U.is_this_reg rd e1 && E.is_mem_store e2)
               || (U.is_this_reg rs e1 && E.is_mem_load e2)
             end)
    |> M.remove_data
         (fun e1 e2 ->
           E.is_reg_load_any e1 &&
             begin
               (U.is_this_reg rd e1 &&
                  (E.is_mem_load e2 || are_this_reg_read2 rs rn e2))
               || (U.is_this_reg rs e1 &&
                     (E.is_mem_store e2 || are_this_reg_read2 rd rn e2))
               || (U.is_this_reg rn e1 && E.is_mem e2)
             end)

(*
 * Returns r, s.t. o+r is aligned for size instr_sz (optin B).
 * Option A changes of size n and address are handled
 * by computing original values.
 *)

  let preSizeChoice =
    let open Constant in
    fun instr_sz {offset=o;_} n ->
    let o,n =
      match opt with
      | OptA when n < 0 ->
         let o = o+n in
         o,-n
      | OptA|OptB -> o,n in
    let sz = MachSize.nbytes instr_sz in
    let r = min n ((sz-(o mod sz)) mod sz) in
    match opt with
    | OptA -> 0-r
    | OptB -> r

(********)
(* Copy *)
(********)

  let seq =
    if C.variant (Variant.T 0) then
      fun m1 m2 -> (m1 >>| m2) >>= fun (_,r) -> M.unitT r
    else M.seq

  let cpy_sz = U.mops_size

  let do_read_cpy_regs rd rs rn ii =
    begin
      U.read_reg_ord rd ii
      >>| U.read_reg_ord rs ii
      >>| U.read_reg_ord rn ii
    end >>=
      fun ((vd,vs),n) ->
      let d =
        match as_constant Constant.as_symbolic_data vd with
        | None -> fail_address ii vd
        | Some d -> d
      and s =
        match as_constant Constant.as_symbolic_data vs with
        | None -> fail_address ii vs
        | Some s -> s
      and n =
        match as_integer n with
        | None -> fail_count ii n
        | Some n -> n in
      M.unitT ((vd,d),(vs,s),n)

      let do_read_cpy_regs_nzcv rd rs rn ii =
        do_read_cpy_regs rd rs rn ii >>|
        begin
          U.read_reg_ord U.nzcv ii >>=
          fun v ->
            match as_integer v with
            | None -> fail_flags ii v
            | Some v -> M.unitT v
        end

(* Forward *)

(*
 * Prologue copy size aims at aligning the
 *  writes of cpym instruction.
 *)
      let cpyPreSizeChoice d _ n = preSizeChoice cpy_sz d n

      let rec read_misaligned os sz ea ii =
        let nsz = MachSize.nbytes sz in
        let sz0 = os mod nsz in
        if sz0 = 0 then
          U.read_mem sz ea ii
        else
          let sz2 = MachSize.pred sz
          and nsz2 = nsz/2 in
          begin
            read_misaligned os sz2 ea ii >>|
            (M.op Op.Add ea (V.intToV nsz2) >>=
             fun ea -> read_misaligned (os+nsz2) sz2 ea ii)
          end >>= fun (low,high) ->
            match U.endian with
            | Endian.Little ->
                M.op1 (Op.LeftShift (MachSize.nbits sz2)) high >>=
                fun high -> M.op Op.Or high low
            | Endian.Big ->
                M.op1 (Op.LeftShift (MachSize.nbits sz2)) low >>=
                fun low -> M.op Op.Or high low


      let cpyf_nobjs st rd rs rn sz vd (vs,_) n m ii =
        let open AArch64Base in
        let delta = MachSize.nbytes sz in
        let over =
          match opt with
          | OptA -> fun m -> -m < delta
          | OptB -> fun m -> m < delta in
        if over m then
          begin
            let vn = V.intToV n in
            match opt,st with
            | OptA,Prologue ->
                begin
                  U.write_reg rn vn ii >>|
                  U.write_reg rd vd ii >>|
                  U.write_reg rs vs ii
                end >>=
                fun _ -> M.unitT (vd,vs,vn)
            | _ ->
                M.unitT (vd,vs,vn)
          end >>= fun r -> M.unitT (r,false)
        else
          let vdelta = V.intToV delta in
          begin
            let rec do_rec vd vs n m =
              let vn = V.intToV n in
              if over m then M.unitT (vd,vs,vn)
              else
                begin
                  match opt with
                  | OptA ->
                      seq
                        (M.add vs vn >>= fun src ->
                          read_misaligned
                            (value_offset src) sz src ii >>= fun data ->
                              M.add vd vn >>= fun dest ->
                                U.write_mem sz dest data ii)
                        (do_rec vd vs (n+delta) (m+delta))
                  | OptB ->
                      seq
                        (read_misaligned
                           (value_offset vs) sz vs ii >>==  fun data ->
                         U.write_mem sz vd data ii)
                        ((M.add vd vdelta >>| M.add vs vdelta) >>=
                         fun (vd,vs) -> do_rec vd vs (n-delta) (m-delta))
                end in
            M.delay_kont "CPYF"
              (do_rec vd vs n m)
              (fun (vd,vs,vn) m ->
                m >>|
                begin
                  let wrn = U.write_reg rn vn ii
                  and wrs = U.write_reg rs vs ii
                  and wrd = U.write_reg rd vd ii in
                  match opt with
                  | OptA ->
                      begin
                        let open AArch64Base in
                        match st with
                        | Prologue ->
                            (wrs >>| wrd >>| wrn) >>! ()
                        | Main|Epilogue ->
                            wrn
                      end
                  | OptB ->
                      (wrs >>| wrd >>| wrn) >>! ()
                end >>= fun (r,_) -> M.unitT (r,true))
          end

      let add_offset v c n =
        M.add v (V.intToV n) >>=
        fun v -> M.unitT (v,Constant.add_offset c n)

      let option_params_forward opt st m =
        let open AArch64Base in
        match opt,st with
        | OptA,Prologue ->
            m >>= fun ((vd,d),(vs,s),n) ->
              (add_offset vd d n >>| add_offset vs s n)
                >>= fun (d,s) -> M.unitT (d,s,0-n)
        | (OptB,_)|(OptA,(Main|Epilogue)) ->
            m

      let read_cpyf_regs st rd rs rn ii =
        do_read_cpy_regs rd rs rn ii |> option_params_forward opt st

      let cpyf st rd rs rn ii =
        let (>>>) m1 m2 = cpy_combine "CPYF" rd rs rn ii m1 m2 in
        let open AArch64Base in
        match st with
        | Prologue ->
            M.para_output_left
              (read_cpyf_regs st rd rs rn ii)
              (set_nzcv ii) >>>
            fun ((vd,d),(_,s as ps),n) ->
              let m = cpyPreSizeChoice d s n  in
              cpyf_nobjs st rd rs rn MachSize.Byte vd ps n m ii
        | Epilogue ->
            read_cpyf_regs st rd rs rn ii >>>
            fun ((vd,_),s,n) ->
              cpyf_nobjs st rd rs rn MachSize.Byte vd s n n ii
        | Main ->
            read_cpyf_regs st rd rs rn ii >>>
            fun ((vd,_),s,n) ->
              cpyf_nobjs st rd rs rn cpy_sz vd s n n ii


(* Flexible copy *)

      type direction = Forward | Backward

      let pp_dir = function
        | Forward -> "->"
        | Backward -> "<-"

(* For non-overlapping transfer, arbitrary choice *)
      let default_direction =
        if C.variant Variant.SwitchMopsDir then Forward else Backward

      let compute_direction d s n =
        let open Constant in
        if Misc.string_eq s.name d.name then
          let os = s.offset and od = d.offset in
          if os > od && od+n > os then Forward
          else if os < od && os+n > od then Backward
          else default_direction
        else default_direction

      let option_params_cpy dir (vd,d as pd) (vs,s as ps) n =
        match opt,dir with
        | OptA,Forward ->
            (add_offset vd d n >>| add_offset vs s n) >>=
            fun (d,s) -> M.unitT (d,s,0-n,0b0000)
        | OptA,Backward ->
            M.unitT (pd,ps,n,0b0000)
        | OptB,Forward ->
            M.unitT (pd,ps,n,0b0010)
        | OptB,Backward ->
            (add_offset vd d n >>| add_offset vs s n) >>=
            fun (d,s) -> M.unitT (d,s,n,0b1010)

      let cpy_write_final st rd rs rn vd vs vn nzcv ii =
        let wrn = U.write_reg rn vn ii
        and wrd = U.write_reg rd vd ii
        and wrs = U.write_reg rs vs ii in
        let open AArch64Base in
        match st with
        | Prologue ->
            (wrn >>| wrd >>| wrs >>| do_set_nzcv nzcv ii) >>=
            fun _ -> M.unitT (vd,vs,vn)
        | _ ->
            (wrn >>| wrd >>| wrs) >>=
            fun _ -> M.unitT (vd,vs,vn)

      let cpy_nobjs_optA st rd rs rn sz vd vs n nzcv m ii =
        if m = 0 then
          let vn = V.intToV n in
          begin
            let open AArch64Base in
            match st with
            | Prologue ->
                cpy_write_final st rd rs rn vd vs vn nzcv ii
            | _ ->
(* Do not write anything in this case of option A, non-proplogue *)
                M.unitT (vd,vs,vn)
          end >>= fun r -> M.unitT (r,false)
        else
          let delta = MachSize.nbytes sz in
          let m_rec =
            if n < 0 then
              let rec do_rec  n m =
                let vn = V.intToV n in
                if m = 0 then M.unitT (vd,vs,vn)
                else
                  seq
                    (M.add vs vn >>= fun src ->
                      read_misaligned
                        (value_offset src) sz src ii >>= fun data ->
                          M.add vd vn >>= fun dest ->
                            U.write_mem sz dest data ii)
                    (do_rec (n+delta) (m+delta)) in
              do_rec n m
            else
              let rec do_rec m n =
                if m=0 then
                  let vn = V.intToV n in
                  M.unitT (vd,vs,vn)
                else
                  let m = m-delta and n = n-delta in
                  let vn = V.intToV n in
                  seq
                    (M.add vs vn >>= fun src ->
                      read_misaligned
                        (value_offset src) sz src ii >>= fun data ->
                          M.add vd vn >>= fun dest ->
                            U.write_mem sz dest data ii)
                    (do_rec n m) in
              do_rec n m in
          M.delay_kont "cpyA"
            m_rec
            (fun (vd,vs,vn) m ->
              m >>|
              begin
                let open AArch64Base in
                match st with
                | Prologue ->
                    cpy_write_final st rd rs rn vd vs vn nzcv ii >>! ()
                | _ ->
                    U.write_reg rn vn ii
              end)
            >>= fun (r,_) -> M.unitT (r,true)


      let cpy_nobjs_optB st rd rs rn sz vd vs n nzcv m ii =
        if m <= 0 then
          let vn = V.intToV n in
          begin
            let open AArch64Base in
            match st with
            | Prologue ->
                cpy_write_final st rd rs rn vd vs vn nzcv ii
            | _ -> M.unitT (vd,vs,vn)
          end >>= fun r -> M.unitT (r,false)
        else
          let delta = MachSize.nbytes sz in
          let vdelta = V.intToV delta in
          let n_zero = nzcv land 0b1000 = 0 in
          let rec do_rec vd vs n m =
            if m <= 0 then
              M.unitT (vd,vs,V.intToV n)
            else if n_zero then
              seq
                (read_misaligned (value_offset vs) sz vs ii >>= fun data ->
                  U.write_mem sz vd data ii)
                ((M.add vd vdelta >>| M.add vs vdelta) >>=
                 fun (vd,vs) -> do_rec vd vs (n-delta) (m-delta))
            else begin
              let msubs =
                M.op Op.Sub vd vdelta >>| M.op Op.Sub vs vdelta in
              seq
                (msubs >>= fun (vd,vs) ->
                  read_misaligned (value_offset vs) sz vs ii
                    >>= fun data -> U.write_mem sz vd data ii)
                (msubs >>= fun (vd,vs) ->
                  do_rec vd vs (n-delta) (m-delta)) end in
          M.delay_kont "cpyB"
            (do_rec vd vs n m)
            (fun (vd,vs,vn) m ->
              m >>| cpy_write_final st rd rs rn vd vs vn nzcv ii)
            >>= fun (r,_) -> M.unitT (r,true)

      let cpy_nobjs st rd rs rn sz vd vs n nzcv m ii =
        match opt with
        | OptA ->
            cpy_nobjs_optA st rd rs rn sz vd vs n nzcv m ii
        | OptB ->
            cpy_nobjs_optB st rd rs rn sz vd vs n nzcv m ii

      let add_nzcv nzcv m = m >>= (fun (r,b) -> M.unitT ((nzcv,r),b))

      let cpy st rd rs rn ii =
        let (>>>) m1 m2 = cpy_combine "CPY" rd rs rn ii m1 m2 in
        let open AArch64Base in
        match st with
        | Prologue ->
            do_read_cpy_regs rd rs rn ii >>>
            fun ((_,d as pd),(_,s as ps),n) ->
              let dir = compute_direction d s n in
              option_params_cpy dir pd ps n >>=
              fun ((vd,d),(vs,s),n,nzcv) ->
                let m = cpyPreSizeChoice d s n in
                if debug_mops then
                  Printf.eprintf "CPYP, opt=%s, dir=%s, n=%d, m=%d, d=%s, s=%s nzcv=0x%x\n%!"
                    (pp_opt opt) (pp_dir dir) n m
                    (V.pp_v  vd) (V.pp_v vs) nzcv ;
                add_nzcv (Some nzcv)
                  (cpy_nobjs st rd rs rn MachSize.Byte vd vs n nzcv m ii)
        |  Main ->
            do_read_cpy_regs_nzcv rd rs rn ii >>>
            fun (((vd,_),(vs,_),n),nzcv) ->
              let delta = MachSize.nbytes cpy_sz in
              let m =
                if n > 0 then (n/delta)*delta
                else if n < 0 then -((-n)/delta)*delta
                else 0 in
              if debug_mops then
                Printf.eprintf
                  "CPYM, opt=%s, n=%d, m=%d, d=%s, s=%s nzcv=0x%x\n%!"
                  (pp_opt opt) n m  (V.pp_v vd) (V.pp_v vs) nzcv ;
              add_nzcv None
                (cpy_nobjs st rd rs rn cpy_sz vd vs n nzcv m ii)
        | Epilogue ->
            do_read_cpy_regs_nzcv rd rs rn ii >>>
            fun (((vd,_),(vs,_),n),nzcv) ->
              add_nzcv None
                (cpy_nobjs st rd rs rn MachSize.Byte vd vs n nzcv n ii)
(*******)
(* Set *)
(*******)

      let set_sz = U.mops_size

      let rec mreplicate n s =
        if n <= 1 then M.unitT s
        else
          mreplicate (n-1) s >>=
          M.op1 (Op.LeftShift 8) >>=
          M.op Op.Or s

      let write_nobjs st rd rn sz ea n rs m ii =
        let delta = MachSize.nbytes sz in
        let over =
          match opt with
          | OptA -> fun m -> -m < delta
          | OptB -> fun m -> m < delta in

        if over m then
          let vn = V.intToV n in
          let open AArch64Base in
          begin match opt,st with
          | OptA,Prologue ->
              (U.write_reg rn vn ii >>|  U.write_reg rd ea ii) >>=
              fun _ ->  M.unitT ((ea,vn),false)
          | (OptA,(Main|Epilogue))
          | (OptB,_)
            ->
              M.unitT ((ea,vn),false)
          end
        else
          let vdelta = V.intToV delta in
          begin
            let mset =
              M.data_input_next
                (U.read_reg_data MachSize.Byte rs ii >>= mreplicate delta)
                (fun s ->
                  let rec do_rec ea n m =
                    let vn = V.intToV n in
                    if over m then  M.unitT (ea,vn)
                    else
                      begin
                        match opt with
                        | OptA ->
                            (M.add ea vn >>= fun dest ->
                              U.write_mem sz dest s ii) >>|
                              do_rec ea (n+delta) (m+delta)
                            | OptB ->
                               U.write_mem sz  ea s ii >>|
                                 (M.op Op.Add ea vdelta >>=
                                 fun ea -> do_rec ea (n-delta) (m-delta))
                      end >>= fun (_,r) -> M.unitT r in
                  do_rec ea n m) in
            M.delay_kont "SET" mset
              (fun (ea,vn) m ->
                begin
                  m >>|
                  begin
                    let wrn = U.write_reg rn vn ii
                    and wrd = U.write_reg rd ea ii in
                    let open AArch64Base in
                    match opt,st with
                    | (OptA,Prologue)
                    | (OptB,_)
                      -> (wrd >>| wrn) >>! ()
                    | (OptA,(Main|Epilogue)) -> wrn
                  end
                end >>= fun (r,_) -> M.unitT (r,true))
          end

      let setPreSizeChoice = preSizeChoice set_sz

      let read_mset_regs st rd rn ii =
        begin
          U.read_reg_ord rd ii >>| U.read_reg_ord rn ii
        end >>=
        fun (vea,n) ->
          let ea =
            match as_constant Constant.as_symbolic_data vea with
            | None -> fail_address ii vea
            | Some ea -> ea
          and n =
            match as_integer n with
            | None -> fail_count ii n
            | Some n -> n in
          let open AArch64Base in
          match opt,st with
          | OptA,Prologue ->
              M.add vea (V.intToV n) >>=
              fun vea -> M.unitT  (vea,Constant.add_offset ea n,0-n)
          | (OptB,_)|(OptA,(Main|Epilogue)) ->
              M.unitT (vea,ea,n)

      let mset st rd rn rs ii =
        let (>>>) = mk_mops_combi "SET" ii in
        let open AArch64Base in
        match st with
        | Prologue ->
            M.para_output_left
              (read_mset_regs st rd rn ii)
              (set_nzcv ii)
              >>>
            fun (vea,ea,n) ->
              let m = setPreSizeChoice ea n  in
              write_nobjs st rd rn MachSize.Byte vea n rs m ii
        | Epilogue ->
            read_mset_regs st rd rn ii >>>
            fun (vea,_,n) -> write_nobjs st rd rn MachSize.Byte vea n rs n ii
        | Main ->
            read_mset_regs st rd rn ii >>>
            fun (vea,_,n) -> write_nobjs st rd rn set_sz vea n rs n ii

end
