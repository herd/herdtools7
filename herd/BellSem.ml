(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2013-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

(** Semantics of Bell *)

module Make (C:Sem.Config)(V:Value.S)
    =
  struct
    module Bell = BellArch_herd.Make(SemExtra.ConfigToArchConfig(C))(V)
    module Act = BellAction.Make(Bell)
    include SemExtra.Make(C)(Bell)(Act)

    let compat = C.variant Variant.BackCompat

(* Not doing barrier pretty print *)
    let barriers = []
    let isync = None

(* Simple size *)
    let reg_sz = V.Cst.Scalar.machsize
    and nat_sz = V.Cst.Scalar.machsize

    let atomic_pair_allowed _ _ = true

(****************************)
(* Build semantics function *)
(****************************)

    module Mixed(SZ : ByteSize.S) = struct

      let (>>=) = M.(>>=)
      let (>>*=) = M.(>>*=)
      let (>>|) = M.(>>|)
      let (>>::) = M.(>>::)
      let (>>!) = M.(>>!)

      let mk_read sz ato s loc v = Act.Access (Dir.R, loc, v, ato, s, sz)

      let read_reg is_data ?(stack=[]) r ii =
        try
          let v = List.assoc r stack in (M.unitT v)
        with Not_found ->
          M.read_loc is_data (mk_read reg_sz false []) (A.Location_reg (ii.A.proc,r)) ii

      let read_reg_ord = read_reg false
      and read_reg_data = read_reg true

      let read_mem sz a s ii =
        M.read_loc false (mk_read sz false s) (A.Location_global a) ii

      let read_mem_atom sz a s ii =
        M.read_loc false (mk_read sz true s) (A.Location_global a) ii


(*    let read_mem_atom cop a ii =
      M.read_loc (mk_read true cop) (A.Location_global a) ii *)

      let write_reg r v ii =
        M.mk_singleton_es (Act.Access (Dir.W, (A.Location_reg (ii.A.proc,r)), v, false, [], reg_sz)) ii

      let write_mem sz a v s ii =
        M.mk_singleton_es (Act.Access (Dir.W, A.Location_global a, v, false, s, sz)) ii

      let write_mem_atom sz a v s ii =
        M.mk_singleton_es (Act.Access (Dir.W, A.Location_global a, v, true, s, sz)) ii


      let commit ii =  M.mk_singleton_es (Act.Commit) ii


      let create_barrier b o ii =
        M.mk_singleton_es (Act.Barrier(b,o)) ii

      let read_roa is_data ?(stack=[]) roa ii =
        match roa with
        | BellBase.Rega r -> read_reg is_data ~stack:stack r ii
        | BellBase.Abs a -> (M.unitT (V.maybevToV a))

      let read_roi is_data roi ii =
        match roi with
        | BellBase.Regi r -> read_reg is_data r ii
        | BellBase.Imm i -> (M.unitT (V.intToV i))

      let read_iar is_data ?(stack=[]) roi ii =
        match roi with
        | BellBase.IAR_roa roa -> read_roa is_data ~stack:stack roa ii
        | BellBase.IAR_imm i -> (M.unitT (V.intToV i))

      let solve_addr_op ao ii = match ao with
      | BellBase.Addr_op_atom roa -> read_roa false roa ii
      | BellBase.Addr_op_add(roa,roi) ->
          (read_roa false roa ii >>|
          read_roi false roi ii) >>=
          (fun (v1,v2) -> M.op Op.Add v1 v2)

      let tr_op ?(stack=[]) ii = function
        | BellBase.OP(bell_op,x,y) ->
            let op = match bell_op with
            | BellBase.Xor -> Op.Xor
            | BellBase.Add -> Op.Add
            | BellBase.And -> Op.And
            | BellBase.Eq -> Op.Eq
            | BellBase.Neq -> Op.Ne
            in
            ((read_iar false ~stack:stack x ii) >>| (read_iar false ~stack:stack y ii)) >>=
            (fun (v1,v2) -> M.op op v1 v2)
        | BellBase.RAI(i) -> (read_iar false ~stack:stack i ii)

      let tr_mov r op ii =
        (tr_op ii op) >>= (fun v -> write_reg r v ii)

      let build_semantics ii =
        let build_semantics_inner ii =
          match ii.A.inst with
          | BellBase.Pnop -> M.unitT B.Next
          | BellBase.Pld(r,addr_op,[("deref"|"lderef")]) when compat ->
              solve_addr_op addr_op ii >>=
              fun addr -> read_mem nat_sz addr ["once"] ii >>=
                fun v -> write_reg r v ii >>*=
                  fun () -> create_barrier ["rb_dep"] None ii >>! B.Next
          | BellBase.Pld(r,addr_op,s) ->
              solve_addr_op addr_op ii >>=
              (fun addr -> read_mem nat_sz addr s ii) >>=
              (fun v -> write_reg r v ii) >>! B.Next

          | BellBase.Pst(addr_op, roi, s) ->
              let s = match s with
              | ["assign"] when compat -> ["release"]
              | _ -> s in
              (solve_addr_op addr_op ii >>|
              read_roi true roi ii) >>=
              (fun (addr,v) -> write_mem nat_sz addr v s ii) >>!
              B.Next

          | BellBase.Pfence(BellBase.Fence (s,o)) ->
              create_barrier s o ii >>! B.Next

          | BellBase.Pcall _ ->
              Warn.fatal "Obsolete 'call' instruction in BellSem\n"

          | BellBase.Prmw(r,op,addr_op,s) ->
              let rloc = solve_addr_op addr_op ii in
              if BellBase.r_in_op r op then
                rloc >>=
                (fun x -> (read_mem_atom nat_sz x s ii) >>=
                  (fun v_read ->
                    (tr_op ~stack:[(r,v_read)] ii op) >>=
                    (fun v -> write_reg r v_read ii >>|
                    write_mem_atom nat_sz x v s ii))) >>!
                B.Next
              else begin
                rloc >>=
                (fun x ->
                  let r1 = read_mem_atom nat_sz x s ii
                  and r2 = tr_op ii op
                  and w1 = fun v -> write_mem_atom nat_sz x v s ii
                  and w2 = fun v -> write_reg r v ii in
                  M.exch r1 r2 w1 w2) >>! B.Next
              end
          | BellBase.Pbranch(Some r,lbl,_) ->
              (read_reg false r ii) >>=
              (fun v -> commit ii >>= fun () -> B.bccT v lbl)

          | BellBase.Pbranch(None ,lbl,_) ->  B.branchT lbl

          | BellBase.Pmov(r,op) ->
              (tr_mov r op ii) >>! B.Next

        in
        M.addT (A.next_po_index ii.A.program_order_index) (build_semantics_inner ii)
    end
  end
