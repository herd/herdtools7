(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2015-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

(* Java instruction semantics *)

module
  Make
    (Conf:Sem.Config)
    (V:Value.S with
      type Cst.Instr.exec = JavaBase.instruction
      and type arch_extra_op = JavaBase.arch_extra_op
      and type 'a arch_constr_op = 'a JavaBase.arch_constr_op) = struct

	module Java = JavaArch_herd.Make(SemExtra.ConfigToArchConfig(Conf))(V)
	module Act = JavaAction.Make(Java)

	include SemExtra.Make(Conf)(Java)(Act)

	let barriers = []
	let isync = None

	let nat_sz = V.Cst.Scalar.machsize

	let atomic_pair_allowed e1 e2 = (e1.E.iiid == e2.E.iiid)

	module Mixed(SZ : ByteSize.S) = struct

      let (>>=)   = M.(>>=)
      let (>>*=)  = M.(>>*=)
      let (>>|)   = M.(>>|)
      let (>>!)   = M.(>>!)
      let (>>>)   = M.cseq
      let (>>>>)  = M.(>>>>)

      let no_mo = AccessModes.NA

      let read_loc port mo =
        M.read_loc port (fun loc v -> Act.Access (Dir.R, loc, v, mo, nat_sz))

      let read_reg port r ii =
        read_loc port no_mo (A.Location_reg (ii.A.proc,r)) ii

      let read_mem port mo a =
        read_loc port mo (A.Location_global a)

      let write_loc mo loc v ii =
        M.mk_singleton_es (Act.Access (Dir.W, loc, v, mo, nat_sz)) ii >>! v

      let write_reg r v ii = write_loc no_mo (A.Location_reg (ii.A.proc,r)) v ii
      let write_mem mo a  = write_loc mo (A.Location_global a)
      let write_mem_atomic a loc v ii =
        M.mk_singleton_es
          (Act.Access (Dir.W, A.Location_global loc, v, a, nat_sz)) ii >>! v

      let fetch_op op v am l ii =
          M.fetch op v
          (fun v vstored ->
            Act.RMW (A.Location_global l, v, vstored, am, nat_sz))
            ii

      let rec build_semantics_expr port e ii : V.v M.t =
        match e with
        | JavaBase.LoadReg reg -> read_reg port reg ii

        | JavaBase.LoadMem (vh, am) -> (read_reg Port.Addr vh ii) >>=
                              (fun l -> read_mem port am l ii)

        | JavaBase.Const i -> M.unitT (V.maybevToV (ParsedConstant.intToV i))

        | JavaBase.Op (op, e1, e2) ->
              (build_semantics_expr Port.No e1 ii >>|
              build_semantics_expr Port.No e2 ii) >>= fun (v1, v2) ->
              M.op op v1 v2

        | JavaBase.Rmw (vh, (op, am), e) ->
              (read_reg Port.Addr vh ii) >>|
              (build_semantics_expr Port.Data e ii) >>=
              (fun (l , v) -> fetch_op op v am l ii)

        | JavaBase.CAS (vh, (read_am, write_am), expect, dest) ->
              (read_reg Port.Addr vh ii) >>= fun loc_vh ->
              (build_semantics_expr Port.No expect ii) >>= fun v_expect ->
              (read_mem Port.Addr read_am loc_vh ii) >>*= fun v_vh ->
              M.altT
                ((M.neqT v_vh v_expect) >>! v_vh)
                ((build_semantics_expr Port.Data dest ii) >>= fun v_dest ->
                    (M.mk_singleton_es
                    (Act.RMW (A.Location_global loc_vh,
                      v_expect, v_dest,
                            (match read_am , write_am with
                              | AccessModes.Acquire, AccessModes.Plain -> read_am
                              | AccessModes.Plain, AccessModes.Release -> write_am
                              | _ -> write_am), nat_sz))
                    ii) >>! v_expect)

      let build_cond e ii =
        let open Op in
        let e = match e with
        | JavaBase.Op (_,_,_) -> e
        | _ -> JavaBase.Op (Ne,e,Java.Const 0) in
        build_semantics_expr Port.No e ii

      let rec build_semantics test ii : (A.program_order_index * B.t) M.t =
        let ii =
            {ii with A.program_order_index = A.next_po_index ii.A.program_order_index} in

            match ii.A.inst with
            | JavaBase.StoreReg (reg, exp) -> (
                        (build_semantics_expr Port.No exp ii) >>=
                        (fun v -> write_reg reg v ii) >>=
                        (fun _ -> M.unitT (ii.A.program_order_index, B.Next [])))
            | JavaBase.StoreMem (vh, am, e) -> (
                    (read_reg Port.Addr vh ii) >>|
                    (build_semantics_expr Port.Data e ii) >>=
                    (fun (l, v) -> write_mem am l v ii) >>=
                    (fun _ -> M.unitT (ii.A.program_order_index, B.Next [])))
            | JavaBase.If (grd, thn, Some e) -> (
                    build_cond grd ii >>>> fun ret ->
                        let ii' = {
                            ii with A.program_order_index =
                            A.next_po_index ii.A.program_order_index;
                        } in
                        let then_branch = build_semantics test {ii' with A.inst = thn} in
                        let else_branch = build_semantics test {ii' with A.inst = e} in
                        M.choiceT ret then_branch else_branch
                )
            | JavaBase.If (grd, thn, None) -> (
                    build_cond grd ii >>>> fun ret ->
                        let ii' = {
                            ii with A.program_order_index =
                            A.next_po_index ii.A.program_order_index;
                        } in
                        let then_branch = build_semantics test {ii' with A.inst = thn} in
                        M.choiceT ret then_branch
                          (build_semantics_list test [] ii)
                )

            | JavaBase.Seq ins_lst ->
                    build_semantics_list test ins_lst ii
            | JavaBase.Fence mo -> M.mk_fence (Act.Fence mo) ii
                                    >>= fun _ -> M.unitT
                                  (ii.A.program_order_index, B.Next [])
            | _ -> assert false (* others are not implemented yet *)

    and build_semantics_list test insts ii =
        match insts with
        | [] -> M.unitT (ii.A.program_order_index, B.Next [])
        | hd :: tl ->
            let ii = {ii with A.inst = hd; } in
            (build_semantics test ii) >>> fun (prog_order, _branch) ->
                build_semantics_list test tl {ii with A.program_order_index = prog_order;}

    let spurious_setaf _ = assert false

    end
end
