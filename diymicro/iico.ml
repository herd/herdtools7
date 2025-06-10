open Edge

module C = struct
  open Cycle
end

module A = struct
  include AArch64_compile
end

module Cas = struct
  let compileRnM ok =
   fun st dep ->
    let src_reg =
      match dep with
      | DepReg r -> r
      | _ -> Warn.fatal "Event has not forwarded any register"
    in
    let rs, st = A.next_reg st in
    let rt, st = A.next_reg st in
    let loc, rn, st = A.assigned_next_loc st in

    let r_eor, st = A.next_reg st in
    let m_addr, st = A.next_reg st in
    let rcheck, st = A.next_reg st in
    let dst_reg, st = A.next_reg st in

    (* For srcM, that allows to check that STR is before CAS *)
    let m_initial = if ok then 0 else 1 in
    let st = A.set_initial st loc m_initial in
    let st = A.add_condition st rcheck (if ok then 2 else 1) in

    let ins =
      A.pseudo
        [A.do_eor r_eor src_reg src_reg; A.do_add64 A.vloc m_addr rn r_eor]
      @ [A.mov rt 2]
      @ A.pseudo
          [
            A.cas A.RMW_P rs rt m_addr;
            A.do_ldr A.vloc rcheck rn;
            A.do_ldr A.vloc dst_reg rn;
          ]
    in
    ins, DepReg dst_reg, st

  let compile src dst ok =
    let cas_base st dep =
      let src_reg =
        match dep with
        | DepReg r -> r
        | _ -> Warn.fatal "Event has not forwarded any register"
      in
      let rs, st = A.next_reg st in
      let rt, st = A.next_reg st in
      let loc, rn, st = A.assigned_next_loc st in
      let rcheck, st = A.next_reg st in

      (* For srcM, that allows to check that STR is before CAS *)
      let m_initial = if ok = (src <> `M) then 0 else 1 in
      let st = A.set_initial st loc m_initial in
      let st = A.add_condition st rcheck (if ok then 2 else 1) in

      let pre_ins, st =
        match src with
        | `Rs -> A.pseudo [A.do_eor rs src_reg src_reg], st
        | `Rt -> A.pseudo [A.do_eor rt src_reg src_reg], st
        | `Rn ->
            let r_eor, st = A.next_reg st in
            let ins =
              A.pseudo
                [A.do_eor r_eor src_reg src_reg; A.do_add64 A.vloc rn rn r_eor]
            in
            ins, st
        | `M ->
            let m_value = if ok then 0 else 1 in
            let reg_str, st = A.next_reg st in
            let ins =
              A.pseudo
                [
                  A.do_eor reg_str src_reg src_reg;
                  A.addi reg_str reg_str m_value;
                  A.str reg_str rn;
                ]
            in
            ins, st
      in
      let ins =
        A.pseudo
          [A.addi rt rt 2; A.cas A.RMW_P rs rt rn; A.do_ldr A.vloc rcheck rn]
      in

      let post_ins, st, dst_dep =
        match dst with
        | `Rs -> [], st, DepReg rs
        | `M ->
            let rn_reg, st = A.next_reg st in
            A.pseudo [A.do_ldr A.vloc rn_reg rn], st, DepReg rn_reg
      in
      pre_ins @ ins @ post_ins, dst_dep, st
    in
    match src, dst with `Rn, `M -> compileRnM ok | _ -> cas_base

  let repr src dst ok =
    "cas:"
    ^ (if ok then "ok" else "no")
    ^ " "
    ^ (match src with `Rs -> "Rs" | `Rn -> "Rn" | `Rt -> "Rt" | `M -> "M")
    ^ "->"
    ^ match dst with `Rs -> "Rs" | `M -> "M"
end

module Csel = struct
  let compile src ok =
   fun st dep ->
    let src_reg =
      match dep with
      | DepReg r -> r
      | _ -> Warn.fatal "Event has not forwarded any register"
    in

    let comp, st = A.next_reg st in
    let rn, st = A.next_reg st in
    let rm, st = A.next_reg st in
    let rd, st = A.next_reg st in
    let rcheck, st = A.next_reg st in

    let compared_value = if ok then 1 else 0 in
    let st = A.add_condition st rcheck (if ok then 1 else 2) in

    let pre_ins, st =
      match src with
      | `cmp -> A.pseudo [A.do_eor comp src_reg src_reg], st
      | `Rn -> A.pseudo [A.do_eor rn src_reg src_reg], st
      | `Rm -> A.pseudo [A.do_eor rm src_reg src_reg], st
    in

    let ins =
      A.pseudo
        [
          A.addi comp comp 1;
          A.addi rn rn 1;
          A.addi rm rm 2;
          A.cmpi comp compared_value;
          A.do_csel A.vloc rd rn rm;
        ]
      @ [A.mov_reg rcheck rd]
    in

    pre_ins @ ins, DepReg rd, st

  let repr src ok =
    "csel:"
    ^ (if ok then "ok" else "no")
    ^ " "
    ^ match src with `cmp -> "cmp" | `Rn -> "Rn" | `Rm -> "Rm"
end

let cartesian2 l1 l2 = List.concat_map (fun x -> List.map (fun y -> x, y) l2) l1

let cartesian3 l1 l2 l3 =
  List.concat_map
    (fun x -> List.concat_map (fun y -> List.map (fun z -> x, y, z) l3) l2)
    l1

let init () =
  add_iico (* Example edge *)
    {
      repr = "pod:rr";
      compile_edge = (fun st _ -> [], DepNone, st);
      direction = Rm, Rm;
      ie = Internal;
      sd = Different;
      significant_source = false;
      significant_dest = false;
    };

  List.iter
    (fun (ok, src, dst) ->
      add_iico
        {
          repr = Cas.repr src dst ok;
          compile_edge = Cas.compile src dst ok;
          direction = Rr, Wr;
          ie = Internal;
          sd = Same;
          significant_source = false;
          significant_dest = false;
        })
    (cartesian3 [true; false] [`Rn; `Rs; `Rt; `M] [`Rs; `M]);

  List.iter
    (fun (ok, src) ->
      add_iico
        {
          repr = Csel.repr src ok;
          compile_edge = Csel.compile src ok;
          direction = Rr, Wr;
          ie = Internal;
          sd = Same;
          significant_source = false;
          significant_dest = false;
        })
    (cartesian2 [true; false] [`cmp; `Rn; `Rm])
