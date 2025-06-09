open Edge

module C = struct
  open Cycle
end

module A = struct
  include AArch64_compile
end

let cas src dst ok =
 fun st dep ->
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
    | `Rs -> A.pseudo [A.do_eor rs src_reg src_reg] @ [A.mov rt 2], st
    | `Rt -> A.pseudo [A.do_eor rt src_reg src_reg; A.addi rt rt 2], st
    | `Rn ->
        let r_eor, st = A.next_reg st in
        let ins =
          A.pseudo
            [A.do_eor r_eor src_reg src_reg; A.do_add64 A.vloc rn rn r_eor]
          @ [A.mov rt 2]
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
          @ [A.mov rt 2]
        in
        ins, st
  in

  let ins = A.pseudo [A.cas A.RMW_P rs rt rn; A.do_ldr A.vloc rcheck rn] in

  let post_ins, st, dst_dep =
    match dst with
    | `Rs -> [], st, DepReg rs
    | `M ->
        let rn_reg, st = A.next_reg st in
        A.pseudo [A.do_ldr A.vloc rn_reg rn], st, DepReg rn_reg
  in

  pre_ins @ ins @ post_ins, dst_dep, st

let casRepr ok src dst =
  "cas:"
  ^ (if ok then "ok" else "no")
  ^ " "
  ^ (match src with `Rs -> "Rs" | `Rn -> "Rn" | `Rt -> "Rt" | `M -> "M")
  ^ "->"
  ^ match dst with `Rs -> "Rs" | `M -> "M"

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
          repr = casRepr ok src dst;
          compile_edge = cas src dst ok;
          direction = Rr, Wr;
          ie = Internal;
          sd = Same;
          significant_source = false;
          significant_dest = false;
        })
    (cartesian3 [true; false] [`Rn; `Rs; `Rt; `M] [`Rs; `M])
