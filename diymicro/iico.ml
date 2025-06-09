open Edge

module C = struct
  open Cycle
end

module A = struct
  include AArch64_compile
end

let casRsRs ok st dep =
  let dst_value = if ok then 0 else 1 in
  let src =
    match dep with
    | DepReg r -> r
    | _ -> Warn.fatal "Event has not forwarded any register"
  in
  let pre_ins, cmp, st =
    if ok then
      let cmp, st = A.next_reg st in
      [A.mov cmp dst_value], cmp, st
    else [], A.ZR, st
  in
  let dst, st = A.next_reg st in
  let _, reg_loc, st = A.assigned_next_loc st in
  let value_check, st = A.next_reg st in
  let ins =
    pre_ins
    @ A.pseudo [A.do_eor dst src src; A.cas A.RMW_P dst cmp reg_loc]
    @ [A.mov_reg value_check dst]
  in
  let st = A.add_condition st value_check dst_value in
  ins, DepReg dst, st

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

  add_iico
    {
      repr = "cas:no Rs->Rs";
      compile_edge = casRsRs false;
      direction = Rr, Wr;
      ie = Internal;
      sd = Same;
      significant_source = false;
      significant_dest = false;
    };

  add_iico
    {
      repr = "cas:ok Rs->Rs";
      compile_edge = casRsRs true;
      direction = Rr, Wr;
      ie = Internal;
      sd = Same;
      significant_source = false;
      significant_dest = false;
    }
