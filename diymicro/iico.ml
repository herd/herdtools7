open Edge

module C = struct
  open Cycle
end

module A = struct
  include AArch64_compile
end

let casRsRs ok st dep =
  let src =
    match dep with
    | DepReg r -> r
    | _ -> Warn.fatal "Event has not forwarded any register"
  in
  let rs, st = A.next_reg st in
  let rt, st = A.next_reg st in
  let loc, rn, st = A.assigned_next_loc st in
  let rcheck, st = A.next_reg st in

  let st = A.set_initial st loc (if ok then 0 else 1) in
  let st = A.add_condition st rcheck (if ok then 2 else 1) in

  let ins =
    A.pseudo [A.do_eor rs src src]
    @ [A.mov rt 2]
    @ A.pseudo [A.cas A.RMW_P rs rt rn; A.do_ldr A.vloc rcheck rn]
  in

  ins, DepReg rs, st

let casRsRt ok st dep =
  let src =
    match dep with
    | DepReg r -> r
    | _ -> Warn.fatal "Event has not forwarded any register"
  in
  let rs, st = A.next_reg st in
  let rt, st = A.next_reg st in
  let loc, rn, st = A.assigned_next_loc st in
  let rcheck, st = A.next_reg st in

  let st = A.set_initial st loc (if ok then 0 else 1) in
  let st = A.add_condition st rcheck (if ok then 2 else 1) in

  let ins =
    A.pseudo [A.do_eor rs src src]
    @ [A.mov rt 2]
    @ A.pseudo [A.cas A.RMW_P rs rt rn; A.do_ldr A.vloc rcheck rn]
  in

  ins, DepReg rt, st

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
    };

  add_iico
    {
      repr = "cas:no Rs->Rt";
      compile_edge = casRsRt false;
      direction = Rr, Wr;
      ie = Internal;
      sd = Same;
      significant_source = false;
      significant_dest = false;
    };

  add_iico
    {
      repr = "cas:ok Rs->Rt";
      compile_edge = casRsRt true;
      direction = Rr, Wr;
      ie = Internal;
      sd = Same;
      significant_source = false;
      significant_dest = false;
    }
