module V = ASLValue.V

module Config = struct
  let verbose = 0
  let texmacros = false
  let hexa = false
  let brackets = false
  let variant _ = false
  let endian = None
  let default_to_symb = true
  let debug = Herd_core.Debug_herd.none
  let keep_failed_as_undetermined = true
  let old_solver = false
end

module A = struct
  include Herd_core.ASLExtra.Make (ASLBase) (Config) (V)
  let opt_env = true
end

module VC = Herd_core.Valconstraint.Make (Config) (A)
module Decisions = Herd_core.ASLBranchDecisions.Make (VC)

let () =
  let decisions = Decisions.create () in
  let source = V.fresh_var () in
  let comparison ?(reverse = false) source positions literal =
    let slice = V.fresh_var () in
    Decisions.record decisions
      [VC.Assign (slice, VC.Unop (Op.ArchOp1 (ASLOp.BVSlice positions), source))];
    let literal = V.scalarToV (ASLScalar.bv_of_string literal) in
    let left, right = if reverse then literal, slice else slice, literal in
    let result = V.fresh_var () in
    Decisions.record decisions [VC.Assign (result, VC.Binop (Op.Eq, left, right))];
    result
  in
  let original = comparison source [0] "1" in
  let repeated = comparison source [0] "1" in
  let reversed = comparison ~reverse:true source [0] "1" in
  let unrelated =
    [ comparison source [1] "1";
      comparison source [0] "0";
      comparison source [0] "01";
      comparison (V.fresh_var ()) [0] "1";
      V.fresh_var () ]
  in
  List.iter
    (fun decision ->
      let assumptions = [original, decision] in
      List.iter
        (fun condition ->
          assert (Decisions.find decisions ~assumptions condition = Some decision))
        [original; repeated; reversed];
      List.iter
        (fun condition ->
          assert (Decisions.find decisions ~assumptions condition = None))
        unrelated)
    [true; false];
  (* Neither another path nor another interpreter inherits this decision. *)
  assert (Decisions.find decisions ~assumptions:[] repeated = None);
  assert
    (Decisions.find (Decisions.create ())
       ~assumptions:[original, true] repeated = None)


let () =
  let decisions = Decisions.create () in
  let source = V.fresh_var () in
  let emit expression =
    let result = V.fresh_var () in
    Decisions.record decisions [VC.Assign (result, expression)];
    result
  in
  let below_limit ?(conversion = ASLOp.ToIntU) ?(offset = V.intToV 1) source =
    let value = emit (VC.Unop (Op.ArchOp1 conversion, source)) in
    let value = emit (VC.Binop (Op.Add, value, offset)) in
    emit (VC.Binop (Op.Lt, value, V.intToV 8))
  in
  let original = below_limit source in
  let repeated = below_limit source in
  List.iter
    (fun decision ->
      let assumptions = [original, decision] in
      assert (Decisions.find decisions ~assumptions repeated = Some decision);
      List.iter
        (fun condition -> assert (Decisions.find decisions ~assumptions condition = None))
        [ below_limit (V.fresh_var ());
          below_limit ~conversion:ASLOp.ToIntS source;
          below_limit ~offset:(V.intToV 2) source;
          below_limit ~offset:(V.scalarToV (ASLScalar.bv_of_string "1")) source ])
    [true; false];
  (* Branch assignments must neither replace definitions nor equate inputs. *)
  Decisions.record decisions [VC.Assign (original, VC.Atom V.v_true)];
  assert (Decisions.find decisions ~assumptions:[original, false] repeated = Some false);
  let a = V.fresh_var () and b = V.fresh_var () in
  Decisions.record decisions
    [VC.Assign (a, VC.Atom V.v_true); VC.Assign (b, VC.Atom V.v_true)];
  assert (Decisions.find decisions ~assumptions:[a, true] b = None);
  (* Ternary operators and their operand order also participate in the key. *)
  let select x y = emit (VC.Terop (Op.If, source, x, y)) in
  let first = select a b and second = select a b and swapped = select b a in
  assert (Decisions.find decisions ~assumptions:[first, true] second = Some true);
  assert (Decisions.find decisions ~assumptions:[first, true] swapped = None);
  (* Conversion widths are part of the operator, even for the same source. *)
  let convert width = emit (VC.Unop (Op.ArchOp1 (ASLOp.ToBV width), source)) in
  let narrow = convert 32 and wide = convert 64 in
  let equal x = emit (VC.Binop (Op.Eq, x, source)) in
  let narrow = equal narrow and wide = equal wide in
  assert (Decisions.find decisions ~assumptions:[narrow, true] wide = None);
  (* Cyclic or conflicting definitions remain opaque. *)
  let x = V.fresh_var () and y = V.fresh_var () in
  Decisions.record decisions
    [VC.Assign (x, VC.Unop (Op.ArchOp1 ASLOp.ToIntU, x));
     VC.Assign (y, VC.Unop (Op.ArchOp1 ASLOp.ToIntU, y))];
  assert (Decisions.find decisions ~assumptions:[x, true] y = None);
  Decisions.record decisions [VC.Assign (repeated, VC.Unop (Op.ArchOp1 ASLOp.ToIntS, source))];
  assert (Decisions.find decisions ~assumptions:[original, true] repeated = None);
  (* Shared expression DAGs must not require exponential recursive expansion. *)
  let rec shared depth value =
    if depth = 0 then value
    else shared (depth - 1) (emit (VC.Binop (Op.Add, value, value)))
  in
  let first = shared 30 source and second = shared 30 source in
  assert (Decisions.find decisions ~assumptions:[first, true] second = Some true)
