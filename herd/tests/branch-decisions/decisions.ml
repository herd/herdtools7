module Decisions = Herd_core.ASLBranchDecisions
module V = ASLValue.V

let () =
  let decisions = Decisions.create () in
  let source = V.fresh_var () in
  let comparison ?(reverse = false) source positions literal =
    let slice = V.fresh_var () in
    Decisions.record_slice decisions ~result:slice ~source ~positions;
    let literal = V.scalarToV (ASLScalar.bv_of_string literal) in
    let left, right = if reverse then literal, slice else slice, literal in
    let result = V.fresh_var () in
    Decisions.record_equal decisions ~result ~left ~right;
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
