type 'event nodes =
  | Events of 'event list
  | Classes of 'event list list

type 'event node =
  | Event of 'event
  | Class of 'event list

type 'event nonempty =
  | Member of 'event node
  | Edge of 'event node * 'event node

type 'event witness =
  | Cycle of 'event nodes
  | Reflexive of 'event node
  | Nonempty of 'event nonempty

type 'event failed_assertion =
  | FailedYes of AST.do_test * 'event witness
  | FailedNo of AST.do_test

type 'event failure = {
  name : string option;
  source : TxtLoc.t;
  reason : AST.pos;
  assertion : 'event failed_assertion;
}

type 'event t = Valid | Invalid of 'event failure list

let is_valid = function
  | Valid -> true
  | Invalid _ -> false
