module ASLConstant = SymbConstant.Make (ASLScalar) (PteVal.No) (ASLBase.Instr)
module ASLScalar = ASLConstant.Scalar
module ASLPteVal = ASLConstant.PteVal
module ASLInstr = ASLConstant.Instr

let list_set =
  let rec list_set acc n elt = function
    | [] -> None
    | _ :: t when n == 0 -> Some (List.rev acc @ (elt :: t))
    | h :: t -> list_set (h :: acc) (n - 1) elt t
  in
  list_set []

module ASLArchOp = struct
  type scalar = ASLScalar.t
  type pteval = ASLPteVal.t
  type instr = ASLInstr.t
  type cst = ASLConstant.v
  type op1 = Set of int * cst | Get of int

  let pp_op1 hexa = function
    | Set (i, v) -> Printf.sprintf "Set(%d, %s)" i (ASLConstant.pp hexa v)
    | Get i -> Printf.sprintf "Get(%d)" i

  let do_op1 op = function
    | Constant.ConcreteVector li -> (
        match op with
        | Get i -> List.nth_opt li i
        | Set (i, elt) -> (
            match list_set i elt li with
            | Some li' -> Some (Constant.ConcreteVector li')
            | None -> None))
    | _ -> None

  let shift_address_right _ _ = None
  let orop _ _ = None
  let andnot2 _ _ = None
  let andop _ _ = None
  let mask _ _ = None
end

module V = SymbValue.Make (ASLConstant) (ASLArchOp)
