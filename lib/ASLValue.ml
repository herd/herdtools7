module ASLConstant = SymbConstant.Make (ASLScalar) (PteVal.No) (ASLBase.Instr)
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

  type op1 =
    | Set of int * cst
    | Get of int
    | BVSlice of int list
    | ToInt
    | ToBool

  let pp_op1 hexa = function
    | Set (i, v) -> Printf.sprintf "Set(%d, %s)" i (ASLConstant.pp hexa v)
    | Get i -> Printf.sprintf "Get(%d)" i
    | BVSlice positions ->
        Printf.sprintf "Slice(%s)" @@ String.concat ", "
        @@ List.map string_of_int positions
    | ToInt -> "ToInt"
    | ToBool -> "ToBool"

  let do_op1 =
    let ( let* ) = Option.bind in
    let return_concrete s = Some (Constant.Concrete s) in
    let as_concrete = function Constant.Concrete v -> Some v | _ -> None in
    let as_concrete_vector = function
      | Constant.ConcreteVector v -> Some v
      | _ -> None
    in
    let all_64_bits_positions = List.init 64 (( - ) 63) in
    fun op cst ->
      match op with
      | Get i ->
          let* vec = as_concrete_vector cst in
          List.nth_opt vec i
      | Set (i, elt) ->
          let* vec = as_concrete_vector cst in
          let* vec' = list_set i elt vec in
          Some (Constant.ConcreteVector vec')
      | ToInt -> (
          match cst with
          | Constant.Concrete s -> ASLScalar.convert_to_int s |> return_concrete
          | Constant.Symbolic _ -> Some cst
          | _ -> None)
      | ToBool ->
          let* s = as_concrete cst in
          return_concrete (ASLScalar.convert_to_bool s)
      | BVSlice positions -> (
          match cst with
          | Constant.Concrete s ->
              let* s' = ASLScalar.try_extract_slice s positions in
              return_concrete s'
          | Constant.Symbolic x ->
              if Misc.list_eq ( = ) positions all_64_bits_positions then
                Some (Constant.Symbolic x)
              else None
          | _ -> None)

  let shift_address_right _ _ = None
  let orop _ _ = None
  let andnot2 _ _ = None
  let andop _ _ = None
  let mask _ _ = None
end

module V = SymbValue.Make (ASLConstant) (ASLArchOp)
