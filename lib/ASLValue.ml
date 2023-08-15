module ASLConstant = SymbConstant.Make (ASLScalar) (PteVal.No) (ASLBase.Instr)
module ASLPteVal = ASLConstant.PteVal
module ASLInstr = ASLConstant.Instr

type asl_op =
  | SetIndex of int
  | SetField of string
  | Concat
  | BVSliceSet of int list

type asl_op1 =
  | GetIndex of int
  | GetField of string
  | BVSlice of int list
  | ToIntU
  | ToIntS
  | ToBool
  | ToBV
  | BoolNot
(*
   A note on ASL ArchOps
   ---------------------

   The following implementation of operations on values don't have any effects
   on symbolics values. This is to mimic herd's semantics. This matches the
   implementation of translation from ASL equations to AArch64 equations where
   those operations are dropped.
 *)

module ASLArchOp :
  ArchOp.S
    with type op = asl_op
     and type op1 = asl_op1
     and type scalar = ASLScalar.t
     and type pteval = ASLPteVal.t
     and type instr = ASLInstr.t
     and type cst = ASLConstant.v = struct
  type scalar = ASLScalar.t
  type pteval = ASLPteVal.t
  type instr = ASLInstr.t
  type cst = ASLConstant.v
  type op = asl_op
  type op1 = asl_op1

  let pp_op = function
    | SetIndex i -> Printf.sprintf "Set[%d]" i
    | SetField x -> Printf.sprintf "Set[%S]" x
    | Concat -> "Concat"
    | BVSliceSet positions ->
        Printf.sprintf "SliceSet[%s]"
        @@ String.concat ", "
        @@ List.map string_of_int positions

  let pp_op1 _hexa = function
    | GetIndex i -> Printf.sprintf "Get[%d]" i
    | GetField x -> Printf.sprintf "Get[%S]" x
    | BVSlice positions ->
        Printf.sprintf "Slice[%s]" @@ String.concat ", "
        @@ List.map string_of_int positions
    | ToIntU -> "ToIntU"
    | ToIntS -> "ToIntS"
    | ToBool -> "ToBool"
    | ToBV -> "ToBV"
    | BoolNot -> "BoolNot"

  let ( let* ) = Option.bind
  let return_concrete s = Some (Constant.Concrete s)
  let as_concrete = function Constant.Concrete v -> Some v | _ -> None

  let as_concrete_vector = function
    | Constant.ConcreteVector v -> Some v
    | _ -> None

  let as_concrete_record = function
    | Constant.ConcreteRecord v -> Some v
    | _ -> None

  let all_64_bits_positions = List.init 64 (( - ) 63)

  let list_set =
    let rec list_set acc n elt = function
      | [] -> None
      | _ :: t when n == 0 -> Some (List.rev acc @ (elt :: t))
      | h :: t -> list_set (h :: acc) (n - 1) elt t
    in
    list_set []

  let do_op op c1 c2 =
    match op with
    | SetIndex i ->
        let* vec = as_concrete_vector c1 in
        let* vec' = list_set i c2 vec in
        Some (Constant.ConcreteVector vec')
    | SetField x ->
        let* record = as_concrete_record c1 in
        if StringMap.mem x record then
          let record' = StringMap.add x c2 record in
          Some (Constant.ConcreteRecord record')
        else None
    | Concat ->
        let* s1 = as_concrete c1 in
        let* s2 = as_concrete c2 in
        let* s = ASLScalar.try_concat s1 s2 in
        return_concrete s
    | BVSliceSet positions ->
        let* s1 = as_concrete c1 in
        let* s2 = as_concrete c2 in
        let* s = ASLScalar.try_write_slice positions s1 s2 in
        return_concrete s

  let do_op1 op cst =
    match op with
    | GetIndex i ->
        let* vec = as_concrete_vector cst in
        List.nth_opt vec i
    | GetField x ->
        let* record = as_concrete_record cst in
        StringMap.find_opt x record
    | ToIntS -> (
        match cst with
        | Constant.Concrete s ->
            ASLScalar.convert_to_int_signed s |> return_concrete
        | Constant.Symbolic _ -> Some cst
        | _ -> None)
    | ToIntU -> (
        match cst with
        | Constant.Concrete s ->
            ASLScalar.convert_to_int_unsigned s |> return_concrete
        | Constant.Symbolic _ -> Some cst
        | _ -> None)
    | ToBV -> (
        match cst with
        | Constant.Concrete s -> ASLScalar.convert_to_bv s |> return_concrete
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
    | BoolNot -> (
        let open Constant in
        let open ASLScalar in
        match cst with
        | Concrete (S_Bool b) -> return_concrete (S_Bool (not b))
        | _ -> None)

  let shift_address_right _ _ = None
  let orop _ _ = None
  let andnot2 _ _ = None
  let andop _ _ = None
  let mask _ _ = None
end

module V = SymbValue.Make (ASLConstant) (ASLArchOp)
