module Rule = struct
  type t =
    | Lit
    | IgnoreTypedExpr
    | ELocalVar
    | EGlobalVar
    | EUndefIdent
    | Binop
    | Unop
    | ECondSimple
    | ECond
    | ESlice
    | ECall
    | EGetArray
    | ERecord
    | EGetBitField
    | EGetBitFields
    | EConcat
    | EUnknown 
    | EPattern
    | LEIgnore
    | LETyped
    | LELocalVar
    | LEGlobalVar
    | LEUndefIdentV0
    | LEUndefIdentV1
    | LESlice
    | LESetArray
    | LESetField
    | LESetFields
    | LETuple
    | PAll
    | PAny
    | PGeq
    | PLeq
    | PNot
    | PRange
    | PSingle
    | PMask
    | PTuple
    | LDIgnore
    | LDVar
    | LDTypedVar
    | LDUninitialisedVar
    | LDTuple
    | LDTypedTuple
    | LDUninitialisedTuple
    | SPass
    | SAssignCall
    | SAssignTuple
    | SAssign
    | SReturnOne
    | SReturnSome
    | SReturnNone
    | SThen
    | SCall
    | SCond
    | SCase
    | SAssert
    | SWhile
    | SRepeat
    | SFor
    | SThrowNone
    | SThrowSomeTyped
    | SThrowSome
    | STry
    | SDeclSome
    | SDeclNone
    | SDebug
    | FUndefIdent
    | FPrimitive
    | FBadArity
    | FCall
    | Block
    | Loop
    | For

  let to_string : t -> string = function
    | Lit -> "Lit"
    | IgnoreTypedExpr -> "IgnoreTypedExpr"
    | ELocalVar -> "ELocalVar"
    | EGlobalVar -> "EGlobalVar"
    | Binop -> "Binop"
    | Unop -> "Unop"
    | ECond -> "ECond"
    | ESlice -> "ESlice"
    | ECall -> "ECall"
    | ERecord -> "ERecord"
    | EGetBitField -> "EGetBitField"
    | EGetBitFields -> "EGetBitFields"
    | EConcat -> "EConcat"
    | EUndefIdent -> "EUndefIdent"
    | ECondSimple -> "ECondSimple"
    | EGetArray -> "EGetArray"
    | EUnknown -> "EUnknown"
    | EPattern -> "EPattern"
    | LEIgnore -> "LEIgnore"
    | LETyped -> "LETyped"
    | LELocalVar -> "LELocalVar"
    | LEGlobalVar -> "LEGlobalVar"
    | LESlice -> "LESlice"
    | LESetArray -> "LESetArray"
    | LESetField -> "LESetField"
    | LESetFields -> "LESetFields"
    | LETuple -> "LETuple"
    | LEUndefIdentV0 -> "LEUndefIdentV0"
    | LEUndefIdentV1 -> "LEUndefIdentV1"
    | PAll -> "PAll"
    | PAny -> "PAny"
    | PGeq -> "PGeq"
    | PLeq -> "PLeq"
    | PNot -> "PNot"
    | PRange -> "PRange"
    | PSingle -> "PSingle"
    | PMask -> "PMask"
    | PTuple -> "PTuple"
    | LDIgnore -> "LDIgnore"
    | LDVar -> "LDVar"
    | LDTypedVar -> "LDTypedVar"
    | LDUninitialisedVar -> "LDUninitialisedVar"
    | LDTuple -> "LDTuple"
    | LDTypedTuple -> "LDTypedTuple"
    | LDUninitialisedTuple -> "LDUninitialisedTuple"
    | SPass -> "SPass"
    | SAssignCall -> "SAssignCall"
    | SAssignTuple -> "SAssignTuple"
    | SAssign -> "SAssign"
    | SReturnOne -> "SReturnOne"
    | SReturnNone -> "SReturnNone"
    | SReturnSome -> "SReturnSome"
    | SThen -> "SThen"
    | SCall -> "SCall"
    | SCond -> "SCond"
    | SCase -> "SCase"
    | SAssert -> "SAssert"
    | SWhile -> "SWhile"
    | SRepeat -> "SRepeat"
    | SFor -> "SFor"
    | SThrowNone -> "SThrowNone"
    | SThrowSomeTyped -> "SThrowSomeTyped"
    | SThrowSome -> "SThrowSome"
    | STry -> "STry"
    | SDeclSome -> "SDeclSome"
    | SDeclNone -> "SDeclNone"
    | SDebug -> "SDebug" 
    | FUndefIdent -> "FUndefIdent"
    | FPrimitive -> "FPrimitive"
    | FBadArity -> "FBadArity"
    | FCall -> "FCall"
    | Block -> "Block"
    | Loop -> "Loop"
    | For -> "For"

  let pp f r = to_string r |> Format.pp_print_string f

  let all =
    [
      Lit;
      IgnoreTypedExpr;
      ELocalVar;
      EGlobalVar;
      Binop;
      Unop;
      ECond;
      ESlice;
      ECall;
      ERecord;
      EGetBitField;
      EGetBitFields;
      EUnknown;
      EPattern;
      EGetArray;
      ECondSimple;
      EUndefIdent;
      EConcat;
      LEIgnore;
      LETyped;
      LELocalVar;
      LEGlobalVar;
      LESlice;
      LESetArray;
      LESetField;
      LESetFields;
      LETuple;
      SPass;
      SAssignCall;
      SAssignTuple;
      SAssign;
      SReturnOne;
      SReturnSome;
      SReturnNone;
      SThen;
      SCall;
      SCond;
      SCase;
      SAssert;
      SWhile;
      SRepeat;
      SFor;
      SThrowNone;
      SThrowSomeTyped;
      SThrowSome;
      STry;
      SDeclSome;
      SDeclNone;
      SDebug;
      FUndefIdent;
      FPrimitive;
      FBadArity;
      FCall;
      Block;
      Loop;
      For;
    ]

  let all_nb = List.length all

  let index =
    let tbl : (t, int) Hashtbl.t = Hashtbl.create all_nb in
    let () = List.iteri (fun i r -> Hashtbl.add tbl r i) all in
    Hashtbl.find tbl

  let of_string =
    let tbl : (string, t) Hashtbl.t = Hashtbl.create all_nb in
    let () =
      List.iter
        (fun r -> Hashtbl.add tbl (to_string r |> String.lowercase_ascii) r)
        all
    in
    fun s -> Hashtbl.find tbl (String.lowercase_ascii s)
end

type rule = Rule.t

module Cmp : Set.OrderedType with type t = rule = struct
  type t = rule

  let compare = compare
end

module Set = Set.Make (Cmp)
module Map = Map.Make (Cmp)

module type INSTR = sig
  val use : rule -> unit
  val use_with : 'a -> rule -> 'a
end

module type BUFFER = sig
  val push : rule -> unit
  val reset : unit -> unit
  val get : unit -> rule list
end

module Make (Buffer : BUFFER) : INSTR = struct
  let use = Buffer.push

  let use_with (x : 'a) r : 'a =
    Buffer.push r;
    x
end

module NoBuffer : BUFFER = struct
  let push = Fun.const ()
  let reset () = ()
  let get () = []
end

module SingleBuffer : BUFFER = struct
  let _buffer : rule list ref = ref []
  let reset () = _buffer := []
  let get () = !_buffer
  let push r = _buffer := r :: !_buffer
end

module SingleSetBuffer : BUFFER = struct
  let _buffer : (rule, unit) Hashtbl.t = Hashtbl.create Rule.all_nb
  let reset () = Hashtbl.clear _buffer
  let get () = Hashtbl.to_seq_keys _buffer |> List.of_seq
  let push r = Hashtbl.replace _buffer r ()
end

module NoInstr = Make (NoBuffer)
module SingleInstr = Make (SingleBuffer)
module SingleSetInstr = Make (SingleSetBuffer)
