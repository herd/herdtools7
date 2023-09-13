module SemanticsRule = struct
  type t =
    | Lit
    | TypedExpr
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
    | ETuple
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
    | TypedExpr -> "TypedExpr"
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
    | ETuple -> "ETuple"
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
      TypedExpr;
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
      ETuple;
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

type semantics_rule = SemanticsRule.t

module SemanticsCmp : Set.OrderedType with type t = semantics_rule = struct
  type t = semantics_rule

  let compare = compare
end

module TypingRule = struct
  type t =
    | Lit
    | TypedExpr
    | ELocalVar
    | EGlobalVar
    | EUndefIdent
    | BinopAnd
    | BinopOr
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
    | EConcatEmpty
    | EConcat
    | ETuple
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
    | TypedExpr -> "TypedExpr"
    | ELocalVar -> "ELocalVar"
    | EGlobalVar -> "EGlobalVar"
    | BinopAnd -> "BinopAnd"
    | BinopOr -> "BinopOr"
    | Binop -> "Binop"
    | Unop -> "Unop"
    | ECond -> "ECond"
    | ESlice -> "ESlice"
    | ECall -> "ECall"
    | ERecord -> "ERecord"
    | EGetBitField -> "EGetBitField"
    | EGetBitFields -> "EGetBitFields"
    | EConcatEmpty -> "EConcatEmpty"
    | EConcat -> "EConcat"
    | ETuple -> "ETuple"
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
      TypedExpr;
      ELocalVar;
      EGlobalVar;
      BinopAnd;
      BinopOr;
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
      EConcatEmpty;
      EConcat;
      ETuple;
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

type typing_rule = TypingRule.t

module TypingCmp : Set.OrderedType with type t = typing_rule = struct
  type t = typing_rule

  let compare = compare
end

module SemanticsSet = Set.Make (SemanticsCmp)
module TypingSet = Set.Make (TypingCmp)
module SemanticsMap = Map.Make (SemanticsCmp)
module TypingsMap = Map.Make (TypingCmp)

module type SEMINSTR = sig
  val use : semantics_rule -> unit
  val use_with : 'a -> semantics_rule -> 'a
end

module type TYPINSTR = sig
  val use : typing_rule -> unit
  val use_with : 'a -> typing_rule -> 'a
end

module type SEMBUFFER = sig
  val push : semantics_rule -> unit
  val reset : unit -> unit
  val get : unit -> semantics_rule list
end

module type TYPBUFFER = sig
  val push : typing_rule -> unit
  val reset : unit -> unit
  val get : unit -> typing_rule list
end

module SemMake (Buffer : SEMBUFFER) : SEMINSTR = struct
  let use = Buffer.push

  let use_with (x : 'a) r : 'a =
    Buffer.push r;
    x
end

module TypMake (Buffer : TYPBUFFER) : TYPINSTR = struct
  let use = Buffer.push

  let use_with (x : 'a) r : 'a =
    Buffer.push r;
    x
end

module SemanticsNoBuffer : SEMBUFFER = struct
  let push = Fun.const ()
  let reset () = ()
  let get () = []
end

module TypingNoBuffer : TYPBUFFER = struct
  let push = Fun.const ()
  let reset () = ()
  let get () = []
end

module SemanticsSingleBuffer : SEMBUFFER = struct
  let _buffer : semantics_rule list ref = ref []
  let reset () = _buffer := []
  let get () = !_buffer
  let push r = _buffer := r :: !_buffer
end

module TypingSingleBuffer : TYPBUFFER = struct
  let _buffer : typing_rule list ref = ref []
  let reset () = _buffer := []
  let get () = !_buffer
  let push r = _buffer := r :: !_buffer
end

module SemanticsSingleSetBuffer : SEMBUFFER = struct
  let _semantics_buffer : (semantics_rule, unit) Hashtbl.t = Hashtbl.create SemanticsRule.all_nb
  let push r = Hashtbl.replace _semantics_buffer r ()
  let reset () = Hashtbl.clear _semantics_buffer
  let get () = Hashtbl.to_seq_keys _semantics_buffer |> List.of_seq
end

module TypingSingleSetBuffer : TYPBUFFER = struct
  let _typing_buffer : (typing_rule, unit) Hashtbl.t = Hashtbl.create TypingRule.all_nb
  let push r = Hashtbl.replace _typing_buffer r ()
  let reset () = Hashtbl.clear _typing_buffer
  let get () = Hashtbl.to_seq_keys _typing_buffer |> List.of_seq
end

module SemanticsNoInstr = SemMake (SemanticsNoBuffer)
module TypingNoInstr = TypMake (TypingNoBuffer)
module SemanticsSingleInstr = SemMake (SemanticsSingleBuffer)
module TypingSingleInstr = TypMake (TypingSingleBuffer)
module SemanticsSingleSetInstr = SemMake (SemanticsSingleSetBuffer)
module TypingSingleSetInstr = TypMake (TypingSingleSetBuffer)
