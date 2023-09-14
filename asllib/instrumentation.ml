(** Provide some instrumentation backends for {!Interpreter} and {!Typing}. *)

module SemanticsRule = struct
  type t =
    | Lit
    | TypedExpr
    | ELocalVar
    | EGlobalVar
    | EUndefIdent
    | Binop
    | BinopAnd
    | BinopOr
    | BinopImpl
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
    | LDUninitialisedTypedTuple
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
    | Catch
    | CatchNamed
    | CatchOtherwise
    | CatchNone
    | CatchNoThrow

  let to_string : t -> string = function
    | Lit -> "Lit"
    | TypedExpr -> "TypedExpr"
    | ELocalVar -> "ELocalVar"
    | EGlobalVar -> "EGlobalVar"
    | Binop -> "Binop"
    | BinopAnd -> "BinopAnd"
    | BinopOr -> "BinopOr"
    | BinopImpl -> "BinopImpl"
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
    | LDUninitialisedTypedTuple -> "LDUninitialisedTypedTuple"
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
    | Catch -> "Catch"
    | CatchNamed -> "CatchNamed"
    | CatchOtherwise -> "CatchOtherwise"
    | CatchNone -> "CatchNone"
    | CatchNoThrow -> "CatchNoThrow"

  let pp f r = to_string r |> Format.pp_print_string f

  let all =
    [
      Lit;
      TypedExpr;
      ELocalVar;
      EGlobalVar;
      Binop;
      BinopAnd;
      BinopOr;
      BinopImpl;
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
    | ELocalVarConstant
    | ELocalVar
    | EGlobalVarConstantVal
    | EGlobalVarConstantNoVal
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
    | ERecordMissingField
    | ERecordNotARecord
    | EGetRecordField
    | EGetBitField
    | EGetBadBitField
    | EGetBitFieldNested
    | EGetBitFieldTyped
    | EGetBitFields
    | EConcatEmpty
    | EConcat
    | ETuple
    | EUnknown
    | EPattern
    | LEIgnore
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
    | LDIgnoreNone
    | LDIgnoreSome
    | LDUninitialisedVar
    | LDUninitialisedTypedVar
    | LDVar
    | LDTypedVar
    | LDUninitialisedTuple
    | LDUninitialisedTypedTuple
    | LDTuple
    | LDTypedTuple
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
    | SliceLength
    | SliceSingle
    | SliceRange
    | SliceStar
    | Catcher
    | Func

  let to_string : t -> string = function
    | Lit -> "Lit"
    | TypedExpr -> "TypedExpr"
    | ELocalVarConstant -> "ELocalVarConstant"
    | ELocalVar -> "ELocalVar"
    | EGlobalVarConstantVal -> "EGlobalVarConstantVal"
    | EGlobalVarConstantNoVal -> "EGlobalVarConstantNoVal"
    | EGlobalVar -> "EGlobalVar"
    | Binop -> "Binop"
    | Unop -> "Unop"
    | ECond -> "ECond"
    | ESlice -> "ESlice"
    | ECall -> "ECall"
    | ERecord -> "ERecord"
    | ERecordMissingField -> "ERecordMissingField"
    | ERecordNotARecord -> "ERecordNotARecord"
    | EGetRecordField -> "EGetRecordField"
    | EGetBitField -> "EGetBitField"
    | EGetBadBitField -> "EGetBadBitField"
    | EGetBitFieldNested -> "EGetBitFieldNested"
    | EGetBitFieldTyped -> "EGetBitFieldTyped"
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
    | LDIgnoreNone -> "LDIgnoreNone"
    | LDIgnoreSome -> "LDIgnoreSome"
    | LDVar -> "LDVar"
    | LDTypedVar -> "LDTypedVar"
    | LDUninitialisedVar -> "LDUninitialisedVar"
    | LDUninitialisedTypedVar -> "LDUninitialisedTypedVar"
    | LDTuple -> "LDTuple"
    | LDTypedTuple -> "LDTypedTuple"
    | LDUninitialisedTuple -> "LDUninitialisedTuple"
    | LDUninitialisedTypedTuple -> "LDUninitialisedTypedTuple"
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
    | SliceLength -> "SliceLength"
    | SliceSingle -> "SliceSingle"
    | SliceRange -> "SliceRange"
    | SliceStar -> "SliceStar"
    | Catcher -> "Catcher"
    | Func -> "Func"

  let pp f r = to_string r |> Format.pp_print_string f

  let all =
    [
      Lit;
      TypedExpr;
      ELocalVarConstant;
      ELocalVar;
      EGlobalVarConstantVal;
      EGlobalVarConstantNoVal;
      EGlobalVar;
      Binop;
      Unop;
      ECond;
      ESlice;
      ECall;
      ERecordMissingField;
      ERecordNotARecord;
      ERecord;
      EGetRecordField;
      EGetBadBitField;
      EGetBitField;
      EGetBitFieldNested;
      EGetBitFieldTyped;
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
      SliceLength;
      SliceSingle;
      SliceRange;
      SliceStar;
      Catcher;
      Func;
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
  let _semantics_buffer : (semantics_rule, unit) Hashtbl.t =
    Hashtbl.create SemanticsRule.all_nb

  let push r = Hashtbl.replace _semantics_buffer r ()
  let reset () = Hashtbl.clear _semantics_buffer
  let get () = Hashtbl.to_seq_keys _semantics_buffer |> List.of_seq
end

module TypingSingleSetBuffer : TYPBUFFER = struct
  let _typing_buffer : (typing_rule, unit) Hashtbl.t =
    Hashtbl.create TypingRule.all_nb

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
