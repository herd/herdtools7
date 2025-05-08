(******************************************************************************)
(*                                ASLRef                                      *)
(******************************************************************************)
(*
 * SPDX-FileCopyrightText: Copyright 2022-2023 Arm Limited and/or its affiliates <open-source-office@arm.com>
 * SPDX-License-Identifier: BSD-3-Clause
 *)
(******************************************************************************)
(* Disclaimer:                                                                *)
(* This material covers both ASLv0 (viz, the existing ASL pseudocode language *)
(* which appears in the Arm Architecture Reference Manual) and ASLv1, a new,  *)
(* experimental, and as yet unreleased version of ASL.                        *)
(* This material is work in progress, more precisely at pre-Alpha quality as  *)
(* per Arm’s quality standards.                                               *)
(* In particular, this means that it would be premature to base any           *)
(* production tool development on this material.                              *)
(* However, any feedback, question, query and feature request would be most   *)
(* welcome; those can be sent to Arm’s Architecture Formal Team Lead          *)
(* Jade Alglave <jade.alglave@arm.com>, or by raising issues or PRs to the    *)
(* herdtools7 github repository.                                              *)
(******************************************************************************)

(** Provide some instrumentation backends for {!Interpreter} and {!Typing}. *)

module SemanticsRule = struct
  type t =
    | ELit
    | Call
    | ATC
    | EExprList
    | EExprListM
    | ESideEffectFreeExpr
    | EVar
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
    | EGetEnumArray
    | ESliceError
    | ERecord
    | EGetBitField
    | EGetBitFields
    | EGetTupleItem
    | EConcat
    | ETuple
    | EArray
    | EEnumArray
    | EArbitrary
    | EPattern
    | LEDiscard
    | LEVar
    | LEMultiAssign
    | LEUndefIdentV0
    | LEUndefIdentV1
    | LESlice
    | LESetArray
    | LESetEnumArray
    | LESetField
    | LESetFields
    | LEDestructuring
    | Slices
    | Slice
    | PAll
    | PAny
    | PGeq
    | PLeq
    | PNot
    | PRange
    | PSingle
    | PMask
    | PTuple
    | LDDiscard
    | LDVar
    | LDTuple
    | SPass
    | SAssignCall
    | SAssign
    | SReturn
    | SSeq
    | SCall
    | SCond
    | SCase
    | SAssert
    | SWhile
    | SRepeat
    | SFor
    | SThrow
    | STry
    | SDecl
    | SPrint
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
    | Spec
    | FindCatcher
    | RethrowImplicit
    | ReadValueFrom
    | BuildGlobalEnv
    | IsConstraintSat
    | AssignArgs

  let to_string : t -> string = function
    | ELit -> "ELit"
    | Call -> "Call"
    | ATC -> "CTC"
    | EExprList -> "EExprList"
    | EExprListM -> "EExprListM"
    | ESideEffectFreeExpr -> "ESideEffectFreeExpr"
    | EVar -> "EVar"
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
    | EGetTupleItem -> "EGetTupleItem"
    | EConcat -> "EConcat"
    | ETuple -> "ETuple"
    | EArray -> "EArray"
    | EEnumArray -> "EEnumArray"
    | ECondSimple -> "ECondSimple"
    | EGetArray -> "EGetArray"
    | EGetEnumArray -> "EGetEnumArray"
    | ESliceError -> "ESliceError"
    | EArbitrary -> "EArbitrary"
    | EPattern -> "EPattern"
    | LEDiscard -> "LEDiscard"
    | LEVar -> "LEVar"
    | LEMultiAssign -> "LEMultiAssign"
    | LESlice -> "LESlice"
    | LESetArray -> "LESetArray"
    | LESetEnumArray -> "LESetEnumArray"
    | LESetField -> "LESetField"
    | LESetFields -> "LESetFields"
    | LEDestructuring -> "LEDestructuring"
    | LEUndefIdentV0 -> "LEUndefIdentV0"
    | LEUndefIdentV1 -> "LEUndefIdentV1"
    | Slices -> "Slices"
    | Slice -> "Slice"
    | PAll -> "PAll"
    | PAny -> "PAny"
    | PGeq -> "PGeq"
    | PLeq -> "PLeq"
    | PNot -> "PNot"
    | PRange -> "PRange"
    | PSingle -> "PSingle"
    | PMask -> "PMask"
    | PTuple -> "PTuple"
    | LDDiscard -> "LDDiscard"
    | LDVar -> "LDVar"
    | LDTuple -> "LDTuple"
    | SPass -> "SPass"
    | SAssignCall -> "SAssignCall"
    | SAssign -> "SAssign"
    | SReturn -> "SReturn"
    | SSeq -> "SThen"
    | SCall -> "SCall"
    | SCond -> "SCond"
    | SCase -> "SCase"
    | SAssert -> "SAssert"
    | SWhile -> "SWhile"
    | SRepeat -> "SRepeat"
    | SFor -> "SFor"
    | SThrow -> "SThrow"
    | STry -> "STry"
    | SDecl -> "SDecl"
    | SPrint -> "SPrint"
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
    | Spec -> "Spec"
    | FindCatcher -> "FindCatcher"
    | RethrowImplicit -> "RethrowImplicit"
    | ReadValueFrom -> "ReadValueFrom"
    | BuildGlobalEnv -> "BuildGlobalEnv"
    | IsConstraintSat -> "IsConstraintSat"
    | AssignArgs -> "AssignArgs"

  let pp f r = to_string r |> Format.pp_print_string f

  let all =
    [
      ELit;
      Call;
      ATC;
      EExprList;
      EExprListM;
      ESideEffectFreeExpr;
      EVar;
      Binop;
      BinopAnd;
      BinopOr;
      BinopImpl;
      Unop;
      ECondSimple;
      ECond;
      ESlice;
      ECall;
      EGetArray;
      EGetEnumArray;
      ESliceError;
      ERecord;
      EGetBitField;
      EGetBitFields;
      EGetTupleItem;
      EConcat;
      ETuple;
      EArray;
      EEnumArray;
      EArbitrary;
      EPattern;
      LEDiscard;
      LEVar;
      LEMultiAssign;
      LEUndefIdentV0;
      LEUndefIdentV1;
      LESlice;
      LESetArray;
      LESetEnumArray;
      LESetField;
      LESetFields;
      LEDestructuring;
      Slices;
      Slice;
      PAll;
      PAny;
      PGeq;
      PLeq;
      PNot;
      PRange;
      PSingle;
      PMask;
      PTuple;
      LDDiscard;
      LDVar;
      LDTuple;
      SPass;
      SAssignCall;
      SAssign;
      SReturn;
      SSeq;
      SCall;
      SCond;
      SCase;
      SAssert;
      SWhile;
      SRepeat;
      SFor;
      SThrow;
      STry;
      SDecl;
      SPrint;
      FUndefIdent;
      FPrimitive;
      FBadArity;
      FCall;
      Block;
      Loop;
      For;
      Catch;
      CatchNamed;
      CatchOtherwise;
      CatchNone;
      CatchNoThrow;
      Spec;
      FindCatcher;
      RethrowImplicit;
      ReadValueFrom;
      BuildGlobalEnv;
      IsConstraintSat;
      AssignArgs;
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
    | BuiltinSingularType
    | BuiltinAggregateType
    | BuiltinSingularOrAggregate
    | NamedType
    | AnonymousType
    | SingularType
    | AggregateType
    | NonPrimitiveType
    | PrimitiveType
    | Structure
    | Canonical
    | Domain
    | Subtype
    | StructuralSubtypeSatisfaction
    | DomainSubtypeSatisfaction
    | SubtypeSatisfaction
    | TypeSatisfaction
    | TypeClash
    | LowestCommonAncestor
    | ApplyUnopType
    | ApplyBinopTypes
    | ELit
    | ATC
    | EVar
    | Binop
    | Unop
    | ECondSimple
    | ECond
    | ESlice
    | ECall
    | ESetter
    | EGetArray
    | ESliceError
    | ERecord
    | EGetRecordField
    | EGetBitField
    | EGetBadField
    | EGetBadBitField
    | EGetBadRecordField
    | EGetBitFieldNested
    | EGetBitFieldTyped
    | EGetTupleItem
    | EGetFields
    | EConcat
    | ETuple
    | EArbitrary
    | EPattern
    | LEDiscard
    | LEVar
    | LEUndefIdentV0
    | LEUndefIdentV1
    | LEDestructuring
    | LESlice
    | LESetArray
    | LESetStructuredField
    | LESetBadBitField
    | LESetBitField
    | LESetBadField
    | LESetFields
    | LEConcat
    | Slice
    | PAll
    | PAny
    | PGeq
    | PLeq
    | PNot
    | PRange
    | PSingle
    | PMask
    | PTuple
    | LDDiscard
    | LDVar
    | LDTuple
    | LDUninitialisedVar
    | LDUninitialisedTyped
    | SPass
    | SAssignCall
    | SAssign
    | SReturn
    | SSeq
    | SCall
    | SCond
    | SCase
    | SAssert
    | SWhile
    | SRepeat
    | SFor
    | SThrow
    | STry
    | SDecl
    | SPrint
    | SPragma
    | FUndefIdent
    | FPrimitive
    | FBadArity
    | FindCheckDeduce
    | AnnotateCall
    | AnnotateCallArgTyped
    | Block
    | Loop
    | For
    | Catcher
    | Subprogram
    | DeclareOneFunc
    | DeclareGlobalStorage
    | DeclareTypeDecl
    | Specification
    | TString
    | TReal
    | TBool
    | TNamed
    | TInt
    | TBits
    | TTuple
    | TArray
    | TEnumDecl
    | TStructuredDecl
    | TNonDecl
    | TBitField
    | TBitFields
    | ReduceSlicesToCall
    | TypeOfArrayLength
    | TypecheckDecl
    | CheckGlobalPragma
    | AnnotateAndDeclareFunc
    | AnnotateFuncSig
    | CheckSetterHasGetter
    | AddNewFunc
    | SubprogramForName
    | HasArgClash
    | GetUndeclaredDefining
    | AnnotateOneParam
    | AnnotateParams
    | ArgsAsParams
    | AnnotateArgs
    | StaticEval
    | ReduceConstants
    | Normalize
    | RenameTyEqs
    | TypeCheckMutuallyRec
    | DeclareSubprograms
    | AnnotateLimitExpr
    | CheckATC
    | CheckSlicesInWidth
    | DisjointSlicesToPositions
    | BitfieldSliceToPositions
    | CheckPositionsInWidth
    | ShouldReduceToCall
    | IsSymbolicallyEvaluable
    | CheckSymbolicallyEvaluable
    | ShouldRememberImmutableExpression
    | AddImmutableExpression
    | SymIntSetSubset
    | SymdomsSubset
    | SymdomsSubsetUnions
    | ApproxConstraint
    | ApproxConstraints
    | LEBitSlice
    | AddLocalImmutableExpr
    | AddGlobalImmutableExpr
    | DeclareConst
    | AddGlobalStorage
    | LookupConstant
    | TypeOf
    | LookupImmutableExpr
    | WithEmptyLocal
    | IsGlobalUndefined
    | IsLocalUndefined
    | IsUndefined
    | IsSubprogram
    | CheckVarNotInEnv
    | CheckVarNotInGEnv
    | CheckDisjointSlices
    | ControlFlowFromStmt
    | AnnotateConstraintBinop
    | ConstraintBinop
    | ConstraintMod
    | ConstraintPow
    | ApplyBinopExtremities
    | PossibleExtremitiesLeft
    | PossibleExtremitiesRight
    | ControlFlowSeq
    | ControlFlowJoin
    | CheckCommonBitfieldsAlign
    | AnnotateFieldInit
    | AnnotateGetArray
    | AnnotateSetArray
    | GetBitvectorWidth
    | GetBitvectorConstWidth
    | CheckParamDecls

  let to_string : t -> string = function
    | BuiltinSingularType -> "BuiltinSingularType"
    | BuiltinAggregateType -> "BuiltinAggregateType"
    | BuiltinSingularOrAggregate -> "BuiltinSingularOrAggregate"
    | NamedType -> "NamedType"
    | AnonymousType -> "AnonymousType"
    | SingularType -> "SingularType"
    | AggregateType -> "AggregateType"
    | NonPrimitiveType -> "NonPrimitiveType"
    | PrimitiveType -> "PrimitiveType"
    | Canonical -> "Canonical"
    | Domain -> "Domain"
    | Structure -> "Structure"
    | Subtype -> "Subtype"
    | StructuralSubtypeSatisfaction -> "StructuralSubtypeSatisfaction"
    | DomainSubtypeSatisfaction -> "DomainSubtypeSatisfaction"
    | SubtypeSatisfaction -> "SubtypeSatisfaction"
    | TypeSatisfaction -> "TypeSatisfaction"
    | TypeClash -> "TypeClash"
    | ApplyUnopType -> "ApplyUnopType"
    | ApplyBinopTypes -> "ApplyBinopTypes"
    | LowestCommonAncestor -> "LowestCommonAncestor"
    | ELit -> "ELit"
    | ATC -> "ATC"
    | EVar -> "EVar"
    | Binop -> "Binop"
    | Unop -> "Unop"
    | ECond -> "ECond"
    | ESlice -> "ESlice"
    | ECall -> "ECall"
    | ESetter -> "ESetter"
    | ERecord -> "ERecord"
    | EGetRecordField -> "EGetRecordField"
    | EGetBitField -> "EGetBitField"
    | EGetBadField -> "EGetBadField"
    | EGetBadBitField -> "EGetBadBitField"
    | EGetBadRecordField -> "EGetBadRecordField"
    | EGetBitFieldNested -> "EGetBitFieldNested"
    | EGetBitFieldTyped -> "EGetBitFieldTyped"
    | EGetTupleItem -> "EGetTupleItem"
    | EGetFields -> "EGetFields"
    | EConcat -> "EConcat"
    | ETuple -> "ETuple"
    | ECondSimple -> "ECondSimple"
    | EGetArray -> "EGetArray"
    | ESliceError -> "ESliceError"
    | EArbitrary -> "EArbitrary"
    | EPattern -> "EPattern"
    | LEDiscard -> "LEDiscard"
    | LEVar -> "LEVar"
    | LESlice -> "LESlice"
    | LESetArray -> "LESetArray"
    | LESetStructuredField -> "LESetStructuredField"
    | LESetBadBitField -> "LESetBadBitField"
    | LESetBitField -> "LESetBitField"
    | LESetBadField -> "LESetBadField"
    | LESetFields -> "LESetFields"
    | LEConcat -> "LEConcat"
    | LEDestructuring -> "LEDestructuring"
    | LEUndefIdentV0 -> "LEUndefIdentV0"
    | LEUndefIdentV1 -> "LEUndefIdentV1"
    | Slice -> "Slice"
    | PAll -> "PAll"
    | PAny -> "PAny"
    | PGeq -> "PGeq"
    | PLeq -> "PLeq"
    | PNot -> "PNot"
    | PRange -> "PRange"
    | PSingle -> "PSingle"
    | PMask -> "PMask"
    | PTuple -> "PTuple"
    | LDDiscard -> "LDDiscardNone"
    | LDVar -> "LDVar"
    | LDUninitialisedVar -> "LDUninitialisedVar"
    | LDUninitialisedTyped -> "LDUninitialisedTyped"
    | LDTuple -> "LDTuple"
    | SPass -> "SPass"
    | SAssignCall -> "SAssignCall"
    | SAssign -> "SAssign"
    | SReturn -> "SReturn"
    | SSeq -> "SThen"
    | SCall -> "SCall"
    | SCond -> "SCond"
    | SCase -> "SCase"
    | SAssert -> "SAssert"
    | SWhile -> "SWhile"
    | SRepeat -> "SRepeat"
    | SFor -> "SFor"
    | SThrow -> "SThrow"
    | STry -> "STry"
    | SDecl -> "SDecl"
    | SPrint -> "SPrint"
    | SPragma -> "SPragma"
    | FUndefIdent -> "FUndefIdent"
    | FPrimitive -> "FPrimitive"
    | FBadArity -> "FBadArity"
    | FindCheckDeduce -> "FindCheckDeduce"
    | AnnotateCall -> "AnnotateCall"
    | AnnotateCallArgTyped -> "AnnotateCallArgTyped"
    | Block -> "Block"
    | Loop -> "Loop"
    | For -> "For"
    | Catcher -> "Catcher"
    | Subprogram -> "Subprogram"
    | DeclareOneFunc -> "DeclareOneFunc"
    | DeclareGlobalStorage -> "DeclareGlobalStorage"
    | DeclareTypeDecl -> "DeclareTypeDecl"
    | Specification -> "Specification"
    | TString -> "TString"
    | TReal -> "TReal"
    | TBool -> "TBool"
    | TNamed -> "TNamed"
    | TInt -> "TInt"
    | TBits -> "TBits"
    | TTuple -> "TTuple"
    | TArray -> "TArray"
    | TEnumDecl -> "TEnumDecl"
    | TStructuredDecl -> "TStructuredDecl"
    | TNonDecl -> "TNonDecl"
    | TBitField -> "TBitField"
    | TBitFields -> "TBitFields"
    | ReduceSlicesToCall -> "ReduceSlicesToCall"
    | TypeOfArrayLength -> "TypeOfArrayLength"
    | TypecheckDecl -> "TypecheckDecl"
    | CheckGlobalPragma -> "CheckGlobalPragmas"
    | AnnotateAndDeclareFunc -> "AnnotateAndDeclareFunc"
    | AnnotateFuncSig -> "AnnotateFuncSig"
    | CheckSetterHasGetter -> "CheckSetterHasGetter"
    | AddNewFunc -> "AddNewFunc"
    | SubprogramForName -> "SubprogramForName"
    | HasArgClash -> "HasArgClash"
    | GetUndeclaredDefining -> "GetUndeclaredDefining"
    | AnnotateOneParam -> "AnnotateOneParam"
    | AnnotateParams -> "AnnotateParams"
    | ArgsAsParams -> "ArgsAsParams"
    | AnnotateArgs -> "AnnotateArgs"
    | StaticEval -> "StaticEval"
    | ReduceConstants -> "ReduceConstants"
    | Normalize -> "Normalize"
    | RenameTyEqs -> "RenameTyEqs"
    | TypeCheckMutuallyRec -> "TypeCheckMutuallyRec"
    | DeclareSubprograms -> "DeclareSubprograms"
    | AnnotateLimitExpr -> "AnnotateLimitExpr"
    | CheckATC -> "CheckATC"
    | CheckSlicesInWidth -> "CheckSlicesInWidth"
    | DisjointSlicesToPositions -> "DisjointSlicesToPositions"
    | BitfieldSliceToPositions -> "BitfieldSliceToPositions"
    | CheckPositionsInWidth -> "CheckPositionsInWidth"
    | ShouldReduceToCall -> "ShouldReduceToCall"
    | IsSymbolicallyEvaluable -> "IsSymbolicallyEvaluable"
    | CheckSymbolicallyEvaluable -> "CheckSymbolicallyEvaluable"
    | ShouldRememberImmutableExpression -> "ShouldRememberImmutableExpression"
    | AddImmutableExpression -> "AddImmutableExpression"
    | SymIntSetSubset -> "SymIntSetSubset"
    | SymdomsSubset -> "SymdomsSubset"
    | SymdomsSubsetUnions -> "SymdomsSubsetUnions"
    | ApproxConstraint -> "ApproxConstraint"
    | ApproxConstraints -> "ApproxConstraints"
    | LEBitSlice -> "LEBitSlice"
    | AddLocalImmutableExpr -> "AddLocalImmutableExpr"
    | AddGlobalImmutableExpr -> "AddLocalImmutableExpr"
    | DeclareConst -> "DeclareConst"
    | AddGlobalStorage -> "AddGlobalStorage"
    | LookupConstant -> "LookupConstant"
    | TypeOf -> "TypeOf"
    | LookupImmutableExpr -> "LookupImmutableExpr"
    | WithEmptyLocal -> "WithEmptyLocal"
    | IsGlobalUndefined -> "IsGlobalUndefined"
    | IsLocalUndefined -> "IsLocalUndefined"
    | IsUndefined -> "IsUndefined"
    | IsSubprogram -> "IsSubprogram"
    | CheckVarNotInEnv -> "CheckVarNotInEnv"
    | CheckVarNotInGEnv -> "CheckVarNotInGEnv"
    | CheckDisjointSlices -> "CheckDisjointSlices"
    | ControlFlowFromStmt -> "ControlFlowFromStmt"
    | AnnotateConstraintBinop -> "AnnotateConstraintBinop"
    | ConstraintBinop -> "ConstraintBinop"
    | ConstraintMod -> "ConstraintMod"
    | ConstraintPow -> "ConstraintPow"
    | ApplyBinopExtremities -> "ApplyBinopExtremities"
    | PossibleExtremitiesLeft -> "PossibleExtremitiesLeft"
    | PossibleExtremitiesRight -> "PossibleExtremitiesRight"
    | ControlFlowSeq -> "ControlFlowSeq"
    | ControlFlowJoin -> "ControlFlowJoin"
    | CheckCommonBitfieldsAlign -> "CheckCommonBitfieldsAlign"
    | AnnotateFieldInit -> "AnnotateFieldInit"
    | AnnotateGetArray -> "AnnotateGetArray"
    | AnnotateSetArray -> "AnnotateSetArray"
    | GetBitvectorWidth -> "GetBitvectorWidth"
    | GetBitvectorConstWidth -> "GetBitvectorConstWidth"
    | CheckParamDecls -> "CheckParamDecls"

  let pp f r = to_string r |> Format.pp_print_string f

  let all =
    [
      BuiltinSingularType;
      BuiltinAggregateType;
      BuiltinSingularOrAggregate;
      SingularType;
      AggregateType;
      NamedType;
      AnonymousType;
      NonPrimitiveType;
      PrimitiveType;
      Canonical;
      Domain;
      Structure;
      Subtype;
      DomainSubtypeSatisfaction;
      StructuralSubtypeSatisfaction;
      SubtypeSatisfaction;
      TypeSatisfaction;
      TypeClash;
      ApplyUnopType;
      ApplyBinopTypes;
      LowestCommonAncestor;
      ELit;
      ATC;
      EVar;
      Binop;
      Unop;
      ECond;
      ESlice;
      ECall;
      ESetter;
      ERecord;
      EGetRecordField;
      EGetBadField;
      EGetBadBitField;
      EGetBadRecordField;
      EGetBitField;
      EGetBitFieldNested;
      EGetBitFieldTyped;
      EGetTupleItem;
      EGetFields;
      EArbitrary;
      EPattern;
      EGetArray;
      ESliceError;
      ECondSimple;
      EConcat;
      ETuple;
      LEDiscard;
      LEVar;
      LESlice;
      LESetArray;
      LESetStructuredField;
      LESetBadBitField;
      LESetBitField;
      LESetBadField;
      LESetFields;
      LESetFields;
      LEDestructuring;
      LEConcat;
      Slice;
      SPass;
      SAssignCall;
      SAssign;
      SReturn;
      SSeq;
      SCall;
      SCond;
      SCase;
      SAssert;
      SWhile;
      SRepeat;
      SFor;
      SThrow;
      STry;
      SDecl;
      SPrint;
      SPragma;
      FUndefIdent;
      FPrimitive;
      FBadArity;
      FindCheckDeduce;
      AnnotateCall;
      AnnotateCallArgTyped;
      Block;
      Loop;
      For;
      Catcher;
      Subprogram;
      TString;
      TReal;
      TBool;
      TNamed;
      TInt;
      TBits;
      TTuple;
      TArray;
      TEnumDecl;
      TStructuredDecl;
      TNonDecl;
      TBitField;
      TBitFields;
      ReduceSlicesToCall;
      TypeOfArrayLength;
      TypecheckDecl;
      CheckGlobalPragma;
      AnnotateAndDeclareFunc;
      AnnotateFuncSig;
      CheckSetterHasGetter;
      AddNewFunc;
      SubprogramForName;
      HasArgClash;
      GetUndeclaredDefining;
      AnnotateOneParam;
      AnnotateParams;
      ArgsAsParams;
      AnnotateArgs;
      StaticEval;
      ReduceConstants;
      Normalize;
      RenameTyEqs;
      TypeCheckMutuallyRec;
      DeclareSubprograms;
      AnnotateLimitExpr;
      CheckATC;
      CheckSlicesInWidth;
      DisjointSlicesToPositions;
      BitfieldSliceToPositions;
      CheckPositionsInWidth;
      ShouldReduceToCall;
      IsSymbolicallyEvaluable;
      CheckSymbolicallyEvaluable;
      ShouldRememberImmutableExpression;
      AddImmutableExpression;
      SymIntSetSubset;
      SymdomsSubset;
      SymdomsSubsetUnions;
      ApproxConstraint;
      ApproxConstraints;
      LEBitSlice;
      AddLocalImmutableExpr;
      AddGlobalImmutableExpr;
      DeclareConst;
      AddGlobalStorage;
      LookupConstant;
      TypeOf;
      LookupImmutableExpr;
      WithEmptyLocal;
      IsGlobalUndefined;
      IsLocalUndefined;
      IsUndefined;
      IsSubprogram;
      CheckVarNotInEnv;
      CheckVarNotInGEnv;
      CheckDisjointSlices;
      BitfieldSliceToPositions;
      ControlFlowFromStmt;
      AnnotateConstraintBinop;
      ConstraintBinop;
      ConstraintPow;
      ApplyBinopExtremities;
      PossibleExtremitiesLeft;
      PossibleExtremitiesRight;
      ControlFlowSeq;
      ControlFlowJoin;
      CheckCommonBitfieldsAlign;
      AnnotateFieldInit;
      AnnotateGetArray;
      AnnotateSetArray;
      GetBitvectorWidth;
      GetBitvectorConstWidth;
      CheckParamDecls;
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

let ( |: ) = TypingNoInstr.use_with
