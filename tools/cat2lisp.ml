(*
  * SPDX-FileCopyrightText: Copyright 2025 Arm Limited and/or its affiliates <open-source-office@arm.com>
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

open Format
open Asllib.Lispobj
open AST

let catsym_alist x = sym_alist "CAT" x

let of_pos _ = nil
let of_loc _ = nil
let of_set_or_rln x =
  key (match x with SET -> "SET" | RLN -> "RLN")

let of_op2 x =
  key (match x with
       | Union -> "UNION"
       | Inter -> "INTER" 
       | Diff -> "DIFF"
       | Seq -> "SEQ" 
       | Cartesian -> "CARTESIAN"
       | Add -> "ADD"  
       | Tuple -> "TUPLE")

let of_op1 x =
  key (match x with
       | Plus -> "PLUS"
       | Star -> "STAR"
       | Opt -> "OPT"
       | Comp -> "COMP"
       | Inv -> "INV" 
       | ToId -> "TOID")

let of_konst x =
  of_list (match x with
           | Empty sr -> [ key "EMPTY"; of_set_or_rln sr ]
           | Universe sr -> [ key "UNIVERSE"; of_set_or_rln sr ])

let of_var x = String x
let of_tag x = String x

let of_varset x = of_list_map of_tag ( List.of_seq (StringSet.to_seq x) )

let of_pat0 x = of_option of_var x
let of_pat x =
  of_list (match x with
           | Pvar pat0 -> [ key "PVAR"; of_pat0 pat0 ]
           | Ptuple pat0s -> [ key "PTUPLE"; of_list_map of_pat0 pat0s ])

let rec of_variant_cond x =
  of_list (match x with
           | Variant s -> [ key "VARIANT"; String s ]
           | OpNot c -> [ key "VAR_OPNOT"; of_variant_cond c ]
           | OpAnd (c1, c2) -> [ key "VAR_OPAND"; of_variant_cond c1; of_variant_cond c2 ]
           | OpOr (c1, c2) -> [ key "VAR_OPOR"; of_variant_cond c1; of_variant_cond c2 ])


           



let rec of_exp x =
  of_list (match x with
           | Konst (loc, konst)                 -> [ key "E_KONST"; of_loc loc; of_konst konst ]
           | Tag (loc, tag)                     -> [ key "E_TAG"; of_loc loc; of_tag tag ]
           | Var (loc, var)                     -> [ key "E_VAR"; of_loc loc; of_var var ]
           | Op1 (loc, op, exp)                 -> [ key "E_OP1"; of_loc loc; of_op1 op; of_exp exp ]
           | Op (loc, op, exps)                 -> [ key "E_OP"; of_loc loc; of_op2 op; of_list_map of_exp exps ]
           | App (loc, fn, arg)                 -> [ key "E_APP"; of_loc loc; of_exp fn; of_exp arg ]
           | Bind (loc, bindings, body)         -> [ key "E_BIND"; of_loc loc; of_list_map of_binding bindings; of_exp body ]
           | BindRec (loc, bindings, body)      -> [ key "E_BINDREC"; of_loc loc; of_list_map of_binding bindings; of_exp body ]
           | Fun (loc, pat, exp, var, varset)   -> [ key "E_FUN"; of_loc loc; of_pat pat; of_exp exp; of_var var; of_varset varset ]
           | ExplicitSet (loc, exps)            -> [ key "E_EXPLICITSET"; of_loc loc; of_list_map of_exp exps ]
           | Match (loc, exp, clauses, default) -> [ key "E_MATCH"; of_loc loc; of_exp exp; of_list_map of_clause clauses; of_option of_exp default ]
           | MatchSet (loc, exp1, exp2, clause) -> [ key "E_MATCHSET"; of_loc loc; of_exp exp1; of_exp exp2; of_set_clause clause ]
           | Try (loc, exp1, exp2)              -> [ key "E_TRY"; of_loc loc; of_exp exp1; of_exp exp2 ]
           | If (loc, cond, ethen, eelse)       -> [ key "E_IF"; of_loc loc; of_cond cond; of_exp ethen; of_exp eelse ])

and of_set_clause x =
  of_list (match x with
           | EltRem (p1, p2, exp) -> [ key "ELTREM"; of_pat0 p1; of_pat0 p2; of_exp exp ]
           | PreEltPost (p1, p2, p3, exp) -> [ key "PREELTPOST"; of_pat0 p1; of_pat0 p2; of_pat0 p3; of_exp exp ])

and of_cond x =
  of_list (match x with
           | Eq (e1, e2)      -> [ key "COND_EQ"; of_exp e1; of_exp e2 ]
           | Subset (e1, e2)  -> [ key "COND_SUBSET"; of_exp e1; of_exp e2 ]
           | In (e1, e2)      -> [ key "COND_IN"; of_exp e1; of_exp e2 ]
           | VariantCond c -> [ key "COND_VARIANT"; of_variant_cond c ])

and of_clause (str, exp) =
  catsym_alist [
      ("TAG", String str);
      ("EXP", of_exp exp)
    ]

and of_binding (loc, pat, exp) =
  catsym_alist [
      ("LOC", of_loc loc);
      ("PAT", of_pat pat);
      ("EXP", of_exp exp);
    ]



let of_do_test x =
  key (match x with Acyclic -> "ACYCLIC" | Irreflexive -> "IRREFLEXIVE" | TestEmpty -> "TESTEMPTY" )

let of_test x =
  of_list (match x with
           | Yes test -> [ key "T_YES"; of_do_test test ]
           | No test  -> [ key "T_NO"; of_do_test test ])

let of_test_type x =
  key (match x with Flagged -> "FLAGGED" | UndefinedUnless -> "UNDEFINEDUNLESS" | Check -> "CHECK" | Assert -> "ASSERT" )

let of_app_test (loc, pos, test, exp, str) =
    catsym_alist [
        ("LOC", of_loc loc);
        ("POS", of_pos pos);
        ("TEST", of_test test);
        ("EXP", of_exp exp);
        ("NAME", of_option of_str str)
      ]

let of_is_rec x =
  key (match x with IsRec -> "ISREC" | IsNotRec -> "ISNOTREC" )


let rec of_ins x =
  of_list (match x with
           | Let (loc, bindings)       -> [ key "I_LET"; of_loc loc; of_list_map of_binding bindings ]
           | Rec (loc, bindings, test) -> [ key "I_REC"; of_loc loc; of_list_map of_binding bindings; of_option of_app_test test ]
           | InsMatch (loc, exp, clauses, insts)
             -> [ key "I_INSMATCH"; of_loc loc; of_exp exp; of_list_map of_insclause clauses; of_option (of_list_map of_ins) insts ]
           | Test ( test, testtype )   -> [ key "I_TEST"; of_app_test test; of_test_type testtype ]
           | UnShow (loc, strs)        -> [ key "I_UNSHOW"; of_loc loc; of_list_map of_str strs ]
           | Show (loc, strs)          -> [ key "I_SHOW"; of_loc loc; of_list_map of_str strs ]
           | ShowAs (loc, exp, str)    -> [ key "I_SHOWAS"; of_loc loc; of_exp exp; of_str str ]
           | Include (_, _)            -> raise (Misc.Fatal "Unexpected include after expansion")
           | Procedure (loc, var, pat, insts, isrec)
             -> [ key "I_PROCEDURE"; of_loc loc; of_var var; of_pat pat; of_list_map of_ins insts; of_is_rec isrec ]
           | Call (loc, var, exp, str) -> [ key "I_CALL"; of_loc loc; of_var var; of_exp exp; of_option of_str str ]
           | Enum (loc, var, tags)     -> [ key "I_ENUM"; of_loc loc; of_var var; of_list_map of_tag tags ]
           | Forall (loc, var, exp, insts)
             -> [ key "I_FORALL"; of_loc loc; of_var var; of_exp exp; of_list_map of_ins insts ]
           | Debug (loc, exp)          -> [ key "I_DEBUG"; of_loc loc; of_exp exp ]
           | WithFrom (loc, var, exp)  -> [ key "I_WITHFROM"; of_loc loc; of_var var; of_exp exp ]
           | Events (loc, var, exps, bool)
             -> [ key "I_EVENTS"; of_loc loc; of_var var; of_list_map of_exp exps; of_bool bool ]
           | IfVariant (loc, cond, thens, elses)
             -> [ key "I_IFVARIANT"; of_loc loc; of_variant_cond cond; of_list_map of_ins thens; of_list_map of_ins elses ])
and of_insclause (str, insts) =
  catsym_alist [
      ("TAG", of_str str);
      ("BODY", of_list_map of_ins insts)
    ]




module Make
         (O:sig
              val verbose : int
              val includes : string list
              val libdir : string
            end) =
  struct
    let libfind =
      let module ML =
        MyLib.Make
          (struct
            let includes = O.includes
            let env = Some "HERDLIB"
            let libdir = O.libdir
            let debug = O.verbose > 0
          end) in
      ML.find

    module ParserConfig =
      struct
        let debug = false
        let libfind = libfind
      end
    module P = ParseModel.Make(ParserConfig)



    let rec expand_ins x =
      match x with
      | InsMatch (loc, exp, clauses, insts) ->
         [ InsMatch (loc, exp,
                     List.map expand_insclause clauses,
                     Option.map expand_inslist insts) ]
      | Procedure (loc, var, pat, insts, is_rec) ->
         [ Procedure (loc, var, pat,
                      expand_inslist insts, is_rec) ]
      | Forall (loc, var, exp, insts) ->
         [ Forall (loc, var, exp,
                   expand_inslist insts) ]
      | IfVariant (loc, cond, thens, elses) ->
         [ IfVariant(loc, cond,
                     expand_inslist thens,
                     expand_inslist elses) ]
      | Include (_, fname) ->
         parse fname
      | _ -> [ x ]

    and expand_insclause (str, insts) =
      (str, expand_inslist insts)

    and expand_inslist x =
      List.concat (List.map expand_ins x)

    and parse fname =
      let (_, _, ast) = P.parse fname in
      List.concat (List.map expand_ins ast)
  end



let verbose = ref 0
let libdir = ref (Filename.concat Version.libdir "herd")
let includes = ref []

let options = [
    ("-set-libdir", Arg.String (fun s -> libdir := s),
    "<path> set installation directory to <path>");
    ("-I", Arg.String (fun s -> includes := !includes @ [s]),
   "<dir> add <dir> to search path");
    ("-v", Arg.Unit (fun _ -> incr verbose),
   "<non-default> show various diagnostics, repeat to increase verbosity");
    ("-q", Arg.Unit (fun _ -> verbose := -1 ),
   "<default> do not show diagnostics");
  ]

let args = ref []
let get_cmd_arg s = args := s :: !args

let prog = "cat2lisp"

let () =
  try
    Arg.parse options
      get_cmd_arg
      (sprintf "Usage: %s ..." prog)
  with
  | Misc.Fatal msg -> eprintf "%s: %s\n" prog msg

let cats = List.rev !args

let () =
  let module MyParse = Make (struct
                           let verbose = !verbose
                           let includes = !includes
                           let libdir = !libdir
                         end) in
  let ast = List.concat (List.map MyParse.parse cats) in
  let lisp_ast = of_list_map of_ins ast in
  print_obj Format.std_formatter lisp_ast

