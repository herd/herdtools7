open Asllib
open Feat
open! Enum

let binops : AST.binop enum =
  finite
    [
      `AND;
      `BAND;
      `BEQ;
      `BOR;
      `DIV;
      `DIVRM;
      `XOR;
      `EQ;
      `GT;
      `GE;
      `IMPL;
      `LT;
      `LE;
      `MOD;
      `SUB;
      `MUL;
      `NE;
      `OR;
      `ADD;
      `POW;
      `RDIV;
      `SHL;
      `SHR;
      `BV_CONCAT;
      `STR_CONCAT;
    ]

let unops : AST.unop enum = finite AST.[ BNOT; NEG; NOT ]
let literal = ASTUtils.one_expr

let exprs : AST.expr enum =
  let ( let+ ) x f = map f x and ( and+ ) = ( ** ) in
  let e_binops exprs =
    let+ e1 = exprs and+ op = binops and+ e2 = exprs in
    AST.E_Binop (op, e1, e2) |> ASTUtils.add_dummy_annotation
  and e_unops exprs =
    let+ e = exprs and+ op = unops in
    AST.E_Unop (op, e) |> ASTUtils.add_dummy_annotation
  and e_conds exprs =
    let+ e = exprs in
    AST.E_Cond (literal, literal, e) |> ASTUtils.add_dummy_annotation
  in
  Fix.Memoize.Int.fix @@ fun exprs ->
  let exprs = pay exprs in
  just literal ++ e_binops exprs ++ e_unops exprs ++ e_conds exprs

(** Convert expressions to string.

    Compared to [PP.expr_to_string], this function never puts parentheses around
    binary operations, whereas [PP.expr_to_string] always puts parentheses
    around binary operations. *)
let rec expr_to_string =
  let open AST in
  let open Printf in
  fun e ->
    match e.desc with
    | E_Binop (op, e1, e2) ->
        sprintf "%s %s %s" (expr_to_string e1) (PP.binop_to_string op)
          (expr_to_string e2)
    | E_Unop (op, e) ->
        sprintf "%s %s" (PP.unop_to_string op) (expr_to_string e)
    | E_Literal _ when e == literal -> "1"
    | E_Cond (e1, e2, e3) when e1 == literal && e2 == literal ->
        sprintf "if 1 then 1 else %s" (expr_to_string e3)
    | _ -> assert false

let tests ~max_per_size ~max_size : string Seq.t =
  let exprs_seq = sample max_per_size exprs 0 max_size Seq.empty in
  Seq.map
    (fun e ->
      Printf.sprintf
        "func main () => integer\n  let - = %s;\n\n  return 0;\nend;"
        (expr_to_string e))
    exprs_seq

let compare_one_test ~filename ~ast_string =
  let aslref =
    try
      Asllib.Builder.from_string ~filename ~ast_string `ASLv1 |> Fun.const None
    with e -> Some e
  and bnfc =
    let open Bnfc_parser in
    let lexbuf = Lexing.from_string ~with_positions:true ast_string in
    try ParGrammar.pSpec LexGrammar.token lexbuf |> Fun.const None
    with e -> Some e
  in
  match (aslref, bnfc) with
  | None, None | Some _, Some _ -> true
  | Some e, None ->
      let () =
        Printf.eprintf
          "Discrepancy found: bnfc generated found no error, while ASLRef \
           found error:\n\
           %s\n\
           %!"
          (Stdlib.Printexc.to_string e)
      in
      false
  | None, Some e ->
      let () =
        Printf.eprintf
          "Discrepancy found: ASLRef found no error, while bnfc generated \
           found error:\n\
           %s\n\
           %!"
          (Stdlib.Printexc.to_string e)
      in
      false

let max_size = 6
let max_per_size = 100_000

let () =
  let tests = tests ~max_size ~max_per_size in
  let res =
    Seq.fold_lefti
      (fun acc i ast_string ->
        let filename = Printf.sprintf "test-binops-%d.asl (fake)" i in
        compare_one_test ~filename ~ast_string && acc)
      true tests
  in
  exit (if res then 0 else 1)
