[@@@warning "-40-42"]

open Asllib
open Printf

type ml_sig = {
  orig_name : string;
  ml_name : string;
  args : string list;
  params : string list;
  returns : bool;
}

let asl_decl_to_asl_sig (decl : AST.decl) : AST.func =
  let open AST in
  match decl.desc with D_Func f -> f | _ -> assert false

let asl_name_to_ml_name =
  let remove_starting_underscore ~original s =
    if String.length s <= 0 || String.length original <= 0 then s
    else if String.get s 0 == '_' && String.get original 0 <> '_' then
      String.sub s 1 (String.length s - 1)
    else s
  in
  let re0 = Str.regexp "\\([a-z]\\)\\([A-Z]\\)"
  and re1 = Str.regexp "\\([^_]\\)\\([A-Z][a-z]\\)" in
  fun s ->
    s
    |> Str.global_replace re0 "\\1_\\2"
    |> Str.global_replace re1 "\\1_\\2"
    |> String.lowercase_ascii
    |> remove_starting_underscore ~original:s

let asl_sig_to_ml_sig (asl_sig : AST.func) : ml_sig =
  let open AST in
  {
    orig_name = asl_sig.name;
    ml_name = asl_name_to_ml_name asl_sig.name;
    returns = Option.is_some asl_sig.return_type;
    args = List.map (fun (s, _) -> asl_name_to_ml_name s) asl_sig.args;
    params = List.map (fun (s, _) -> asl_name_to_ml_name s) asl_sig.parameters;
  }

let list_is_empty = function [] -> true | _ :: _ -> false
let pp_list pp_elt chan = List.iter (pp_elt chan)

let fprintf_ml_sig chan ml_sig =
  let pp_one_arg chan s = fprintf chan " -> %s:v m" s in
  fprintf chan "val %s : ii%t -> %s m" ml_sig.ml_name
    (fun chan ->
      if list_is_empty ml_sig.args && list_is_empty ml_sig.params then
        output_string chan " -> unit"
      else begin
        pp_list pp_one_arg chan ml_sig.params;
        pp_list pp_one_arg chan ml_sig.args
      end)
    (if ml_sig.returns then "v" else "unit")

let open_write_file filename f =
  let chan = open_out filename in
  let finally () = close_out_noerr chan in
  Fun.protect ~finally (fun () -> f chan)

(** [print_n c chan n] prints [c0; c1; ... cn] *)
let print_n c chan n =
  if n > 0 then begin
    fprintf chan " %c0" c;
    for i = 1 to n - 1 do
      fprintf chan "; %c%d" c i
    done;
    fprintf chan " "
  end

(* arguments are named [a0], [a1], ... [an] *)
let print_arg_n = print_n 'a'

(* parameters are named [p0], [p1], ... [pn] *)
let print_param_n = print_n 'p'

(* ignored values are named [_0], [_1], ... [_n] *)
let print_ignored_n = print_n '_'

(* [print_arg_list_c c chan args] prints each argument in args preceded by a
   [~] and it numbered name. *)
let print_arg_list_c c chan args =
  List.iteri (fun i s -> fprintf chan " ~%s:%c%d" s c i) args

let print_arg_list = print_arg_list_c 'a'
let print_param_list = print_arg_list_c 'p'

let print_ml_specialized_primitive chan ml_sig =
  let nargs = List.length ml_sig.args and nparams = List.length ml_sig.params in
  fprintf chan
    {|
  let %s ii (params : v m list) (args : v m list) : v m list m =
    match (params, args) with
    | [%a], [%a] ->
        %s ii%t |> %s
    | _, [%a] ->
        arity_error %S ~expected:%d ~actual:(List.length params)
    | _ ->
        arity_error %S ~expected:%d ~actual:(List.length args)
|}
    ml_sig.ml_name print_param_n nparams print_arg_n nargs ml_sig.ml_name
    (fun chan ->
      if list_is_empty ml_sig.args then output_string chan " ()"
      else begin
        print_param_list chan ml_sig.params;
        print_arg_list chan ml_sig.args
      end)
    (if ml_sig.returns then "wrap_return" else "no_return")
    print_ignored_n nargs ml_sig.orig_name nparams ml_sig.orig_name nargs

let print_ml_sigs chan = List.iter (fprintf chan "  %a\n" fprintf_ml_sig)

let template : _ format =
  {|(* Auto-generated file - do not edit. *)

module type I = sig
  type ii
  type v
  type 'a m

  (* Utils *)
  val return : 'a -> 'a m
  val bind : 'a m -> ('a -> 'b m) -> 'b m
  val arity_error : string -> expected:int -> actual:int -> 'a

  (* Primitives *)
%aend

module Make(I: I) = struct
  open I

  let wrap_return x = return [ x ]
  let no_return m = bind m (fun () -> return [])
%a
  let primitives =
    [
%a    ]
end
|}

let main ~input ~output =
  let ast = Builder.from_file `ASLv1 input in
  let asl_sigs = List.map asl_decl_to_asl_sig ast in
  let corresponding_ml_defs = List.map asl_sig_to_ml_sig asl_sigs in
  open_write_file output @@ fun chan ->
  fprintf chan template print_ml_sigs corresponding_ml_defs
    (pp_list print_ml_specialized_primitive)
    corresponding_ml_defs
    (pp_list (fun chan asl_sig ->
         fprintf chan "      (%s, %S);\n" asl_sig.ml_name asl_sig.orig_name))
    corresponding_ml_defs

let print_usage () =
  eprintf
    {|primitive_to_signature INPUT_FILE OUTPUT_FILE
    writes in OUTPUT_FILE the OCaml signature needed to implement the functions in INPUT_FILE.|}

let () =
  if Array.length Sys.argv != 3 then (
    print_usage ();
    exit 1);

  main ~input:Sys.argv.(1) ~output:Sys.argv.(2)
