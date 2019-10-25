(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2010-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

open Printf

exception Exit
exception UserError of string
exception Fatal of string
exception NoIsync

(************)
(* Switches *)
(************)
let switch = ref false
let switchelse = ref false

(**************)
(* File names *)
(**************)

let dot_name name_xxx =
  let base = Filename.chop_extension name_xxx in
  base ^ ".dot"

let filebase f =
  let f = Filename.basename f in
  let b =
    try Filename.chop_extension f
    with Invalid_argument _ -> f in
  b


(****************)
(* basic utils  *)
(****************)

let polymorphic_compare = compare

external int_compare : int -> int -> int = "caml_int_compare"
let int_eq (x:int) (y:int) = x == y
let string_eq (s1:string) (s2:string) = (=) s1 s2

external identity : 'a -> 'a = "%identity"

let ing _ = ()
let ing2 _ _ = ()

let is_none = function
  | None -> true
  | Some _ -> false

let is_some = function
  | None -> false
  | Some _ -> true

let as_some = function
  | Some x -> x
  | None -> assert false

let proj_opt default = function
  | None -> default
  | Some x -> x

let app_opt f = function
  | None -> None
  | Some x -> Some (f x)

let map_opt = app_opt

let app_opt_def d f = function
  | None -> d
  | Some x -> f x

let rec last = function
  | [] -> assert false
  | [x] -> x
  | _::xs -> last xs

let rec option_map f xs = match xs with
| [] -> []
| x::xs ->
   match f x with
   | None -> option_map f xs
   | Some y -> y :: option_map f xs

let map_string f s =
  let b = Buffer.create (String.length s) in
  for k=0 to String.length s-1 do
    Buffer.add_string b (f s.[k])
  done ;
  Buffer.contents b

let opt_compare cmp x y = match x,y with
| None,None -> 0
| None,Some _ -> -1
| Some _,None -> 1
| Some x,Some y -> cmp x y

let opt_eq eq x y = match x,y with
| None,None -> true
| Some x,Some y -> eq x y
| (None,Some _)|(Some _,None) -> false

let pair_compare cmpx cmpy (x1,y1) (x2,y2) =
  match cmpx x1 x2 with
  | 0 -> cmpy y1 y2
  | r -> r

(* Avoid String.lowercase warning *)
let char_lowercase = function
  | 'A' -> 'a'
  | 'B' -> 'b'
  | 'C' -> 'c'
  | 'D' -> 'd'
  | 'E' -> 'e'
  | 'F' -> 'f'
  | 'G' -> 'g'
  | 'H' -> 'h'
  | 'I' -> 'i'
  | 'J' -> 'j'
  | 'K' -> 'k'
  | 'L' -> 'l'
  | 'M' -> 'm'
  | 'N' -> 'n'
  | 'O' -> 'o'
  | 'P' -> 'p'
  | 'Q' -> 'q'
  | 'R' -> 'r'
  | 'S' -> 's'
  | 'T' -> 't'
  | 'U' -> 'u'
  | 'V' -> 'v'
  | 'W' -> 'w'
  | 'X' -> 'x'
  | 'Y' -> 'y'
  | 'Z' -> 'z'
  | c -> c

let char_uppercase = function
  | 'a' -> 'A'
  | 'b' -> 'B'
  | 'c' -> 'C'
  | 'd' -> 'D'
  | 'e' -> 'E'
  | 'f' -> 'F'
  | 'g' -> 'G'
  | 'h' -> 'H'
  | 'i' -> 'I'
  | 'j' -> 'J'
  | 'k' -> 'K'
  | 'l' -> 'L'
  | 'm' -> 'M'
  | 'n' -> 'N'
  | 'o' -> 'O'
  | 'p' -> 'P'
  | 'q' -> 'Q'
  | 'r' -> 'R'
  | 's' -> 'S'
  | 't' -> 'T'
  | 'u' -> 'U'
  | 'v' -> 'V'
  | 'w' -> 'W'
  | 'x' -> 'X'
  | 'y' -> 'Y'
  | 'z' -> 'Z'
  | c -> c

let xcase c s =
   let r =
    Bytes.init (String.length s)
      (fun k -> c (String.unsafe_get s k)) in
   Bytes.unsafe_to_string r

let lowercase s = xcase char_lowercase s
let uppercase s = xcase char_uppercase s

let capitalize s = match s with
| "" -> assert false
| _ ->
    String.make 1 (char_uppercase s.[0]) ^
    String.sub s 1 (String.length s-1)

(* Compatibility *)
let rec find_opt p = function
  | [] -> None
  | x::xs ->
      if p x then Some x
      else find_opt p xs

let split_on_char c s =
  let len = String.length s in
  let rec do_rec k0 k =
    if k >= len then [String.sub s k0 (k-k0) ]
    else if c = String.unsafe_get s k then
      String.sub s k0 (k-k0)::do_rec (k+1) (k+1)
    else do_rec k0 (k+1) in
  do_rec 0 0

(********************)
(* Position parsing *)
(********************)

let pos_of_string s =
  try
    let k = String.index s ',' in
    Some
      (float_of_string (String.sub s 0 k),
       float_of_string (String.sub s (k + 1) (String.length s-(k+1))))
  with _ -> None

(***************)
(* int parsing *)
(***************)

let string_as_int s = try int_of_string s with Failure _ -> assert false

let string_of_intkm s =
  let len = String.length s in
  try
    let x =
      match s.[len-1] with
      | 'k'|'K' -> 1000 * int_of_string (String.sub s 0 (len-1))
      | 'm'|'M' -> 1000000 * int_of_string (String.sub s 0 (len-1))
      | _ -> int_of_string s in
    Some x
  with
  | Failure _ -> None


(***********)
(* Explode *)
(***********)

let explode s =
  let r = ref [] in
  for k =String.length s-1 downto 0 do
    r := s.[k] :: !r
  done ;
  !r

(* Fold utilities *)

let fold_to_iter fold  f = fold (fun x () -> f x) ()
let fold_bool f k =  f true (f false k)

(******************)
(* List utilities *)
(******************)
let consp = function
  | [] -> false
  | _::_ -> true

let cons x xs = x::xs

let pp_list chan sep pp_x =
  let rec do_rec = function
    | [] -> ()
    | [x] -> pp_x chan x
    | x::xs -> pp_x chan x ; output_string chan sep ; do_rec xs in
  do_rec

let rec rev_iter f  = function
  | [] -> ()
  | x::xs -> rev_iter f xs ; f x

let rec interval n m = if n < m then n::interval (n+1) m else []
let rec replicate n x =
  if n <= 0 then [] else x::replicate (n-1) x

let iteri f =
  let rec iter_rec i xs = match xs with
  | [] -> ()
  | x::xs -> f i x ; iter_rec (i+1) xs in
  iter_rec 0

let mapi f =
  let rec map_rec i xs = match xs with
  | [] -> []
  | x::xs -> f i x :: map_rec (i+1) xs in
  map_rec 0

let rev_filter p xs =
  let rec do_rec ys = function
    | [] -> ys
    | x::xs ->
        do_rec (if p x then x::ys else ys) xs in
  do_rec [] xs

let rec map3 f xs ys zs = match xs,ys,zs with
| [],[],[] -> []
| x::xs,y::ys,z::zs ->
    f x y z::map3 f xs ys zs
| _,_,_ -> assert false

let rec list_compare cmp xs ys = match xs,ys with
| [],[] -> 0
| [],_::_ -> -1
| _::_,[] -> 1
| x::xs,y::ys ->
    begin match cmp x y with
    | 0 -> list_compare cmp xs ys
    | r -> r
    end

let rem_dups is_same =
  let rec rem_rec prev = function
    | [] -> []
    | atom::rem ->
        if is_same prev atom then rem_rec prev rem
        else atom::rem_rec atom rem in
  fun atoms -> match atoms with
  | [] -> []
  | atom::atoms ->
      atom::rem_rec atom atoms

let rec for_all_strict p = function
  | [] -> true
  | x::xs ->
      let b = p x in
      if b then
        for_all_strict p xs
      else begin
        ignore (for_all_strict p xs) ;
        false
      end

let exists_exists p xss =
  List.exists (fun xs -> List.exists p xs) xss

let nsplit n xs =
  let rec combine xs yss = match yss with
  | [] -> xs,yss
  | ys::ry-> match xs with
    | [] -> xs,yss
    | x::xs ->
        let xs,ry = combine xs ry in
        xs,(x::ys)::ry in
  let rec do_rec xs yss = match xs with
  | [] -> yss
  | _::_ ->
      let xs,yss = combine xs yss in
      do_rec xs yss in
  let yss = do_rec xs (replicate n []) in
  List.map List.rev yss

(* Bool's *)

let (|||) p1 p2 = fun e -> p1 e || p2 e

let (&&&) p1 p2 = fun e -> p1 e && p2 e

(* Array *)

let array_map2 f xs ys =
  let lx = Array.length xs and ly = Array.length ys in
  if lx <> ly then raise (Invalid_argument "Misc.array_map2") ;
  let zs = Array.make  lx (f xs.(0) ys.(0)) in
  for k = 0 to lx-1 do
    zs.(k) <- f xs.(k) ys.(k)
  done ;
  zs

(* strings *)

let rec split_rec buff k =
  try
    let i = String.index_from buff k ',' in
    String.sub buff k (i-k)::split_rec buff (i+1)
  with
  | Not_found ->
      if k >= String.length buff then []
      else [String.sub buff k (String.length buff - k)]

let split_comma buff = split_rec buff 0

(************)
(* Matrices *)
(************)

(* Transposition *)
exception TransposeFailure

let transpose rows = match rows with
| [] -> raise TransposeFailure
| xs::_ ->
    let cols = List.rev_map (fun _ -> []) xs in
    let cols =
      try
        List.fold_right (List.map2 cons) rows cols
     with Invalid_argument _ -> raise TransposeFailure in
    cols

(* Code pretty print *)

let rec iter_by_line f prog =
  let heads,prog =
    List.split
      (List.map
         (fun xs -> match xs with
         | x::xs -> Some x,xs
             | [] ->None,[])
         prog) in
  if not (List.for_all is_none heads) then begin
    f heads ;
    iter_by_line f prog
  end

let fmt_cell sz s = sprintf " %-*s " sz s

let fmt_line szs line =
  String.concat "|"
    (List.map2
       (fun sz io -> match io with
       | Some i -> fmt_cell sz i
       | None -> fmt_cell sz "")
       szs line)

let compute_sizes m =
  List.map
    (fun cs ->
      List.fold_right
        (fun i k -> max (String.length i) k)
        cs 0)
    m

let pp_prog chan m =
  let szs = compute_sizes m in
  iter_by_line
    (fun line -> fprintf chan "%s;\n" (fmt_line szs line))
    m

(* Code fmt *)

let string_of_prog m =
  let buff = Buffer.create 128 in
  let szs = compute_sizes m in
  iter_by_line
    (fun line -> bprintf buff "%s;\n" (fmt_line szs line))
    m ;
  Buffer.contents buff

let lines_of_prog m =
  let szs = compute_sizes m in
  let r = ref [] in
  iter_by_line
    (fun line -> r := fmt_line szs line :: !r)
    m ;
  List.rev !r

let dump_symbolic s = "%" ^ s

(******)
(* IO *)
(******)

let output_protect_close c f out =
  let y = try f out with e -> c out ; raise e in
  c out ;
  y

let output_protect_gen o f name =
  let chan =
    try o name
    with Sys_error msg ->
      eprintf "open_out failed: %s\n" msg ; flush stderr ;
      try open_out "/dev/null" with Sys_error _ -> assert false in
  let y =
    try f chan with e -> close_out chan ; raise e in
  close_out chan ;
  y

let input_protect_gen o f name =
  let chan =
    try o name
    with Sys_error msg ->
      raise
        (Fatal
           (sprintf "open_in failed: %s" msg)) in
  let y =
    try f chan with e -> close_in chan ; raise e in
  close_in chan ;
  y

let output_protect f name = output_protect_gen open_out f name
let input_protect f name = input_protect_gen open_in f name



(**************************)
(* Reading list of inputs *)
(**************************)

let input_line chan =
  try Some (input_line chan)
  with
  | End_of_file -> None
  | Sys_error msg -> raise (Fatal msg)

let is_list name =
  let base = Filename.basename name in
  String.length base > 0 && base.[0] = '@'

let fconcat dir name = match dir with
| "." -> name
| _ ->
    if Filename.is_relative name then
      Filename.concat dir name
    else
      name

let ignore_line line =
  String.length line = 0 ||
  (String.length line > 0 && line.[0] = '#')

let rec input_lines  f dir k chan =
  let rec do_rec k =
    match input_line chan with
    | Some base ->
        if ignore_line base then
          do_rec k
        else
          let name = fconcat dir base in
          do_rec (read_filenames f name k)
    | None -> k in
  do_rec k

and read_filenames f name k =
  if is_list name then
    let dir = Filename.dirname name in
    input_protect (input_lines f dir k) name
  else f name k

let fold_stdin f k = input_lines f "." k stdin

let rec fold_argv f names k = match names with
| [] -> k
| name::names -> read_filenames f name (fold_argv f names k)

let iter_argv f names =
  let f x () = f x in
  fold_argv f names ()

let iter_stdin f =
 let f x () = f x in
 input_lines f "." () stdin

(* Defaults to stdin when no argument *)
let fold_argv_or_stdin f tests k = match tests with
| [] -> fold_stdin f k
| _::_ -> fold_argv f tests k

let iter_argv_or_stdin f tests  = match tests with
| [] -> iter_stdin f
| _::_ -> iter_argv f tests


(* Continue *)
let expand_argv names =
  let fs = fold_argv (fun x xs -> x::xs) names [] in
  List.rev fs

(* With iterator *)
type dir_name = string

type iter = ((dir_name * string) * in_channel) list * string list

(*
let pp_iter (chans,ns) =
  sprintf "{chans=[%s], ns=[%s]}"
    (String.concat "; " (List.map (fun ((_,n),_) -> n) chans))
    (String.concat "; " ns)
*)

let mk_iter names = [], names

let clean_iter (chans,_) =
  List.iter (fun (_,chan) -> close_in chan) chans ;
  [],[]

let open_arobase name =
  let dir = Filename.dirname name in
  try
    let chan =
      try open_in name
      with Sys_error msg ->
        eprintf "Warning: %s\n%!" msg ;
        raise Exit in
    Some ((dir,name),chan)
  with Exit -> None

let rec next_iter st =
  match st with
  | [],[] -> None
  | [],n::ns -> go "." n ([],ns)
  | ((dir,name),chan)::chans,ns ->
      begin try
        begin match input_line chan with
        | Some line -> go dir line st
        | None ->
            close_in chan ;
            next_iter (chans,ns)
        end
      with Fatal msg -> raise (Fatal (sprintf "File %s: %s\n" name msg))
      end

and go dir line (chans,ns as st) =
  if ignore_line line then next_iter st
  else if is_list line then
    match open_arobase (fconcat dir line) with
    | Some p -> next_iter (p::chans,ns)
    | None -> next_iter st
  else Some (fconcat dir line,st)

(***************************)
(* cross product iteration *)
(***************************)

let fold_cross_gen add start xss kont r =
 let rec fold_rec r ys xss = match xss with
  | [] -> kont ys r
  | xs::xss ->
      List.fold_left
        (fun r x -> fold_rec r (add x ys) xss)
        r xs in
 fold_rec r start (List.rev xss)


let fold_cross xss = fold_cross_gen cons [] xss

(*******************)
(* Simple bindings *)
(*******************)

(* Simple operations on string elements (avoid polymorphic compare) *)

module Simple = struct

  type 'a bds = (string * 'a) list

  let assoc (k:string) env =
    let rec find_rec = function
      | [] -> raise Not_found
      | (k0,v)::env ->
          if k0 = k then v else find_rec env in
    find_rec env

  let mem (y:string) xs = List.exists (fun x -> x=y) xs

  let mem_assoc (k:string) env =
    try ignore (assoc k env) ; true
    with Not_found -> false

end

(*************)
(* Test name *)
(*************)

let clean_name n =
  if Filename.check_suffix n ".litmus"  then
    Filename.chop_suffix n ".litmus"
  else
    n

(*************)
(* Test name *)
(*************)

let add_atag = sprintf "%s.atag"
and check_atag s = Filename.check_suffix s ".atag"
