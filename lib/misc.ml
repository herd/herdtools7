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
exception Timeout
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

let filebase f = Filename.basename f |> Filename.remove_extension

(****************)
(* basic utils  *)
(****************)

let polymorphic_compare = compare

let int_compare = Int.compare
let int_eq = Int.equal
let max_int (x:int) (y:int) = if x >= y then x else y
let min_int (x:int) (y:int) = if x <= y then x else y
let string_eq = String.equal
let bool_eq = Bool.equal
let identity = Fun.id

let ing _ = ()
let ing2 _ _ = ()

let not_found () = raise Not_found

let some = Option.some
let is_none = Option.is_none
let is_some = Option.is_some
let as_some = Option.get
let proj_opt default = Option.value ~default
let seq_opt f o = Option.bind o f

let opt_list_fold =
  let ( let* ) = Option.bind and return x = Some x in
  fun f ->
    let rec fold_rec = function
      | [] -> return []
      | x::xs ->
         let* y = f x in
         let* ys = fold_rec xs in
         return (y::ys) in
    fold_rec

let app_opt = Option.map

let app_opt2 ok no parse s = match parse s with
| None -> no ()
| Some v -> ok v

let delay_parse ok parse = app_opt2 ok not_found parse

let check_opt some = Option.fold ~none:() ~some
let snd_opt p = app_opt snd p
let map_opt = app_opt
let app_opt_def none some = Option.fold ~none ~some

let pp_opt_arg pp = function
| Some x -> "," ^ pp x
| None -> ""

let rec last = function
  | [] -> assert false
  | [x] -> x
  | _::xs -> last xs

let rec pop_last x = function
  | [] -> x,[]
  | y::ys ->
      let lst,xs = pop_last y ys in
      lst,x::xs

let pop_filter_opt =
  let rec aux f acc = function
    | [] -> None, List.rev acc
    | h :: t ->
        if f h then Some h, List.rev_append acc t else aux f (h :: acc) t
  in
  fun f -> aux f []

let map_string f s =
  let b = Buffer.create (String.length s) in
  for k=0 to String.length s-1 do
    Buffer.add_string b (f s.[k])
  done ;
  Buffer.contents b

let fmt_percent s = map_string (function | '%' -> "%%" | c -> String.make 1 c) s

let skip_spaces s = map_string (function | ' ' -> "" | c -> String.make 1 c) s

let opt_compare cmp = Option.compare cmp
let opt_eq eq = Option.equal eq
let pair x y = x,y

let pair_compare cmpx cmpy (x1,y1) (x2,y2) =
  match cmpx x1 x2 with
  | 0 -> cmpy y1 y2
  | r -> r

let pair_eq eqx eqy (x1,y1) (x2,y2) =  eqx x1 x2 &&  eqy y1 y2

let rec list_compare cmp xs ys = match xs,ys with
  | [],[] -> 0
  | [],_::_ -> -1
  | _::_,[] -> 1
  | x::xs,y::ys ->
      begin match cmp x y with
      | 0 -> list_compare cmp xs ys
      | r -> r
      end

let rec list_eq eq xs ys = match xs,ys with
  | [],[] -> true
  | ([],_::_)|(_::_,[]) -> false
  | x::xs,y::ys -> eq x y && list_eq eq xs ys

let char_uppercase = Char.uppercase_ascii
let lowercase = String.lowercase_ascii
let uppercase = String.uppercase_ascii
let capitalize = String.capitalize_ascii
let uncapitalize = String.uncapitalize_ascii

let to_c_name =
  let tr c = match c with
    | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> Char.escaped c
    | _  -> "" in
  map_string tr

(* Compatibility *)
let find_opt = List.find_opt
let filter_map = List.filter_map
let split_on_char = String.split_on_char

(********************)
(* Position parsing *)
(********************)

let pos_of_string s =
  try Scanf.sscanf s "%f,%f" (fun x y -> Some (x, y))
  with _ -> None

(***************)
(* int parsing *)
(***************)

let string_as_int s = try int_of_string s with Failure _ -> assert false
let string_as_int64 s = try Int64.of_string s with Failure _ -> assert false

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
let nilp = function
  | [] -> true
  | _::_ -> false

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

let iteri = List.iteri
let mapi = List.mapi

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

let cuts sz0 =
  let rec do_rec xs = match xs with
    | [] -> 0,[]
    | [_] -> 1,[]
    | x::xs ->
       let sz,ps = do_rec xs in
       if sz >= sz0 then
         sz,List.map (fun (p,s) -> x::p,s) ps
       else
         sz+1,
         ([x],xs)::List.map (fun (p,s) -> x::p,s) ps in
  fun xs -> let _,r = do_rec xs in r

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

let rec exists_pair p xs = match xs with
| [] -> false
| x::xs -> List.exists (p x) xs || exists_pair p xs

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

let rec group same xs = match xs with
| [] -> []
| x::xs ->
    let xx,xs = List.partition (same x) xs in
    (x::xx)::group same xs

let group_iter same do_it xs =
  let xss = group same xs in
  List.iter
    (fun xs -> match xs with
    | [] -> assert false
    | x::_ -> do_it x xs)
    xss

let group_iteri same do_it xs =
  let xss = group same xs in
  List.iteri
    (fun k xs -> match xs with
    | [] -> assert false
    | x::_ -> do_it k x xs)
    xss

(* Check f yield eq results on a list, and returns the result  *)

let check_same eq f xs =
  try
    List.fold_left
      (fun prev x -> match prev with
      | None -> Some (f x)
      | Some y0 ->
          if eq y0 (f x) then prev
          else raise Exit)
      None xs
  with Exit -> None

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
let split_comma = String.split_on_char ','

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

(********************)
(* Subset generator *)
(********************)

(* Written in the same generic style as fold_cross *)
let fold_subsets_gen add start xs kont r =
  let rec fold_rec r ys = function
    | [] -> kont ys r
    | x::xs ->
       fold_rec
         (fold_rec r ys xs)
         (add x ys)
         xs in
  fold_rec r start xs

(* Contrieved way to keep set order in subsets *)
let fold_subsets xs kont r =
  let kont s = kont (s []) in
  fold_subsets_gen (fun x s k -> s (x::k)) (fun k -> k) xs kont r

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

  let assoc = List.assoc
  let assoc_opt = List.assoc_opt
  let mem = List.mem

  let mem_assoc (k:string) env =
    try ignore (assoc k env) ; true
    with Not_found -> false

  let map f env = List.map (fun (k,v) -> k,f v) env
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

let add_ctag = sprintf "%s.ctag"
and check_ctag s = Filename.check_suffix s ".ctag"
let tr_ctag s =
  assert (check_ctag s) ;
  String.sub s 0 (String.length s - 5)

let add_atag = sprintf "%s.atag"
and check_atag s = Filename.check_suffix s ".atag"

let tr_atag s =
  if check_atag s then
    Some (Filename.chop_suffix s ".atag")
  else None

let is_atag = check_atag

let is_prefix prf =
  let prf_len = String.length prf in
  fun s ->
    let len = String.length s in
    len > prf_len && string_eq prf (String.sub s 0 prf_len )

let do_tr prf =
  let prf_len = String.length prf in
    (*Printf.printf "prf: %s\n" prf;*)
  fun s ->
    let len = String.length s in
    (*Printf.printf "s: %s\n" s;*)
    if is_prefix prf s then
      let news = (String.sub s prf_len (len-prf_len)) in
      (*Printf.printf "news: %s\n" news;*)
      Some news
    else
      (*Printf.printf "notnews: %s\n" s;*)
      None

let add_pte = sprintf "pte_%s"
let tr_pte = do_tr "pte_"
let is_pte = is_prefix "pte_"
let pp_pte = sprintf "PTE(%s)"

let add_tlb = sprintf "tlb_%s"

let add_af = sprintf "af_%s"
let tr_af = do_tr "af_"

let add_db = sprintf "db_%s"
let tr_db = do_tr "db_"

let add_dbm = sprintf "dbm_%s"
let tr_dbm = do_tr "dbm_"

let add_physical s = sprintf "phy_%s" s
let tr_physical = do_tr "phy_"
let is_physical = is_prefix "phy_"
let pp_physical = sprintf "PA(%s)"

let add_valid = sprintf "valid_%s"
let add_oa = sprintf "oa_%s"

(******************)
(* Hash utilities *)
(******************)
let  mix a b c =
  let a = a-b in let a = a-c in
  let a = a lxor (c lsr 13) in
  let b = b-c in let b = b-a in
  let b = b lxor (a lsl 8) in
  let c = c-a in let c = c-b in
  let c = c lxor (b lsr 13) in
  let a = a-b in let a = a-c in
  let a = a lxor (c lsr 12) in
  let b = b-c in let b = b-a in
  let b = b lxor (a lsl 16) in
  let c = c-a in let c = c-b in
  let c = c lxor (c lsr 5) in
  let a = a-b in let a = a-c in
  let a = a lxor (c lsl 3) in
  let b = b-c in let b = b-a in
  let b = b lxor (a lsl 10) in
  let c = c-a in let c = c-b in
  let c = c lxor (c lsr  15) in
  c

(* Group by optional integer key *)

let group_by_int get_key env =
  let m =
    List.fold_left
      (fun m (loc,_ as p) ->
        match get_key loc with
        | Some proc ->
           let env_proc = IntMap.safe_find [] proc m in
           IntMap.add proc (p::env_proc) m
        | None -> m)
        IntMap.empty env in
  List.fold_right
    (fun (loc,_ as p) k ->
      match get_key loc with
      | None  -> [p]::k
      | Some _ -> k)
    env
    (List.rev
       (IntMap.fold
          (fun _ env_p k -> List.rev env_p::k)
          m []))
