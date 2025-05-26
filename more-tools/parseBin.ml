(****************************************************************************)
(*                           The Diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2023-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

open Printf

(* Grammar symbols, the int argument is non-vraisemblance degree *)
module Symbol = struct
  type t =
    | P of (string * string) (* Relation *)
    | Seq of (string * string) * int
    | And of (string * string) * int
    | Or of (string * string) * int
    | S of set
  and set =
    | SP of string
    | SAnd of string
    | SOr of string

  let get_degree = function
    | P _| S _ -> 0
    | Seq (_,d)|And(_,d)|Or (_,d) -> d

  let get_set_arg = function
    | SP e|SAnd e|SOr e -> e

  let get_arg = function
    | P a|Seq (a,_) | And (a,_) | Or (a,_) -> a
    | S s ->
        let e = get_set_arg s in (e,e)

  let pp_arg (e1,e2) = sprintf  "(%s,%s)" e1 e2

  let pp_degree = function
    | 0 -> ""
    | d -> sprintf "<%d>" d

  let pp_set = function
    | SP e -> sprintf "P(%s)" e
    | SOr e -> sprintf "Or(%s)" e
    | SAnd e -> sprintf "And(%s)" e

  let pp = function
    | P a -> pp_arg a |> sprintf "P%s"
    | Seq (a,d) -> sprintf "Seq%s%s" (pp_arg a) (pp_degree d)
    | And (a,d) -> sprintf "And%s%s" (pp_arg a) (pp_degree d)
    | Or (a,d) -> sprintf "Or%s%s" (pp_arg a) (pp_degree d)
    | S s  -> sprintf "[%s]" (pp_set s)

end

module
  Make
    (O:sig val verbose : int end) = struct

  open PreCat

  let () = ignore O.verbose

  let get_arg = Symbol.get_arg
  and get_degree = Symbol.get_degree
  and is_p =
    let open Symbol in
    function
    | P _ -> true
    | Seq _|And _|Or _|S _ -> false

  exception Error of string

  let pp_error tag t =
    eprintf "Error: %s\n" tag ;
    eprintf "tree:\n%a\n" PreCat.pp_trees [t] ;
    raise Misc.Exit

  let error tag = raise (Error tag)

(* Second boolean flags inversion *)
  let same_arg2 (e1,e2) (e3,e4) =
    String.equal e1 e3 && String.equal e2 e4
    || String.equal e1 e4 && String.equal e2 e3

  let check_arg f a = match f with
    | None -> true
    | Some f -> String.equal f a

  let get_dg = get_degree
  let sum_dgs p1 p2 = get_dg p1 + get_dg p2

  (* For "And", inversion is possible for "P" only *)
  let same_arg_and f1 f2 p1 (e1,e2) p2  (e3,e4) =
    if String.equal e1 e3 && String.equal e2 e4 then begin
      if check_arg f1 e1 && check_arg f2 e2 then
        Some ((e1,e2),sum_dgs p1 p2,false,false) (* Same orientation *)
      else if check_arg f1 e2 && check_arg f2 e1 && is_p p1 && is_p p2 then
        Some ((e2,e1),2,true,true) (* Double inversion *)
      else None
    end else begin
      if String.equal e1 e4 && String.equal e2 e3 then begin
        if is_p p1 && check_arg f1 e2 && check_arg f2 e1
        then Some ((e2,e1),1+get_dg p2,true,false)
        else if is_p p2 &&  check_arg f1 e4 && check_arg f2 e3
        then  Some ((e4,e1),1+get_dg p1,false,true)
        else None
      end else None
    end

  (* Second argument flags number of inversions *)
  let seq_arg2 f1 f2 p1 (e1,e2) p2 (e3,e4) =
    if check_arg f1 e1 && check_arg f2 e4 && String.equal e2 e3 && not (String.equal e1 e4)
    then Some ((e1,e4),sum_dgs p1 p2,false,false)
    else if is_p p2 && check_arg f1 e2 && check_arg f2 e3 && String.equal e2 e4 && not (String.equal e1 e3)
    then Some ((e1,e3),1+get_dg p1,false,true)
    else if is_p p1 && check_arg f1 e2 && check_arg f2 e4 && String.equal e1 e3 && not (String.equal e2 e4)
    then Some ((e2,e4),1+get_dg p2,true,false)
    else if
      is_p p1 && is_p p2 && check_arg f1 e2 && check_arg f2 e3
      && String.equal e1 e4 && not (String.equal e2 e3)
    then Some ((e2,e3),2,true,true)
    else None

  let check_and p q =
    let open Symbol in
    match p,q with
    | (S _|P _|Or _|Seq _),_
    | And _,(Seq _|Or _)
      -> true
    | And _, (S _|P _|And _)
      -> false

  and check_seq p q =
    let open Symbol in
    match p,q with
    | (P _|S _|Or _|And _),_
    | Seq _,(And _|Or _)
      -> true
    | Seq _, (P _|S _|Seq _)
      -> false

  let pp_os = function
    | None -> "--"
    | Some e -> e

  let pp_bin f1 f2 p op q r =
    if O.verbose > 0 then
      eprintf "(%s) %s %s %s (%s) -> %s\n%!"
        (pp_os f1)
        (Symbol.pp p)
        op
        (Symbol.pp q)
        (pp_os f2)
        (Symbol.pp r)

  and pp_no_bin f1 f2 p op q =
    if O.verbose > 0 then
      eprintf "NOT: (%s) %s %s %s (%s)\n%!"
        (pp_os f1)
        (Symbol.pp p)
        op
        (Symbol.pp q)
        (pp_os f2)

  and  pp_try_bin p op q =
    if O.verbose > 0 then
      eprintf "TRY: %s %s %s\n%!"
        (Symbol.pp p)
        op
        (Symbol.pp q)

  let rec inverse_name = function
    | Name n -> Inverse n
    | Inverse n -> Name n
    | Plus n -> Plus (inverse_name n)
    | Neg n -> Neg (inverse_name n)

  let inverse i =
    if i then
      function
      | Arg (Rel(n,(e1,e2)),ws) -> Arg (Rel (inverse_name n,(e2,e1)),ws)
      | t ->
          eprintf "Inverse attempt:\n%a\n%!" pp_tree t;
          assert false
    else Misc.identity

  let mk_bin op =
    let as_list = function
      | Connect (op2,_,ts,_) when op=op2 -> ts
      | t -> [t] in
    fun (e1,e2) t u ->
      let ts = as_list t
      and us = as_list u
      and a = ARel (e1,e2) in
      Connect (op,a,ts@us,[])

  let mk_and = mk_bin And
  and mk_seq = mk_bin Seq
  and mk_or = mk_bin Or

  let check f1 f2 _sz k ps qs =
    List.fold_left
      (fun k (p,t) ->
         let a = get_arg p in
         List.fold_left
           (fun k (q,u) ->
              let b = get_arg q in
              let k =
                if check_and p q then begin
                  match same_arg_and f1 f2 p a q b with
                  | Some (a,d,i1,i2) ->
                      let r = Symbol.And (a,d) in
                      pp_bin f1 f2 p "&" q r ;
                      let v = mk_and a (inverse i1 t) (inverse i2 u) in
                      (r,v)::k
                  | None ->
                      pp_no_bin f1 f2 p "&" q ;
                      k
                end else k in
              if check_seq p q then begin
                match seq_arg2 f1 f2 p a q b with
                | Some (a,d,i1,i2) ->
                    let r = Symbol.Seq (a,d) in
                    pp_bin f1 f2 p ";" q r ;
                    let v = mk_seq a (inverse i1 t) (inverse i2 u) in
                    (r,v)::k
                | None ->
                    pp_no_bin f1 f2 p ";" q ;
                    k
              end else k)
           k qs)
      k ps

  let reconstruct p ts ws =
    let op,(a1,a2) =
      match p with
      | Symbol.Seq (a,_) -> Seq,a
      | Symbol.And (a,_) -> And,a
      | _ -> assert false in
    Connect (op,ARel (a1,a2),ts,ws)

  let get_mins =
    let rec do_rec dmin rs = function
      | [] -> rs
      | (p,_ as c)::ps ->
          let d = Symbol.get_degree p in
          if d < dmin then do_rec d [c] ps
          else if d > dmin then do_rec dmin rs ps
          else do_rec dmin (c::rs) ps in
    do_rec 1000 []

  let parse_and f1 f2 _ws ps =
    let ps = Array.of_list (List.map (fun p -> [p]) ps) in
    let n = Array.length ps in
    let mat = Array.make (n+1) [||] in
    mat.(1) <- ps ;
    for i = 2 to n do (* size *)
      let t = Array.make (n-i+1) [] in
      for j = 0 to n-i do (* first index *)
        let f2 = if i+j+1 = n then f2 else None in
        for k = 1 to i-1 do
          let s1 = mat.(k).(j)
          and s2 = mat.(i-k).(j+k) in
          let f1 = if j = 0 then f1 else None in
          t.(j) <- check f1 f2 i t.(j) s1 s2
        done
      done ;
      mat.(i) <- t
    done ;
    match mat.(n).(0) with
    | [] -> error "NoParse"
    | [p] -> p
    | _::_::_ as ps ->
        let ps = get_mins ps in
        match ps with
        | [] -> assert false
        | [p] -> p
        | _ ->
            List.map (fun (p,_) -> Symbol.pp p) ps |> String.concat "," |>
            eprintf "Ambiguous: {%s}\n%!" ;
            error "Ambiguous"

  let or_arg f1 f2 p (e1,e2) =
    if check_arg f1 e1 && check_arg f2 e2
    then Some ((e1,e2),0)
    else if is_p p &&  check_arg f1 e2 && check_arg f2 e1
    then Some ((e2,e1),1)
    else None

  let parse_or f1 f2 =
    let rec p_rec = function
      | [] -> 0,("",""),[]
      | (p,t)::ps ->
          let f,_,ts = p_rec ps in
          begin
            match or_arg f1 f2 p (get_arg p)with
            | None -> error "NoParse"
            | Some (a,d) ->
                let inv = d > 0 in
                d+f,a,(inverse inv t)::ts
          end in
    fun ps ->
      let d,(e1,e2),ts = p_rec ps in
      Symbol.Or ((e1,e2),d),
      Connect (Or,ARel (e1,e2),ts,[])

    let f_next f1 f2 p =
      match f1,f2 with
      | None,_ -> None
      | Some f1,None ->
          let a,b = get_arg p in
          if String.equal f1 a then Some b else None
      | Some _,Some _ -> f1

  let rec parse_list f1 f2 =
    let rec p_rec f = function
      | [] -> []
      | [t] -> parse f f2 t
      | t::ts ->
          begin
            match parse f None t with
            | [] -> p_rec f ts
            | [p,_ as c] ->
                let f = f_next f f2 p in
                c::p_rec f ts
            | _::_::_ -> error "Ambiguous"
          end in
    p_rec f1


  and parse f1 f2 t =
    let p =
      match t with
      | Arg (Rel (_,a),_) -> [Symbol.P a,t]
      | Arg (Set _,_) -> []
      | Connect (op,_,ts,ws) -> parse_bin f1 f2 op ts ws in
    begin
      match p with
      | [p,_] when O.verbose > 1 ->
          eprintf "Parse\n%a" PreCat.pp_trees [t] ;
          eprintf "As %s\n" (Symbol.pp p)
      | _ -> ()
    end ;
    p

  and parse_bin f1 f2 op ts ws =
    try
      let ps = parse_list f1 f2 ts in
      match op,ps with
      | _,[] ->  []
      | Or,_::_ -> [parse_or f1 f2 ps]
      | (And|Seq),_::_ -> [parse_and f1 f2 ws ps]
    with Error tag -> error tag

  let f1 = Some "E1" and f2 = Some "E2"

  let zyva d =
    match d with
    | Def (op,Rel (name,_),ts,ws) ->
        begin
          if O.verbose > 0 then
            eprintf "Input\n%a\n%!" pp_tree (Connect (op,ARel ("E1","E2"),ts,ws)) ;
          match parse_bin f1 f2 op ts ws with
          | [p,Connect (op,ARel (a1,a2),ts,ws)] ->
              eprintf "%s -> %s\n" (pp_tag op) (Symbol.pp p) ;
              Def (op,Rel (name,(a1,a2)),ts,ws)
          | _ -> assert false
        end
    | _ -> d

end
