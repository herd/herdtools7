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

  let get_degree = function
    | P _ -> 0
    | Seq (_,d)|And(_,d)|Or (_,d) -> d

  let get_arg = function
    | P a | Seq (a,_) | And (a,_) | Or (a,_) -> a

  let pp_arg (e1,e2) = sprintf  "(%s,%s)" e1 e2

  let pp_degree = function
    | 0 -> ""
    | d -> sprintf "<%d>" d

  let pp = function
    | P a -> pp_arg a |> sprintf "P%s"
    | Seq (a,d) -> sprintf "Seq%s%s" (pp_arg a) (pp_degree d)
    | And (a,d) -> sprintf "And%s%s" (pp_arg a) (pp_degree d)
    | Or (a,d) -> sprintf "Or%s%s" (pp_arg a) (pp_degree d)
end

module
  Make
    (O:sig val verbose : int end) = struct

  open PreCat

  let () = ignore O.verbose

  let get_arg = Symbol.get_arg
  and get_degree = Symbol.get_degree

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

(* Second argument flags number of inversions *)
  let seq_arg2 (e1,e2) (e3,e4) =
    if String.equal e2 e3 && not (String.equal e1 e4)
    then Some (e1,e4)
    else if String.equal e2 e4 && not (String.equal e1 e3)
    then Some (e1,e3)
    else if String.equal e1 e3 && not (String.equal e2 e4)
    then Some (e2,e4)
(*    else if String.equal e1 e4 && not (String.equal e2 e3)
    then Some (e2,e3) *)
    else None

  let or_arg = function
    | [] -> assert false
    | (p,t)::rem ->
        let ps,ts = List.split rem in
        let a = get_arg p in
        if
          not
            (List.for_all
               (fun p -> same_arg2 (get_arg p) a)
               ps) then error "NoParse"
        else a,(t::ts)

  let check_and p q =
    let open Symbol in
    match p,q with
    | (P _|Or _|Seq _),_
    | And _,(Seq _|Or _)
      -> true
    | And _, (P _|And _)
      -> false

  and check_seq p q =
    let open Symbol in
    match p,q with
    | (P _|Or _|And _),_
    | Seq _,(And _|Or _)
      -> true
    | Seq _, (P _|Seq _)
      -> false

  let pp_bin p op q r =
    if O.verbose > 1 then
      eprintf "%s %s %s -> %s\n%!"
        (Symbol.pp p)
        op
        (Symbol.pp q)
        (Symbol.pp r)

  let check sz k ps qs =
    List.fold_left
      (fun k p ->
         let a = get_arg p in
         List.fold_left
           (fun k q ->
              let b = get_arg q in
              if same_arg2 a b && (sz = 2 || check_and p q) then begin
                let r = Symbol.And (a,0) in
                pp_bin p "&" q r ;
                r::k
              end else
                match seq_arg2 a b with
                | None -> k
                | Some c ->
                    if sz=2 || check_seq p q then begin
                      let r = Symbol.Seq (c,0) in
                      pp_bin p ";" q r ;
                      r::k
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
      | p::ps ->
          let d = Symbol.get_degree p in
          if d < dmin then do_rec d [p] ps
          else if d > dmin then do_rec dmin rs ps
          else do_rec dmin (p::rs) ps in
    do_rec 1000 []

  let parse_and ws ps =
    let ps,ts = List.split ps in
    let ps = Array.of_list (List.map (fun p -> [p]) ps) in
    let n = Array.length ps in
    let mat = Array.make (n+1) [||] in
    mat.(1) <- ps ;
    for i = 2 to n do (* size *)
      let t = Array.make (n-i+1) [] in
      for j = 0 to n-i do (* first index *)
        for k = 1 to i-1 do
          let s1 = mat.(k).(j)
          and s2 = mat.(i-k).(j+k) in
          t.(j) <- check i t.(j) s1 s2
        done
      done ;
      mat.(i) <- t
    done ;
    match mat.(n).(0) with
    | [] -> error "NoParse"
    | [p] -> p,reconstruct p ts ws
    | _::_::_ as ps ->
        let ps = get_mins ps in
        match ps with
        | [] -> assert false
        | [p] -> p,reconstruct p ts ws
        | _ ->
            List.map Symbol.pp ps |> String.concat "," |>
            eprintf "Ambiguous: {%s}\n%!" ;
            error "Ambiguous"

  let rec parse_list ts =
    List.fold_right (fun t k -> parse t @ k) ts []

  and parse t =
    let p =
      match t with
      | Arg (Rel (_,a),_) -> [Symbol.P a,t]
      | Arg (Set _,_) -> []
      | Connect (op,_,ts,ws) -> parse_bin op ts ws in
    begin
      match p with
      | [p,_] when O.verbose > 1 ->
          eprintf "Parse\n%a" PreCat.pp_trees [t] ;
          eprintf "As %s\n" (Symbol.pp p)
      | _ -> ()
    end ;
    p

  and parse_bin op ts ws =
    try
      let ps = parse_list ts in
      match op,ps with
      | _,[] ->  []
      | Or,_::_ ->
          let a,ts = or_arg ps in
          let a1,a2 = a in
          [Symbol.Or (a,0),Connect (Or,ARel (a1,a2),ts,ws)]
      | (And|Seq),_::_ -> [parse_and ws ps]
    with Error tag -> error tag

  let zyva d =
    match d with
    | Def (op,Rel (name,_),ts,ws) ->
        begin
          match parse_bin op ts ws with
          | [p,Connect (op,ARel (a1,a2),ts,ws)] ->
              eprintf "%s -> %s\n" (pp_tag op) (Symbol.pp p) ;
              Def (op,Rel (name,(a1,a2)),ts,ws)
          | _ -> assert false
        end
    | _ -> d

end
