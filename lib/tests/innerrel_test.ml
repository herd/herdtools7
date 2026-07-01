(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2026-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

module R = InnerRel.Make(Int)

let g = R.of_list [(1,2); (2,3);]

let nodes = R.Elts.of_list [1;2;]

let () =
  (* Fixed by PR #1725 *)
  try
    assert
      (R.all_topos_kont nodes g (fun _ n -> n+1) 0 = 1)
  with R.Cyclic -> assert false

(**********************)
(* Transitive closure *)
(**********************)

let to_list m = R.fold (fun p k -> p::k) m []

let pp_i fmt m =
  let open Format in
  fprintf fmt "@[%a@]"
    (pp_print_list ~pp_sep:pp_print_space
      (fun fmt (i1, i2) -> fprintf fmt "@[%d<-%d@]" i1 i2))
    (to_list m)

let simple_tr g =
  let rec loop m =
    let m2 = R.union m @@ R.sequence g m in
    if R.subrel m2 m then m2
    else loop m2 in
  loop g

let log_tr =
  let rec loop m d =
    let d2 = R.sequence d d in
    let m2 = R.union (R.union m d2) (R.sequence m d2) in
    if R.subrel m2 m then m2 else loop m2 d2 in
  fun g -> loop g g

let check k i =
  let t0 = Sys.time () in
  let ic = simple_tr i in
  let t1 = Sys.time () in
  let il = log_tr i in
  let t2 = Sys.time () in
  let it = R.transitive_closure i in
  let t3 = Sys.time () in
  let d1 = t1 -. t0 and d2 = t2 -. t1
  and d3 = t3 -. t2 in
  if d1 > 0.1 then begin
    Printf.eprintf "NAIVE=%f, LOG=%f, OPT=%f\n%!" d1 d2 d3 ;
    flush stderr
  end ;
  let ok = R.equal it ic && R.equal il ic in
  if not ok then
    Format.eprintf "@[<hv>%a@ gave with trajan@ %a@ and with log@ %a@]@."
      pp_i i pp_i it pp_i il ;
  k && ok

let check_manual k edges = check k (R.of_list edges)

let () =
  assert
    begin
      List.fold_left check_manual true
        [[(0, 0);];
         [(0, 1);];
         [(1, 1);];
         [(0, 1); (1, 0)];
         [(0, 1); (1, 1)];
         [(0, 1); (1, 2); (2, 1)];
         [(4, 5); (1, 4); (5, 1); (0, 4)];
         [(1, 0); (1, 2); (2, 1)];
         [(0, 1); (1, 0); (1, 2); (2, 1)];
         [(0,1); (1,2); (2,3); (3,4); (4,5); (5,0);];
         [(0,1); (0,2); (1,6); (1,3); (1,4); (2,5); (2,6);];
         [(0,1); (0,2); (1,3); (1,4); (2,5); (2,6); (3,3); (1,1);];
        ]
    end

let rand_graph p n l =
  let sub x y =
    let d = x-y in
    if d < 0 then d+n
    else d
  and inc x = (x+1) mod n in

  let rec do_rec g i =
    if i < 0 then g
    else
      let rec do2 g c j =
        if c < 0 then g
        else
          let g =
            if Random.float 1.0 < p  then R.add (i,j) g
            else g in
          do2 g (c-1) (inc j) in
      do_rec (do2 g (2*l+1) (sub i l)) (i-1) in
  do_rec R.empty (n-1)

let sz = 64
and p = 0.5
and l = 5
and n = 10

let gs =
  let rec do_rec k gs =
    if k <= 0 then gs
    else do_rec (k-1) (rand_graph p sz l::gs) in
  do_rec n []

let  () = assert (List.fold_left check true gs)

let () = R.check_bijection @@ R.of_list []
let () = R.check_bijection @@ R.of_list [ (1, 1) ]
let () = R.check_bijection @@ R.of_list [ (1, 2); (3, 4); (5, 6); ]
let () = R.check_bijection @@ R.of_list [ (1, 2); (2, 3); (3, 4); (4, 1) ]

let () =
  try
    R.check_bijection @@ R.of_list [ (1, 2); (1, 3); ];
    assert false;
  with
  | R.NonAffine _ -> ()
  | R.NonInjective _ -> assert false

let () =
  try
    R.check_bijection @@ R.of_list [ (2, 3); (1, 3); ];
    assert false;
  with
  | R.NonAffine _ -> assert false
  | R.NonInjective _ -> ()

