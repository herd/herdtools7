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

let simple_tr =
  let rec loop m =
    let m2 = R.union m @@ R.sequence m m in
    if R.subrel m2 m then m2
    else loop m2 in
  loop


let check_manual k edges =
  let i = R.of_list edges in
  let ic = simple_tr i
  and it = R.transitive_closure i in
  let ok = R.equal it ic in
  if not ok then
    Format.eprintf "@[<hv>%a@ gave with trajan@ %a@ and with log@ %a@]@."
      pp_i i pp_i it pp_i ic ;
  k && ok

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
