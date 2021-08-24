(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2014-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)


(* Systematic allocation of test threads according to machine topology *)

module type Config = sig
  val verbose : int
  val file_name : string
  val nthreads : int
  val avail : int
  val smt : int
  val nsockets : int
  val smtmode : Smt.t
  val mode : Mode.t
  val is_active : bool
  val inlined : bool
end

let active_tag (proc,a) = Printf.sprintf "act_%i_%s"  proc a

type sz = { scanline : int; scansz  : int; }

module Make(Cfg:Config) (O:Indent.S) : sig
  val dump_alloc : string list list -> sz
end = struct
  open Cfg
  open Printf

let () =
  if verbose > 0 && smtmode = Smt.No then
    Warn.warn_always
      "%s smtmode not specified, defaulting to static thread allocation"
      (Pos.str_pos0 file_name)

(* Get reasonable default values when topology is not specified *)
  let set_ifnone def x =  match smtmode with
    | Smt.No -> def
    | Smt.Seq|Smt.End -> x

  let smt = set_ifnone 1 smt
  let nsockets = set_ifnone 1 nsockets
  let ncores = avail / smt
  let cores_in_sock = ncores / nsockets
  let ninst =  avail / nthreads

  let pp_t pp_elt t =
    let pp = List.map (fun e -> sprintf "%s" (pp_elt e)) (Array.to_list t) in
    "{" ^ String.concat ", " pp ^"}"

  let topo =
    Array.init nsockets
      (fun s ->
        Array.init cores_in_sock
          (fun i ->
            Array.init smt
              (match smtmode with
              | Smt.Seq|Smt.No ->  fun j -> cores_in_sock * smt * s + smt*i + j
              | Smt.End ->  fun j -> i + cores_in_sock * s + ncores * j)))



  let pp_ints = pp_t (sprintf "%i")
  let pp_intss = pp_t pp_ints
  let pp_intsss = pp_t pp_intss


  let procs = Misc.interval 0 nthreads

(*
  let pp_ints xs =
    sprintf "[%s]" (String.concat "," (List.map (sprintf "%i") xs))

  let pp_groups chan gs =
    fprintf chan "%s" (String.concat " " (List.map pp_ints gs))

*)

  let pp_xs pp_elt xs =
    sprintf "[%s]"
      (String.concat "," (List.map pp_elt xs))

  let pp_g xs = pp_xs (sprintf "%i") xs

  let pp_gs gs = pp_xs pp_g gs

  let pp_gss gss = String.concat "," (List.map pp_gs gss)

  let sort_gs gs =
    List.sort
      (fun g1 g2 -> match compare (List.length g2) (List.length g1) with
      | 0 -> compare g1 g2
      | r -> r)
      gs


  let norm_gss gss =
    let gss = List.map (List.map (List.sort compare)) gss in
    let gss = List.map sort_gs gss in
    let gss = sort_gs gss in
    gss

(* Core related info *)
  type info = { sock:int ; next:int; ids:int array}

(* Initial 'next' array, indexed by core_id *)
  let alloc_next()  =
    let m = ref IntMap.empty in
    let core_id = ref 0 in
    for s = 0 to nsockets-1 do
      for c = 0 to cores_in_sock-1 do
        let i = { sock = s; next=0; ids=topo.(s).(c) } in
        m := IntMap.add !core_id i !m ;
        incr core_id
      done
    done ;
    !m

  let find_next next k =
    try IntMap.find k next
    with Not_found -> assert false

  let () = Random.init 0 (* Reproducible results.. *)

  let find_core next ok sz =
    let rec find_rec found nfound k =
      if k >= ncores then found
      else
        let i = IntMap.find k next in
        if ok k i && i.next + sz  <= smt
        then begin
          let nfound = nfound+1 in
          find_rec
            (if Random.int nfound = 0 then Some k else found) nfound (k+1)
        end else find_rec found nfound (k+1) in
    find_rec None 0 0

  let find_cores next ok0 gs = match gs with
  | [] -> assert false
  | g::gs ->
      begin match find_core next (fun _ i -> ok0 i.sock) (List.length g) with
      | None -> raise Exit
      | Some c ->
          let rec find_rec ok = function
            | [] -> []
            | g::gs ->
                begin match find_core next ok (List.length g) with
                | Some c ->
                    c::find_rec (fun k i -> k <> c && ok k i) gs
                | None -> raise Exit
                end in
          let sock = (find_next next c).sock in
          sock,c::find_rec (fun k i ->  k <> c && i.sock = sock) gs
      end

  let rec find_all next ok gss = match gss with
  | [] -> []
  | gs::gss ->
      let sock,cs = find_cores next ok gs in
      List.combine gs cs @ find_all next (fun s -> s <> sock && ok s) gss

(*
  let add_next = match Cfg.mode with
  | Mode.Std -> fun n xs  -> n+List.length xs
  | Mode.PreSi -> fun _ _ ->  Cfg.smt
*)

  let add_next n xs =  n+List.length xs

  let alloc_instance next ok gss =
    let rec alloc_rec next = function
      | [] -> next,[]
      | (xs,c)::rem ->
          let i = find_next next c in
          let next,ys =
            alloc_rec
              (IntMap.add c { i with next = add_next i.next xs} next)
              rem in
          let xs =
            List.mapi
              (fun k r -> r,i.ids.(i.next+k))
              xs in
          next,xs@ys in
    alloc_rec next (find_all next ok gss)

  let alloc_instance_hard next gss =
    try alloc_instance next (fun _ -> true) gss
    with Exit -> try
      alloc_instance next (fun _ -> true) gss
    with Exit ->
      let rec do_rec s =
        if s >= nsockets then raise Exit
        else try
          let r = alloc_instance next (fun sock -> s=sock) gss in
          r
        with Exit -> do_rec (s+1) in
      do_rec 0

  let rec cmp_length xs ys = match xs,ys with
  | _::_,[] -> -1
  | [],_::_ -> 1
  | [],[] -> 0
  | _::xs,_::ys -> cmp_length xs ys

  let sort_length gss =
    let gss = List.sort cmp_length gss in
    List.map (List.sort cmp_length) gss

  let shuffle_xs xs = match xs with
    | []|[_] -> xs
    | _ ->
        let t = Array.of_list xs in
        let len = Array.length t in
        for k=0 to len-1 do
          let j = k + Random.int (len-k) in
          let x = t.(k) in
          t.(k) <- t.(j) ; t.(j) <- x
        done ;
        Array.to_list t

  let shuffle_gss gss =
    List.map (List.map shuffle_xs) gss

  let pp_line gs xs =
    O.f "// %s" (pp_gss gs) ;
    O.o (String.concat " " (List.map (sprintf "%i,") xs))

  let std_kont k gss cpu =
    pp_line gss cpu ;
    gss::k

  let presi_kont (k1,k2) gss cpu = (gss::k1,cpu::k2)

  let alloc_instances kont k gss =
    let gss = norm_gss gss in
    let rec alloc_rec next gss k =
      if k >= ninst then []
      else try
        let next,inst = alloc_instance_hard next gss in
        if verbose > 2 then eprintf "Instance: %s\n"
          (String.concat ","
             (List.map (fun (i,j) -> sprintf "%i-%i" i j) inst)) ;
        inst::alloc_rec next (shuffle_gss gss) (k+1)
      with Exit ->
        if verbose > 2 then eprintf "Failure, instance %i\n" k ;
        [] in
    let r = alloc_rec (alloc_next ()) (sort_length gss) 0 in
    let cpu = Array.make (ninst*nthreads) (-1) in
    List.iteri
      (fun i ps ->
        List.iter
          (fun (r,id) -> cpu.(i*nthreads+r) <- id) ps)
      r ;
    kont k gss (Array.to_list cpu)


(* maxelt  : maximum cardinal of an subset in partition
   maxpart : maximum cardinal of a partition *)

let part pp_part maxelt maxpart k r =
  let rec p_rec r cp p = function
    | [] ->
        if verbose > 1 then eprintf "%s\n" (pp_part p) ;
        k r p
    | x::xs ->
        let r =
          if cp < maxpart then
            p_rec r (cp+1) ([x]::p) xs
          else r in
        let rec do_rec r prev = function
          | [] -> r
          | y::ys ->
              let r =
                let y = x::y in
                if List.length y <= maxelt then
                  p_rec r cp (y::prev@ys) xs
                else r in
              do_rec r (y::prev) ys in
        do_rec r [] p in
  function
    | [] -> assert false
    | x::xs -> p_rec r 1 [[x]] xs


  let handle_groups sz all_gs =
    if inlined then
      O.o "static const char *group[] = {"
    else
      O.f "static const char *_group_%d[] = {" nthreads ;
    List.iter
      (fun g -> O.f "\"%s\"," (pp_gss g))
      all_gs ;
    O.o "};" ;
    O.o "" ;
    if not inlined then begin
      O.f "const char **group_%d = &_group_%d[0];" nthreads nthreads ;
      O.o ""
    end ;
    let scansz = List.length all_gs in
    if inlined then begin
      O.f "#define %s %i" "SCANSZ" scansz ;
      O.f "#define %s %i" "SCANLINE" sz
    end ;
      begin match Cfg.mode with
    | Mode.Std ->
        O.o "" ;
        O.o "static count_t ngroups[SCANSZ];"
    | Mode.PreSi|Mode.Kvm -> ()
    end ;
    O.o "" ;
    { scansz=scansz; scanline=sz; }

  let std_handle groups =
(* Actual virtual proc numbers *)
    O.o "static int cpu_scan[] = {" ;
    let all_gs =  groups [] procs in
    O.o "};" ;
    O.o "" ;
    handle_groups (nthreads*ninst) (List.rev all_gs)

  let handle_table name mk gss cpus =
    if inlined then
      O.f "static const int %s[] = {" name
    else
      O.f "static const int _%s_%d[] = {" name nthreads ;
    List.iter2
      (fun gs cpu ->
        let xs = mk cpu in
        pp_line gs xs)
      gss cpus ;
    O.o "};" ;
    O.o "" ;
    if not inlined then begin
      O.f "const int *%s_%d = &_%s_%d[0];"
        name nthreads name nthreads ;
      O.o ""
    end ;
    ()


  let mk_t f cpu =
    let len = avail in
    let t = Array.make len (-1) in
    List.iteri
      (fun i c -> if c >= 0 then t.(c) <- f i)
      cpu ;
    Array.to_list t

  let mk_inst = mk_t (fun i -> i / nthreads)
  and mk_role = mk_t (fun i -> i mod nthreads)

  let handle_vars vss gss =
    let role_map =
      let r = ref StringMap.empty in
      List.iteri
        (fun i vs ->
          List.iter
            (fun v ->
              let old =
                try StringMap.find v !r
                with Not_found -> IntSet.empty in
              r := StringMap.add v (IntSet.add i old) !r)
            vs)
        vss ;
      !r in

    if Cfg.is_active && List.exists (fun vs -> vs <> []) vss then begin
      O.o "#define ACTIVE 1" ;
      O.o "typedef struct {" ;
      O.fi "int %s;"
        (String.concat ","
           (List.flatten
              (List.mapi
                 (fun i vs ->
                   (List.map (fun v -> active_tag (i,v)) vs))
                 vss))) ;
      O.o "} active_t;" ;
      O.o "" ;
      O.o "static active_t active[] = {" ;
      List.iter
        (fun gs ->
          O.f "// %s" (pp_gss gs) ;
          let smt =
            List.fold_right
              (fun gs k ->
                List.fold_right
                  (fun gs k -> IntSet.of_list gs::k)
                  gs k)
              gs [] in
          let smt_map =
            List.fold_right
              (fun gs m ->
                IntSet.fold
                  (fun g m -> IntMap.add g gs m)
                  gs m)
              smt IntMap.empty in
          let acts =
            List.flatten
              (List.mapi
                 (fun i vs ->
                   let smt =
                     try IntMap.find i smt_map
                     with Not_found -> assert false in
                   List.map
                     (fun v ->
                       try
                         let roles = StringMap.find v role_map in
                         i = IntSet.min_elt (IntSet.inter roles smt)
                       with Not_found -> false)
                     vs)
                 vss) in
          O.fi "{%s},"
                (String.concat ","
                   (List.map
                      (fun b -> if b then "1" else "0")
                      acts)))
        gss ;
      O.o "};" ;
      O.o ""
    end


  let presi_handle vss groups =
    let (gss,cpus) = groups ([],[]) procs in
    let gss = List.rev gss and cpus = List.rev cpus in
    handle_table "inst" mk_inst gss cpus ;
    handle_table "role" mk_role gss cpus ;
    handle_vars vss gss ;
    handle_groups avail gss


  let dump_alloc_gen kont handle =
    O.o "/*" ;
    O.f " Topology: %s" (pp_intsss topo) ;
    O.o "*/" ;
    O.o "" ;
(* Partition according to sockets *)
    let sockets =
      part (fun x -> "SOCKET: " ^pp_gss x)
        cores_in_sock nsockets (alloc_instances kont) in
(* Partition according to smt *)
    let groups =
      part (fun x -> "SMT: " ^pp_gs x) smt ncores sockets in
(* Handle all that *)
    handle groups


  let dump_alloc vss = match Cfg.mode with
  | Mode.Std -> dump_alloc_gen std_kont std_handle
  | Mode.PreSi|Mode.Kvm -> dump_alloc_gen presi_kont (presi_handle vss)

end
