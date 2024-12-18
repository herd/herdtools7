(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2016-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)


(* RCU translation *)

open Printf

let prog = if Array.length Sys.argv > 0 then Sys.argv.(0) else "mrcu"

module Top
    (O:
       sig
         val verbose : int
         val outputdir : string option
         val outall : string -> unit
         val force : bool
       end) =
  struct

    module LISA = BellBase

    module LISALexParse = struct
      type instruction = LISA.parsedPseudo
      type token = LISAParser.token

      module L = BellLexer.Make(Splitter.Default)
      let lexer = L.token
      let parser = LISAParser.main
    end

    module Dec = struct let hexa = false end
    module P = GenParser.Make(GenParser.DefaultConfig)(LISA)(LISALexParse)
    module A = ArchExtra_tools.Make(Dec)(LISA)(PteVal.No)(AddrReg.No)
    module Alloc = SymbReg.Make(A)

    module D = Dumper.Make(A)

    module RegAlloc = struct

      let all_regs = A.RegSet.of_list  A.allowed_for_symb
      module Collect = CollectRegs.Make(A)

      let create t =
        let m0 = Collect.collect t in
        A.ProcMap.map
          (fun rs -> A.RegSet.diff all_regs rs)
          m0

      let alloc p m =
        let rs = A.ProcMap.safe_find all_regs p m in
        let r =
          try A.RegSet.min_elt rs
          with Not_found -> Warn.fatal "Not enough registers" in
        r,A.ProcMap.add p (A.RegSet.remove r rs) m
    end

    module F = struct
      open LISA

(* Reduce critical sections to useful ones *)
      let rec opt d p = match p with
        | Macro _|Symbolic _| Align _ -> assert false
        | Label (lbl,p) ->
            let d,p = opt d p  in
            d,Label (lbl,p)
        | Nop -> d,Nop
        | Instruction i ->
            begin match i with
            |  Pfence (Fence (["rcu_read_unlock"],None)) ->
                if d <= 0 then Warn.user_error "extra unlock"
                else if d = 1 then 0,p
                else d-1,Nop
            |  Pfence (Fence (["rcu_read_lock"],None)) ->
                if d > 0 then d+1,Nop
                else 1,p
            | _ -> d,p
            end


      let rec do_opt_code  d = function
        | [] -> d,[]
        | p::k ->
            let d,p = opt d p in
            begin match p with
            | Nop -> do_opt_code d k
            | _   ->
                let d,k = do_opt_code d k in
                d,p::k
            end

      let opt_code cs =
        let d,cs = do_opt_code 0 cs in
        if d > 0 then  Warn.user_error "extra lock"
        else cs

      let opt_test t =
        let open MiscParser in
        { t with prog = List.map (fun (i,cs) -> i,opt_code cs) t.prog; }

(* Count sync and unlock *)
      let count (syncs,unlocks as c) = function
        | Pcall "sync"|Pfence (Fence (["sync"],None)) -> syncs+1,unlocks
        | Pfence (Fence (["rcu_read_unlock"],None)) -> syncs,unlocks+1
        | _ -> c

      let count_test t =
        List.fold_right
          (fun (_,cs) k ->
            List.fold_right
              (fun c k -> pseudo_fold count k c)
              cs k)
          t.MiscParser.prog (0,0)


(* Big work: translate call[sync], f[lock] and f[unlock]  *)
      let sync_var i = sprintf "S%i" i
      let cs_var i = sprintf "C%i" i

      let fence = Pfence (Fence (["mb"],None))

      let store_release x v =
        Pst  (Addr_op_atom (Abs x),Imm v,["release"])

      let read_acquire r x =
        Pld (r,Addr_op_atom (Abs x),["acquire"])

      let set r = Pmov (r,RAI (IAR_imm 1))

      let cons_ins i k= Instruction i::k

      let tr n m u v s c id =
        let rec do_tr (idx_sync,idx_unlock,free as st) p = match p with
        | Macro _|Symbolic _| Align _ -> assert false
        | Nop -> st,[Nop]
        | Label (lbl,p) ->
            let st,ps = do_tr st p in
            begin match ps with
            | [] ->  st,[Label (lbl,Nop); ]
            | p::ps -> st,Label (lbl,p)::ps
            end
        | Instruction i ->
            begin match i with
            | Pcall "sync"
            | Pfence (Fence (["sync"],None)) ->
                let k = cons_ins fence [] in

                let rec add_reads free i k =
                  if i >= m then free,k
                  else
                    let r,free = RegAlloc.alloc id free in
                    v.(i).(idx_sync) <- (id,r) ;
                    let free,k = add_reads free (i+1) k in
                    free,cons_ins (read_acquire r (cs_var i)) k in

                let free,k = add_reads free 0 k in
                let r,free =  RegAlloc.alloc id free in
                s.(idx_sync) <- (id,r) ;
                let k = cons_ins fence k in
                let k = cons_ins (set r) k in
                let k = cons_ins (store_release (sync_var idx_sync) 1) k in
                let k = cons_ins fence k in
                (idx_sync+1,idx_unlock,free),k
            |  Pfence (Fence (["rcu_read_lock"],None)) ->
                let rec add_reads free j k =
                  if j >= n then free,k
                  else
                    let r,free = RegAlloc.alloc id free in
                    u.(j).(idx_unlock) <- (id,r) ;
                    let free,k = add_reads free (j+1) k in
                    free,cons_ins  (read_acquire r (sync_var j)) k in
                let free,ps = add_reads free 0 [] in
                (idx_sync,idx_unlock,free),ps
            |  Pfence (Fence (["rcu_read_unlock"],None)) ->
                let r,free =  RegAlloc.alloc id free in
                c.(idx_unlock) <- (id,r) ;
                (idx_sync,idx_unlock+1,free),
                cons_ins
                  (store_release  (cs_var idx_unlock) 1)
                  (cons_ins (set r) [])
            | _ -> st,[p]
            end

        and tr_code st ps = match ps with
        | [] -> st,[]
        | p::ps ->
            let st,p = do_tr st p in
            let st,ps = tr_code st ps in
            st,p@ps in

        tr_code

      let zero = A.zero
      let one = A.one

      let tr_test n m u v s c free =
        let rec tr_rec st = function
          | [] -> []
          | ((p,_,f) as id,ps)::rem ->
              assert (f = MiscParser.Main) ;
              let st,ps = tr n m u v s c p st ps in
              (id,ps)::tr_rec st rem in
        fun t ->
          let open MiscParser in
          let prog = tr_rec (0,0,free) t.prog in

          let open ConstrGen in
          let rec loop_i i k =
            if i >= m then k
            else
              let rec loop_j j k =
                if j >= n then k
                else
                  let id1,r1 = v.(i).(j) and id2,r2 = u.(j).(i) in
                  let id3,r3 = s.(j) and id4,r4 = c.(i) in
                  Or
                    [
                     Atom (LV (Loc (A.Location_reg (id1,r1)),one));
                     Atom (LV (Loc (A.Location_reg (id2,r2)),one));
                     Atom (LV (Loc (A.Location_reg (id3,r3)),zero));
                     Atom (LV (Loc (A.Location_reg (id4,r4)),zero));
                   ]::loop_j (j+1) k in
              loop_j 0 (loop_i (i+1) k) in
          let filter = Some (ConstrGen.And (loop_i 0 []) ) in
          { t with prog; filter; }


      let noreg = -1,LISA.GPRreg (-1)

      let dump = match O.outputdir with
      | None -> D.dump_info stdout
      | Some d ->
          fun name t ->
            let fname = name.Name.file in
            let base = Filename.basename fname in
            let fname = Filename.concat d base in
            Misc.output_protect
              (fun chan -> D.dump_info chan name t)
              fname ;
            O.outall base

      let zyva name t =
        try
          let t = opt_test t in
          let n,m = count_test t in
          if O.verbose > 0 then begin
            eprintf "%s: nsyncs=%i, nunlock=%i\n" name.Name.name n m ;
          end ;
 (* silent fail if no real rcu *)
          if O.force then begin
            if (n = 0 && m = 0) then raise Misc.Exit
          end else begin
            if (n = 0 || m = 0) then raise Misc.Exit
          end ;
          let u = Array.make_matrix n m noreg
          and v = Array.make_matrix m n noreg
          and s = Array.make n noreg
          and c = Array.make m noreg
          and free = RegAlloc.create t in
          let t = tr_test n m u v s c free t in
          dump name t
        with Misc.Fatal msg|Misc.UserError msg ->
          eprintf "Adios: %s\n" msg ;
          D.dump_info stderr name t ;
          raise Misc.Exit
    end

let from_chan chan splitted =  match splitted.Splitter.arch with
| `LISA ->
    let name = splitted.Splitter.name in
    let parsed = P.parse chan splitted in
    let parsed = Alloc.allocate_regs parsed in
    F.zyva name parsed
| arch -> Warn.user_error "Bad arch for %s: %s" prog (Archs.pp arch)



module SP = Splitter.Make(Splitter.Default)

let from_file fname =
  Misc.input_protect
    (fun chan ->
      let (splitted:Splitter.result) = SP.split fname chan in
      from_chan chan splitted) fname
end

let verbose = ref 0
let outputdir = ref None
let force = ref false
let args = ref []

let opts =
  [
   "-v",Arg.Unit (fun () -> incr verbose), " be verbose";
    "-o", Arg.String (fun s -> outputdir := Some s),
   "<name>  all output in directory <name>";
   "-force", Arg.Bool (fun b -> force := b),
   sprintf "<bool> force translation, default %b" !force;
  ]

let () =
  Arg.parse opts
    (fun s -> args := !args @ [s])
    (sprintf "Usage: %s [options]* [test]*" prog)


let allchan = match !outputdir with
| None -> None
| Some d -> Some (open_out (Filename.concat d "@all"))

module X =
  Top
    (struct
      let verbose = !verbose
      let outputdir = !outputdir
      let outall = match allchan with
      | None -> fun _ -> ()
      | Some chan -> Printf.fprintf chan "%s\n"
      let force = !force
    end)


let () =
  Misc.iter_argv_or_stdin
    (fun fname ->
      try X.from_file fname with
      | Misc.Exit -> ()
      | Misc.Fatal msg|Misc.UserError msg ->
          Warn.warn_always "%a %s" Pos.pp_pos0 fname msg ;
          ()
      | e ->
          Printf.eprintf "\nFatal: %a Adios\n" Pos.pp_pos0 fname ;
          raise e)
    !args ;
  match allchan with
  | None -> ()
  | Some chan -> close_out chan
