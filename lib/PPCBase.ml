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

(** Define registers, barriers, and instructions for PPC *)

(* Who am i ? *)
let arch = Archs.ppc
let endian = Endian.Big
let base_type = CType.Base "int"

(*************)
(* Registers *)
(*************)

type ireg =
  | GPR0 | GPR1 | GPR2 | GPR3
  | GPR4 | GPR5 | GPR6 | GPR7
  | GPR8 | GPR9 | GPR10 | GPR11
  | GPR12 | GPR13 | GPR14 | GPR15
  | GPR16 | GPR17 | GPR18 | GPR19
  | GPR20 | GPR21 | GPR22 | GPR23
  | GPR24 | GPR25 | GPR26 | GPR27
  | GPR28 | GPR29 | GPR30 | GPR31

let iregs =
  [
   GPR0,"r0";  GPR1,"r1";
   GPR2,"r2";  GPR3,"r3";
   GPR4,"r4";  GPR5,"r5";
   GPR6,"r6";  GPR7,"r7";
   GPR8,"r8";  GPR9,"r9";
   GPR10,"r10";  GPR11,"r11";
   GPR12,"r12";  GPR13,"r13";
   GPR14,"r14";  GPR15,"r15";
   GPR16,"r16";  GPR17,"r17";
   GPR18,"r18";  GPR19,"r19";
   GPR20,"r20";  GPR21,"r21";
   GPR22,"r22";  GPR23,"r23";
   GPR24,"r24";  GPR25,"r25";
   GPR26,"r26";  GPR27,"r27";
   GPR28,"r28";  GPR29,"r29";
   GPR30,"r30";  GPR31,"r31";
 ]

type freg =
  | FPR0 | FPR1 | FPR2 | FPR3
  | FPR4 | FPR5 | FPR6 | FPR7
  | FPR8 | FPR9 | FPR10 | FPR11
  | FPR12 | FPR13 | FPR14 | FPR15
  | FPR16 | FPR17 | FPR18 | FPR19
  | FPR20 | FPR21 | FPR22 | FPR23
  | FPR24 | FPR25 | FPR26 | FPR27
  | FPR28 | FPR29 | FPR30 | FPR31

let fregs =
  [
   FPR0 , "FPR0";  FPR1 , "FPR1";
   FPR2 , "FPR2";  FPR3 , "FPR3";
   FPR4 , "FPR4";  FPR5 , "FPR5";
   FPR6 , "FPR6";  FPR7 , "FPR7";
   FPR8 , "FPR8";  FPR9 , "FPR9";
   FPR10 , "FPR10";  FPR11 , "FPR11";
   FPR12 , "FPR12";  FPR13 , "FPR13";
   FPR14 , "FPR14";  FPR15 , "FPR15";
   FPR16 , "FPR16";  FPR17 , "FPR17";
   FPR18 , "FPR18";  FPR19 , "FPR19";
   FPR20 , "FPR20";  FPR21 , "FPR21";
   FPR22 , "FPR22";  FPR23 , "FPR23";
   FPR24 , "FPR24";  FPR25 , "FPR25";
   FPR26 , "FPR26";  FPR27 , "FPR27";
   FPR28 , "FPR28";  FPR29 , "FPR29";
   FPR30 , "FPR30";  FPR31 , "FPR31";
 ]


type crbit = int
type xerbit =
  | XER_SO | XER_OV | XER_CA

type reg =
  | Ireg of ireg (* integer registers *)
  | Freg of freg (* float registers *)
  | PC (* program counter *)
  | Rc (* record bit *)
  | OE
  | XER_0 | XER_1 | XER_2
      (* bits of the condition register *)
  | CRBit of int (* i in [0..31] *)
        (* fields of the condition register *)
  | CRField  of int (* i in [0..7] *)
  | LR (* link register; for return address *)
  | CTR (* count register; used for some branches *)
  | CARRY (* carry bit of the status register *)
  | Symbolic_reg of string
(* Internal regs *)
  | Internal of int
(* Reservation (for specific dependencies *)
  | RES
  | RESADDR

let base =  Internal 0
and max_idx = Internal 1
and idx = Internal 2
and ephemeral = Internal 3
let loop_idx = Internal 4
let signal = Internal 5
let tb0 = Internal 6
let tb1 = Internal 7
let tb_addr0 = Internal 8
let tb_addr1 = Internal 9

let pc = PC

let reg_compare = compare

let symb_reg_name = function
  | Symbolic_reg s -> Some s
  | _ -> None

let symb_reg r = Symbolic_reg r

let pp_ireg r =
  try List.assoc r iregs with
  | Not_found -> assert false

let pp_freg r =
  try List.assoc r fregs
  with Not_found -> assert false

open Printf

let pp_crf crb = sprintf "cr%i" crb

let pp_reg r =
  match r with
  | Ireg(ir) -> pp_ireg ir
  | Freg(fr) -> pp_freg fr
  | CRField k -> sprintf "CR%i" k
  | CRBit k -> sprintf "CR:%i" k
  | PC -> "PC"
  | LR -> "LR"
  | CTR -> "CTR"
  | CARRY -> "CARRY"
  | Rc -> "Rc"
  | XER_0 -> "XER_0"
  | XER_1 -> "XER_1"
  | XER_2 -> "XER_2"
  | OE -> "OE"
  | Symbolic_reg r -> "%"^r
  | Internal i -> sprintf "i%i" i
  | RES -> "RES"
  | RESADDR -> "RESADDR"

let parse_list =
  List.map (fun (r,s) -> s,Ireg r) iregs @
  List.map (fun (r,s) -> s,Freg r) fregs

let parse_reg s =
  let s = Misc.lowercase s in
  try Some (List.assoc s parse_list)
  with Not_found -> None


let regs_interval =
  let iregs = List.map (fun (r,_) -> Ireg r) iregs in
  let rec from_reg r = function
    | [] -> assert false
    | t::rem as rs ->
        if  reg_compare r t = 0 then  rs
      else from_reg r rem in
  fun r -> match r with
  | Ireg _ -> from_reg r iregs
  | _ -> Warn.fatal "illegal regs_interval from %s" (pp_reg r)

let typeof c = assert false

(************)
(* Barriers *)
(************)

type barrier =
  | Sync
  | Isync
  | Lwsync
  | Eieio

let all_kinds_of_barriers =  [ (*Isync;*) Sync ; Lwsync ; Eieio; ]

let pp_barrier b =
  match b with
  | Sync -> "Sync"
  | Isync -> "Isync"
  | Lwsync -> "Lwsync"
  | Eieio -> "Eioio"

let barrier_compare = compare


(****************)
(* Instructions *)
(****************)

(*type idx = int (* limited to ? bits *)*)
(*type k = int   (* limited to ? bits *)*)
type lbl = Label.t
type cond =
  | Eq | Ne
  | Lt | Ge
  | Gt | Le

type crfindex = int (* in fact [0..7] *)

type setcr0 = SetCR0 | DontSetCR0

(* j: for arithm at least, should be ireg *)

type 'k kinstruction =
  | Pnop
(* Two forms of ins: set cr0 or not *)
  | Padd of setcr0*reg*reg*reg
  | Psub of setcr0*reg*reg*reg
  | Psubf of setcr0*reg*reg*reg
  | Por of setcr0*reg*reg*reg
  | Pand of setcr0*reg*reg*reg
  | Pxor of setcr0*reg*reg*reg
  | Pmull of setcr0*reg*reg*reg
  | Pdiv of setcr0*reg*reg*reg

(* cr0 seting is implicit... *)
  | Paddi of reg*reg*'k (* no *)
  | Pandi of reg*reg*'k (* yes *)
  | Pori of reg*reg*'k  (* no *)
  | Pxori of reg*reg*'k (* no *)
  | Pmulli of  reg*reg*'k (* no *)

  | Pli of reg*'k
  | Pb of lbl
  | Pbcc of cond * lbl
  | Pcmpwi of crfindex * reg*'k
  | Pcmpw of crfindex * reg*reg
  | Plwzu of reg * 'k * reg
  | Pmr of reg * reg
  | Pstwu of reg * 'k * reg
  | Plwarx of reg*reg*reg (* load word and reserve indexed *)
  | Pstwcx of reg*reg*reg (* store word conditional indexed *)
(* Mixed size load and store, just added at the moment *)
  | Pload of MachSize.sz * reg * 'k * reg
  | Ploadx of MachSize.sz * reg * reg * reg
  | Pstore of MachSize.sz * reg * 'k * reg
  | Pstorex of MachSize.sz * reg * reg * reg
(* Fence instructions *)
  | Psync
  | Peieio
  | Pisync
  | Plwsync
(* Extra, is a nop in memevents *)
  | Pdcbf of reg*reg
(* extra for Richard Bornat; no semantics in memevents *)
  | Pnor of setcr0*reg*reg*reg
  | Pneg of setcr0*reg*reg
  | Pslw  of setcr0*reg*reg*reg
  | Psrawi  of setcr0*reg*reg*'k
  | Psraw  of setcr0*reg*reg*reg
  | Pbl of lbl
  | Pblr
  | Pmtlr of reg
  | Pmflr of reg
(* Extra load and store multiple, litmus only *)
  | Plmw of reg * 'k * reg
  | Pstmw of reg * 'k * reg
  | Pcomment of string

type instruction = int kinstruction

type parsedInstruction = MetaConst.k kinstruction

let ppi_index_mode opcode r1 r2 r3 =
  opcode^" "^pp_reg r1 ^ ","^pp_reg r2 ^ ","^pp_reg r3

let ppi_index_mode2 opcode r1 r2 =
  opcode^" "^pp_reg r1 ^ ","^pp_reg r2

let ppi_imm_index_mode pp_idx opcode r1 d r2 =
  opcode^" "^pp_reg r1 ^ ","^pp_idx d ^ "("^pp_reg r2^")"

let ppi_imm_instr pp_k opcode r1 r2 v =
  opcode^" "^pp_reg r1 ^ ","^pp_reg r2 ^ ","^pp_k v

let ppi_imm_instr_memo pp_k opcode set r1 r2 v =
  let memo = match set with
  | SetCR0 -> opcode ^ "."
  | DontSetCR0 -> opcode in
  ppi_imm_instr pp_k memo r1 r2 v

let ppi_ri pp_k opcode rD v = opcode^" "^pp_reg rD ^ ","^pp_k v

let ppi_rr opcode rD rS = opcode^" "^pp_reg rD^","^pp_reg rS

let pp_op3 memo set rD rA rB =
  let memo = match set with
  | SetCR0 -> memo ^ "."
  | DontSetCR0 -> memo in
  ppi_index_mode memo rD rA rB

let pp_op2 memo set rD rA =
  let memo = match set with
  | SetCR0 -> memo ^ "."
  | DontSetCR0 -> memo in
  ppi_index_mode2 memo rD rA

let pp_cond cond = match cond with
| Eq -> "eq" | Ne -> "ne"
| Lt -> "lt" | Ge -> "ge"
| Gt -> "gt" | Le -> "le"

open MachSize

let memo_load = function
  | Byte -> "lbz"
  | Short -> "lhz"
  | Word -> "lwz"
  | Quad -> "ld"

let memo_loadx sz = memo_load sz ^ "x"

let memo_store = function
  | Byte -> "stb"
  | Short -> "sth"
  | Word -> "stw"
  | Quad -> "std"

let memo_storex sz = memo_store sz ^ "x"

let do_pp_instruction pp_k i = match i with
| Pnop -> "nop"
| Padd(set,rD,rA,rB) -> pp_op3 "add" set rD rA rB
| Psub(set,rD,rA,rB) -> pp_op3 "sub" set rD rA rB
| Psubf(set,rD,rA,rB) -> pp_op3 "subf" set rD rA rB
| Por(set,rD,rA,rB) -> pp_op3 "or" set rD rA rB
| Pxor(set,rD,rA,rB) -> pp_op3 "xor" set rD rA rB
| Pand(set,rD,rA,rB) -> pp_op3 "and" set rD rA rB
| Pmull(set,rD,rA,rB) -> pp_op3 "mullw" set rD rA rB
| Pdiv(set,rD,rA,rB) -> pp_op3 "divw" set rD rA rB

| Paddi(rD,rA,simm) -> ppi_imm_instr pp_k "addi" rD rA simm
| Pori(rD,rA,simm) -> ppi_imm_instr pp_k "ori" rD rA simm
| Pxori(rD,rA,simm) -> ppi_imm_instr pp_k "xori" rD rA simm
| Pandi(rD,rA,simm) -> ppi_imm_instr pp_k "andi." rD rA simm
| Pmulli(rD,rA,simm) -> ppi_imm_instr pp_k "mulli" rD rA simm

| Pli(rD,v) -> ppi_ri pp_k "li" rD v
| Pcmpwi (0,rS,v) -> ppi_ri pp_k "cmpwi" rS v
| Pcmpwi (crf,rS,v) ->
    "cmpwi" ^ " " ^pp_crf crf ^ "," ^ pp_reg rS  ^ "," ^ pp_k v
| Pb lbl -> "b   " ^ lbl
| Pbcc(cond, lbl) -> "b"^pp_cond cond ^ "  " ^ lbl
| Pcmpw(0,rA,rB) -> ppi_rr "cmpw" rA rB
| Pcmpw(crf,rA,rB) ->
    "cmpw" ^ " " ^pp_crf crf ^ "," ^ pp_reg rA  ^ "," ^ pp_reg rB
| Plwzu(rD,d,rA) -> ppi_imm_index_mode pp_k "lwzu" rD d rA
| Pmr (rD,rS) -> ppi_rr "mr" rD rS
| Pstwu(rS,d,rA) -> ppi_imm_index_mode pp_k "stwu" rS d rA
| Plwarx(rD,rA,rB) -> ppi_index_mode "lwarx" rD rA rB
| Pstwcx(rS,rA,rB) -> ppi_index_mode "stwcx." rS rA rB


| Plmw (rD,d,rA) -> ppi_imm_index_mode pp_k "lmw" rD d rA
| Pstmw (rS,d,rA) -> ppi_imm_index_mode pp_k "stmw" rS d rA

| Pload (sz,rD,d,rA) ->  ppi_imm_index_mode pp_k (memo_load sz) rD d rA
| Ploadx (sz,rD,rA,rB) ->  ppi_index_mode (memo_loadx sz) rD rA rB
| Pstore (sz,rS,d,rA) ->  ppi_imm_index_mode pp_k (memo_store sz) rS d rA
| Pstorex (sz,rS,rA,rB) ->  ppi_index_mode (memo_storex sz) rS rA rB

| Psync -> "sync"
| Plwsync -> "lwsync"
| Pisync -> "isync"
| Peieio -> "eieio"
| Pdcbf (r1,r2) -> ppi_rr "dcbf" r1 r2

| Pnor(set,rD,rA,rB) -> pp_op3 "nor" set rD rA rB
| Pneg(set,rD,rA) -> pp_op2 "neg" set rD rA
| Pslw(set,rD,rA,rB) -> pp_op3 "slw" set rD rA rB
| Psrawi(set,rD,rA,k) -> ppi_imm_instr_memo pp_k "srawi" set rD rA k
| Psraw(set,rD,rA,rB) -> pp_op3 "sraw" set rD rA rB
| Pbl lbl -> "bl   " ^ lbl
| Pblr -> "blr"
| Pmtlr reg -> "mtlr " ^ pp_reg reg
| Pmflr reg -> "mflr " ^ pp_reg reg
| Pcomment s -> "com \"" ^ s ^ "\""

let pp_instruction _m ins = do_pp_instruction string_of_int ins

let dump_instruction ins = do_pp_instruction string_of_int ins
and dump_parsedInstruction ins = do_pp_instruction MetaConst.pp  ins

(**********************)
(* Symbolic reg stuff *)
(**********************)

(*
  GPR0 can sometimes be understood as constant zero.
 *)
let allowed_for_symb =
  let regs = List.map fst iregs in
  match regs with
  | GPR0::safe -> List.map (fun r -> Ireg r) safe
  | _ -> assert false

let fold_regs (f_reg,f_sreg) =

  (* Let us have a functional style... *)
  let fold_reg reg (y_reg,y_sreg) = match reg with
  | Symbolic_reg s -> y_reg,f_sreg s y_sreg
  | Ireg _ -> f_reg reg y_reg,y_sreg
  |  _ -> y_reg, y_sreg in

  fun (y_reg,y_sreg as c) ins -> match ins with
    (* Three regs style *)
  | Padd (_,r1,r2,r3)
  | Psub (_,r1,r2,r3)
  | Psubf (_,r1,r2,r3)
  | Por (_,r1,r2,r3)
  | Pand (_,r1,r2,r3)
  | Pxor (_,r1,r2,r3)
  | Pmull (_,r1,r2,r3)
  | Pdiv (_,r1,r2,r3)
  | Plwarx (r1,r2,r3)
  | Pstwcx (r1,r2,r3)
  | Ploadx (_,r1,r2,r3)
  | Pstorex (_,r1,r2,r3)
  | Pnor (_,r1,r2,r3)
  | Pslw (_,r1,r2,r3)
  | Psraw (_,r1,r2,r3)
    -> fold_reg r3 (fold_reg r2 (fold_reg r1 (y_reg,y_sreg)))
	(* Two *)
  | Paddi (r1,r2,_)
  | Pori (r1,r2,_)
  | Pandi (r1,r2,_)
  | Pxori (r1,r2,_)
  | Pmulli (r1,r2,_)
  | Pcmpw (_,r1,r2)
  | Plwzu (r1,_,r2)
  | Pmr (r1,r2)
  | Pdcbf (r1,r2)
  | Pstwu (r1,_,r2)
  | Pload (_,r1,_,r2)
  | Pstore (_,r1,_,r2)
  | Pneg (_,r1,r2)
  | Psrawi (_,r1,r2,_)
    ->  fold_reg r2 (fold_reg r1 (y_reg,y_sreg))
	(* One *)
  | Pli (r1,_)
  | Pcmpwi (_,r1,_)
  | Pmtlr r1
  | Pmflr r1
    ->  fold_reg r1 (y_reg,y_sreg)
	(* None *)
  | Pnop
  | Pb _
  | Pbcc _
  | Psync
  | Peieio
  | Pisync
  | Plwsync
  | Pbl _
  | Pblr
  | Pcomment _
    -> c
  | Plmw (r1,_,r2)
  | Pstmw (r1,_,r2) ->
      let rs = regs_interval r1 in
      fold_reg r2 (List.fold_right fold_reg rs (y_reg,y_sreg))


(* Map over symbolic regs *)
let map_regs f_reg f_symb =

  let map_reg reg = match reg with
  | Symbolic_reg s -> f_symb s
  | Ireg _ -> f_reg reg
  | _ -> reg in

  let map3 ins r1 r2 r3 =ins (map_reg r1,map_reg r2,map_reg r3)
  and map2 ins r1 r2 = ins (map_reg r1,map_reg r2) in

  fun ins -> match ins with
(* Special, keep r1 as is *)
  | Plmw (r1,d,r2) -> Plmw (r1,d,map_reg r2)
  | Pstmw (r1,d,r2) -> Pstmw (r1,d,map_reg r2)
(* Standard, map all registers *)
  | Padd (set,r1,r2,r3) -> (* Hum ocaml constructor syntax does not help *)
      map3  (fun (r1,r2,r3) -> Padd (set,r1,r2,r3)) r1 r2 r3
  | Psub (set,r1,r2,r3) ->
      map3  (fun (r1,r2,r3) -> Psub (set,r1,r2,r3)) r1 r2 r3
  | Psubf (set,r1,r2,r3) ->
      map3  (fun (r1,r2,r3) -> Psubf (set,r1,r2,r3)) r1 r2 r3
  | Por (set,r1,r2,r3) ->
      map3  (fun (r1,r2,r3) -> Por (set,r1,r2,r3)) r1 r2 r3
  | Pand (set,r1,r2,r3) ->
      map3  (fun (r1,r2,r3) -> Pand (set,r1,r2,r3)) r1 r2 r3
  | Pxor (set,r1,r2,r3) ->
      map3 (fun (r1,r2,r3) -> Pxor (set,r1,r2,r3)) r1 r2 r3
  | Pmull (set,r1,r2,r3) ->
      map3 (fun (r1,r2,r3) -> Pmull (set,r1,r2,r3)) r1 r2 r3
  | Pdiv (set,r1,r2,r3) ->
      map3 (fun (r1,r2,r3) -> Pdiv (set,r1,r2,r3)) r1 r2 r3
  | Ploadx (sz,r1,r2,r3) ->
      map3 (fun (r1,r2,r3) -> Ploadx (sz,r1,r2,r3)) r1 r2 r3
  | Pstorex (sz,r1,r2,r3) ->
      map3 (fun (r1,r2,r3) -> Pstorex (sz,r1,r2,r3)) r1 r2 r3
  | Plwarx (r1,r2,r3) ->
      map3 (fun (r1,r2,r3) -> Plwarx (r1,r2,r3)) r1 r2 r3
  | Pstwcx (r1,r2,r3) ->
      map3 (fun (r1,r2,r3) -> Pstwcx (r1,r2,r3)) r1 r2 r3
  | Pnor (set,r1,r2,r3) ->
      map3  (fun (r1,r2,r3) -> Pnor (set,r1,r2,r3)) r1 r2 r3
  | Pslw (set,r1,r2,r3) ->
      map3  (fun (r1,r2,r3) -> Pslw (set,r1,r2,r3)) r1 r2 r3
  | Psraw (set,r1,r2,r3) ->
      map3  (fun (r1,r2,r3) -> Psraw (set,r1,r2,r3)) r1 r2 r3

(* Two *)
  | Paddi (r1,r2,v) ->
      map2 (fun (r1,r2) -> Paddi (r1,r2,v)) r1 r2
  | Pori (r1,r2,v) ->
      map2 (fun (r1,r2) -> Pori (r1,r2,v)) r1 r2
  | Pandi (r1,r2,v) ->
      map2 (fun (r1,r2) -> Pandi (r1,r2,v)) r1 r2
  | Pxori (r1,r2,v) ->
      map2 (fun (r1,r2) -> Pxori (r1,r2,v)) r1 r2
  | Pmulli (r1,r2,v) ->
      map2 (fun (r1,r2) -> Pmulli (r1,r2,v)) r1 r2
  | Pcmpw (crf,r1,r2) ->
      map2 (fun (r1,r2) -> Pcmpw (crf,r1,r2)) r1 r2
  | Pload (sz,r1,cst,r2) ->
      map2 (fun (r1,r2) -> Pload (sz,r1,cst,r2)) r1 r2
  | Plwzu (r1,cst,r2) ->
      map2 (fun (r1,r2) -> Plwzu (r1,cst,r2)) r1 r2
  | Pmr (r1,r2) ->
      map2 (fun (r1,r2) -> Pmr (r1,r2)) r1 r2
  | Pdcbf (r1,r2) ->
      map2 (fun (r1,r2) -> Pdcbf (r1,r2)) r1 r2
  | Pstore (sz,r1,cst,r2) ->
      map2 (fun (r1,r2) -> Pstore (sz,r1,cst,r2)) r1 r2
  | Pstwu (r1,cst,r2) ->
      map2 (fun (r1,r2) -> Pstwu (r1,cst,r2)) r1 r2
  | Pneg (set,r1,r2) ->
      map2 (fun (r1,r2) -> Pneg (set,r1,r2)) r1 r2
  | Psrawi (set,r1,r2,v) ->
      map2 (fun (r1,r2) -> Psrawi (set,r1,r2,v)) r1 r2

	(* One *)
  | Pli (r1,v) ->
      Pli (map_reg r1,v)
  | Pcmpwi (crf,r1,v) ->
      Pcmpwi (crf,map_reg r1,v)
  | Pmtlr r ->
      Pmtlr (map_reg r)
  | Pmflr r ->
      Pmflr (map_reg r)

	(* None *)
  | Pnop
  | Pb _
  | Pbcc _
  | Psync
  | Peieio
  | Pisync
  | Plwsync
  | Pbl _
  | Pblr
  | Pcomment _

    -> ins

(* No addresses burried in PPC code *)
let fold_addrs _f c _ins = c

let map_addrs _f ins = ins

(* Go back to 32bits mode *)

let norm_ins ins = match ins with
  | Pload(Quad,r1,cst,r2) -> Pload(Word,r1,cst,r2)
  | Ploadx(Quad,r1,r2,r3) -> Ploadx(Word,r1,r2,r3)
  | Pstore(Quad,r1,cst,r2) -> Pstore(Word,r1,cst,r2)
  | Pstorex(Quad,r1,r2,r3) -> Pstorex(Word,r1,r2,r3)
  | Pnop
  | Pload ((Byte|Short|Word),_,_,_)
  | Ploadx ((Byte|Short|Word),_,_,_)
  | Pstore ((Byte|Short|Word),_,_,_)
  | Pstorex ((Byte|Short|Word),_,_,_)
  | Pb _ | Pbcc (_,_)
  | Pdcbf (_, _)
  | Pstwcx (_, _, _)|Plwarx (_, _, _)
  | Pstwu (_,_,_)|Pmr (_, _)
  | Plwzu (_,_,_)|Pcmpw (_, _, _)
  | Pcmpwi (_, _, _)|Pli (_, _)
  | Pmulli (_, _, _)|Pxori (_, _, _)|Pori (_, _, _)
  | Pandi (_, _, _)|Paddi (_, _, _)|Pdiv (_, _, _, _)
  | Pmull (_, _, _, _)|Pxor (_, _, _, _)
  | Pand (_, _, _, _)|Por (_, _, _, _)
  | Psub (_, _, _, _)| Psubf (_, _, _, _)|Padd (_, _, _, _)
  | Plwsync|Pisync|Peieio|Psync
  | Pnor (_,_,_,_) | Pneg(_,_,_)
  | Pslw(_,_,_,_) | Psrawi(_,_,_,_) | Psraw(_,_,_,_)
  | Pbl _ | Pblr | Pmtlr _ | Pmflr _
  | Pcomment _
  | Plmw _|Pstmw _
          -> ins

let is_data r1 i = match i with
| Pstore (_,r,_,_)
| Pstorex (_,r,_,_)
| Pstwcx (r,_,_)
  -> r1 = r
| _ -> false

let get_next = function
  | Pnop
  | Padd _ | Psub (_, _, _, _)|Psubf (_, _, _, _)
  | Por (_, _, _, _)|Pand (_, _, _, _)|Pxor (_, _, _, _)
  |Pmull (_, _, _, _)|Pdiv (_, _, _, _)|Paddi (_, _, _)
  | Pandi (_, _, _)|Pori (_, _, _)|Pxori (_, _, _)|Pmulli (_, _, _)
  |Pli (_, _)
  |Pcmpwi (_, _, _)|Pcmpw (_, _, _)|Plwzu(_,_,_)
  |Pmr (_, _)|Pstwu(_,_,_)
  |Plwarx (_, _, _)|Pstwcx (_, _, _)
  |Pload _|Ploadx _|Pstore _|Pstorex _
  |Psync|Peieio|Pisync|Plwsync
  |Pdcbf (_, _)|Pnor (_, _, _, _)|Pneg (_, _, _)
  |Pslw (_, _, _, _)|Psrawi (_, _, _, _)|Psraw (_, _, _, _)
  |Plmw _|Pstmw _
  |Pcomment _ -> [Label.Next]
  |Pb lbl -> [Label.To lbl]
  |Pbcc (_, lbl) -> [Label.Next;Label.To lbl]
        (* Hum *)
  |Pbl _ -> [Label.Next]
  |Pblr|Pmtlr _|Pmflr _ -> []


(* Macros *)


include Pseudo.Make
    (struct
      type ins = instruction
      type pins = parsedInstruction
      type reg_arg = reg

      let parsed_tr = function
	| Pli (r, k) -> Pli(r,MetaConst.as_int k)
	| Pmulli (r1, r2, k) -> Pmulli(r1,r2,MetaConst.as_int k)
	| Pxori (r1, r2, k) -> Pxori(r1,r2,MetaConst.as_int k)
	| Pori (r1, r2, k) -> Pori(r1,r2,MetaConst.as_int k)
	| Pandi (r1, r2, k) -> Pandi(r1,r2,MetaConst.as_int k)
	| Paddi (r1, r2, k) -> Paddi(r1,r2,MetaConst.as_int k)
	| Pcmpwi (i, r, k) -> Pcmpwi(i,r,MetaConst.as_int k)
	| Plwzu (r1,k,r2) -> Plwzu(r1,MetaConst.as_int k,r2)
	| Pstwu (r1,k,r2) -> Pstwu(r1,MetaConst.as_int k,r2)
	| Pload (s,r1,k,r2) -> Pload(s,r1,MetaConst.as_int k,r2)
	| Pstore(s,r1,k,r2) -> Pstore(s,r1,MetaConst.as_int k,r2)
	| Psrawi(s,r1,r2,k) -> Psrawi(s,r1,r2,MetaConst.as_int k)
	| Plmw(r1,k,r2) -> Plmw(r1,MetaConst.as_int k,r2)
	| Pstmw (r1,k,r2) -> Pstmw(r1,MetaConst.as_int k,r2)
	    
        | Pnop
	| Ploadx (_,_,_,_)
	| Pstorex (_,_,_,_)
	| Pb _ | Pbcc (_,_)
	| Pdcbf (_, _)
	| Pstwcx (_, _, _)|Plwarx (_, _, _)|Pmr (_, _)
	| Pcmpw (_, _, _)|Pdiv (_, _, _, _)
	| Pmull (_, _, _, _)|Pxor (_, _, _, _)
	| Pand (_, _, _, _)|Por (_, _, _, _)
	| Psub (_, _, _, _)| Psubf (_, _, _, _)|Padd (_, _, _, _)
	| Plwsync|Pisync|Peieio|Psync
	| Pnor (_,_,_,_) | Pneg(_,_,_)
	| Pslw(_,_,_,_) | Psraw(_,_,_,_)
	| Pbl _ | Pblr | Pmtlr _ | Pmflr _
	| Pcomment _
          as same -> same

(* Number if memory accesses per instruction (for estimating test complexity) *)
      let get_naccesses = function
        | Pnop
(* Two forms of ins: set cr0 or not *)
        | Padd _
        | Psub _
        | Psubf _
        | Por _
        | Pand _
        | Pxor _
        | Pmull _
        | Pdiv _
(* cr0 seting is implicit... *)
        | Paddi _
        | Pandi _
        | Pori _
        | Pxori _
        | Pmulli _
        | Pli _
        | Pb _
        | Pbcc _
        | Pcmpwi _
        | Pcmpw _
        | Pmr _
        | Psync
        | Peieio
        | Pisync
        | Plwsync
        | Pdcbf _
        | Pnor _
        | Pneg _
        | Pslw  _
        | Psrawi  _
        | Psraw  _
        | Pbl _
        | Pblr
        | Pmtlr _
        | Pmflr _
        | Pcomment _
          -> 0
        | Plwzu _
        | Pstwu _
        | Plwarx _
        | Pstwcx _
        | Pload _|Ploadx _|Pstore _|Pstorex _
          -> 1
        |Plmw (_r1,_,_)
        |Pstmw (_r1,_,_)
            -> 2 (* could be List.length (regs_interval r1), not that important *)

      let fold_labels k f = function
        | Pb lab
        | Pbcc (_,lab) -> f k lab
        | Pnop
        | Pdcbf (_, _)
        | Pstwcx (_, _, _)|Plwarx (_, _, _)
        | Pstwu(_,_,_)|Pmr (_, _)
        | Plwzu(_,_,_)|Pcmpw (_, _, _)
        | Pcmpwi (_, _, _)|Pli (_, _)
        | Pmulli (_, _, _)|Pxori (_, _, _)|Pori (_, _, _)
        | Pandi (_, _, _)|Paddi (_, _, _)|Pdiv (_, _, _, _)
        | Pmull (_, _, _, _)|Pxor (_, _, _, _)
        | Pand (_, _, _, _)|Por (_, _, _, _)
        | Psub (_, _, _, _)| Psubf (_, _, _, _)|Padd (_, _, _, _)
        | Plwsync|Pisync|Peieio|Psync
        | Pnor (_,_,_,_) | Pneg(_,_,_)
        | Pslw(_,_,_,_) | Psrawi(_,_,_,_) | Psraw(_,_,_,_)
        | Pbl _ | Pblr | Pmtlr _ | Pmflr _
        | Pcomment _
        | Plmw _|Pstmw _
        | Pload _|Ploadx _|Pstore _|Pstorex _
          -> k




      let map_labels f = function
        | Pb lab -> Pb (f lab)
        | Pbcc (cc,lab) -> Pbcc (cc,f lab)
        | Pnop
        | Pdcbf (_, _)
        | Pstwcx (_, _, _)|Plwarx (_, _, _)
        | Pstwu(_,_,_)|Pmr (_, _)
        | Plwzu(_,_,_)|Pcmpw (_, _, _)
        | Pcmpwi (_, _, _)|Pli (_, _)
        | Pmulli (_, _, _)|Pxori (_, _, _)|Pori (_, _, _)
        | Pandi (_, _, _)|Paddi (_, _, _)|Pdiv (_, _, _, _)
        | Pmull (_, _, _, _)|Pxor (_, _, _, _)
        | Pand (_, _, _, _)|Por (_, _, _, _)
        | Psub (_, _, _, _)| Psubf (_, _, _, _)|Padd (_, _, _, _)
        | Plwsync|Pisync|Peieio|Psync
        | Pnor (_,_,_,_) | Pneg(_,_,_)
        | Pslw(_,_,_,_) | Psrawi(_,_,_,_) | Psraw(_,_,_,_)
        | Pbl _ | Pblr | Pmtlr _ | Pmflr _
        | Plmw _|Pstmw _
        | Pcomment _
        | Pload _|Ploadx _|Pstore _|Pstorex _
            as ins
          -> ins

    end)


(* A few macros *)
let m_t = Hashtbl.create 17

let r0 = Ireg GPR0

(* LWZ, a convenience *)
let lwz regs k =  match regs with
| [r; addr] ->
    Instruction (Pload (Word,r,0,addr))::k
| _  -> Warn.fatal "LWZ macros takes two reg arguments (tgt,address)"

(* FNO *)
let fno regs k = match regs with
| [r; addr] ->
    let lab = Label.next_label "FNO" in
    Label (lab,Nop)::
    Instruction (Plwarx (r,r0,addr))::
    Instruction (Pstwcx (r,r0,addr))::
    Instruction (Pbcc (Ne,lab))::k
| _  -> Warn.fatal "FNO macro takes two reg arguments (tgt,address)"

let fnox regs k = match regs with
| [r; idx; addr] ->
    let lab = Label.next_label "FNO" in
    Label (lab,Nop)::
    Instruction (Plwarx (r,idx,addr))::
    Instruction (Pstwcx (r,idx,addr))::
    Instruction (Pbcc (Ne,lab))::k
| _  -> Warn.fatal "FNOX macro takes three reg arguments (tgt,idx,address)"

let fno2 regs k = match regs with
| [r; addr] ->
    let lab = Label.next_label "FNO"
    and lab_next =  Label.next_label "FNO" in
    Label (lab,Nop)::
    Instruction (Plwarx (r,r0,addr))::
    Instruction (Pcmpw (0,r0,r0))::
    Instruction (Pbcc (Eq,lab_next))::
    Label (lab_next,Instruction (Pisync))::
    Instruction (Pstwcx (r,r0,addr))::
    Instruction (Pbcc (Ne,lab))::k
| _  -> Warn.fatal "FNO macros takes two reg arguments (tgt,address)"


(* Store atomic *)
let sym = Symbolic_reg "%sta"

let sta regs k = match regs with
| [r; addr] ->
    let lab = Label.next_label "sta" in
    Label (lab,Nop)::
    Instruction (Plwarx (sym,r0,addr))::
    Instruction (Pstwcx (r,r0,addr))::
    Instruction (Pbcc (Ne,lab))::k
| _  -> Warn.fatal "STA macros takes two reg arguments (src,address)"

let stax regs k = match regs with
| [r; idx; addr] ->
    let lab = Label.next_label "sta" in
    Label (lab,Nop)::
    Instruction (Plwarx (sym,idx,addr))::
    Instruction (Pstwcx (r,idx,addr))::
    Instruction (Pbcc (Ne,lab))::k
| _  -> Warn.fatal "STAX macros takes three reg arguments (src,idx,address)"


let sym = Symbolic_reg "%lock"

let lock regs k = match regs with
| [addr] ->
    let loop = Label.next_label "LOOP" in
    let atom = Label.next_label "ATO" in
    Instruction (Pb atom)::
    Label (loop,Nop)::
    Instruction (Pload (Word,sym,0,addr))::
    Instruction (Pcmpwi (0,sym,0))::
    Instruction (Pbcc (Ne,loop))::
    Label (atom,Nop)::
    Instruction (Plwarx (sym,r0,addr))::
    Instruction (Pcmpwi (0,sym,0))::
    Instruction (Pbcc (Ne,loop))::
    Instruction (Pli (sym,1))::
    Instruction (Pstwcx (sym,r0 ,addr))::
    Instruction (Pbcc (Ne,loop))::
    Instruction (Pisync)::
    k
| _ -> Warn.fatal "LOCK takes one reg argument (address)"

let unlock regs k = match regs with
| [addr] ->
    Instruction (Plwsync)::
    Instruction (Pli (sym,0))::
    Instruction (Pstore (Word,sym,0,addr))::
    k
| _ -> Warn.fatal "UNLOCK takes one reg argument (address)"

(* SETFLAG/READFLAG *)
let setflag regs k = match regs with
| [addr] ->
    Instruction (Plwsync)::
    Instruction (Pli (sym,1))::
    Instruction (Pstore (Word,sym,0,addr))::k
| _ -> Warn.fatal "SF takes one reg argument (address)"

let readflag regs k =  match regs with
| [r; addr] ->
    let next = Label.next_label "NEXT" in
    Instruction (Pload (Word,r,0,addr))::
    Instruction (Pcmpw (0,r,r))::
    Instruction (Pbcc (Ne,next))::
    Label (next,Nop)::
    Instruction (Pisync)::k
| _ -> Warn.fatal "RF takes two reg argument (dest,address)"

let readflagloop regs k = match regs with
| [addr] ->
    let loop = Label.next_label "LOOP" in
    Label (loop,Nop)::
    Instruction (Pload (Word,sym,0,addr))::
    Instruction (Pcmpwi (0,sym,0))::
    Instruction (Pbcc (Eq,loop))::
    Instruction (Pisync)::k
| _ -> Warn.fatal "RFL takes one reg argument (address)"

let () =
  Hashtbl.add m_t "LWZ" lwz ;
  Hashtbl.add m_t "STA" sta ;
  Hashtbl.add m_t "STAX" stax ;
  Hashtbl.add m_t "FNO" fno ;
  Hashtbl.add m_t "FNOX" fnox ;
  Hashtbl.add m_t "FNO2" fno2 ;
  Hashtbl.add m_t "LOCK" lock ;
  Hashtbl.add m_t "UNLOCK" unlock ;
  Hashtbl.add m_t "SF" setflag ;
  Hashtbl.add m_t "RF" readflag ;
  Hashtbl.add m_t "RFL" readflagloop ;
  ()


let get_macro name = Hashtbl.find m_t name

let get_id_and_list _i = Warn.fatal "get_id_and_list is only for Bell"
