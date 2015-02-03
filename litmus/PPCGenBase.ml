(*********************************************************************)
(*                        Memevents                                  *)
(*                                                                   *)
(* Jade Alglave, Luc Maranget, INRIA Paris-Rocquencourt, France.     *)
(* Susmit Sarkar, Peter Sewell, University of Cambridge, UK.         *)
(*                                                                   *)
(*  Copyright 2010 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

(* Who am i ? *)
let arch = `PPCGen

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


type crbit = int (* i in [0..31] *)
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
  | CRBit of crbit
        (* fields of the condition register *)
  | CRField  of int (* i in [0..7] *)
  | LR (* link register; for return address *)
  | CTR (* count register; used for some branches *)
  | CARRY (* carry bit of the status register *)
  | Symbolic_reg of string
(* Internal regs *)
  | Internal of int

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

let reg_compare = Pervasives.compare

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


    let parse_list =
      List.map (fun (r,s) -> s,Ireg r) iregs @
      List.map (fun (r,s) -> s,Freg r) fregs

    let parse_reg s =
      let s = String.lowercase s in
      try Some (List.assoc s parse_list)
      with Not_found -> None


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

let barrier_compare = Pervasives.compare

(****************)
(* Instructions *)
(****************)

type idx = int (* limited to ? bits *)
type k = int   (* limited to ? bits *)
type lbl = Label.t
type cond =
  | Eq | Ne
  | Lt | Ge
  | Gt | Le

type crindex = int (* in fact [0..7] *)
type crmask = int (* in fact [0..127] *)

type setcr0 = SetCR0 | DontSetCR0
type setsoov = SetSOOV | DontSetSOOV
type setaa = SetAA | DontSetAA
type setlk = SetLK | DontSetLK

(* j: for arithm at least, should be ireg *)
type instruction =
  [
  (* Generated fixed-point instructions *)
  (* #include "./src_power_gen/ast.gen" *)

  (* Branch *)
  | `Pb_lbl of lbl
  | `Pbl_lbl of lbl
  | `Pbcc_lbl of cond * lbl
  | `Pblr_lbl

  | `Pcomment of string
 ]

    let pp_k = string_of_int
    let pp_idx = string_of_int

    let ppi_index_mode opcode r1 r2 r3 =
      opcode^" "^pp_reg r1 ^ ","^pp_reg r2 ^ ","^pp_reg r3

    let ppi_index_mode2 opcode r1 r2 =
      opcode^" "^pp_reg r1 ^ ","^pp_reg r2

    let ppi_imm_index_mode opcode r1 d r2 =
      opcode^" "^pp_reg r1 ^ ","^pp_idx d ^ "("^pp_reg r2^")"

    let ppi_imm_instr opcode r1 r2 v =
      opcode^" "^pp_reg r1 ^ ","^pp_reg r2 ^ ","^pp_k v

    let ppi_imm_instr_memo opcode set r1 r2 v =
      let memo = match set with
      | SetCR0 -> opcode ^ "."
      | DontSetCR0 -> opcode in
      ppi_imm_instr memo r1 r2 v

    let ppi_ri opcode rD v = opcode^" "^pp_reg rD ^ ","^pp_k v

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

    let do_pp_instruction i = match i with
    (* extended mnemonics first *)
    | `Paddi (r1, Ireg GPR0, k) -> sprintf "li %s,%d" (pp_reg r1) k
    (* TODO: other extended mnemonics *)
 
    (* #include "src_power_gen/pretty.gen" *)

    | `Pb_lbl lbl -> "b   " ^ lbl
    | `Pbl_lbl lbl -> "bl   " ^ lbl
    | `Pbcc_lbl (cond, lbl) -> "b"^pp_cond cond ^ "  " ^ lbl
    | `Pblr_lbl -> "blr"

    | `Pcomment s -> "com \"" ^ s ^ "\""

    let pp_instruction _m ins = do_pp_instruction ins

    let dump_instruction = do_pp_instruction

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
  (* #include "src_power_gen/fold.gen" *)

  | `Pb_lbl _
  | `Pbcc_lbl _
  | `Pbl_lbl _
  | `Pblr_lbl
  | `Pcomment _
    -> c


        (* Map over symbolic regs *)
let map_regs f_reg f_symb =

  let map_reg reg = match reg with
  | Symbolic_reg s -> f_symb s
  | Ireg _ -> f_reg reg
  | _ -> reg in

  let map3 ins r1 r2 r3 =ins (map_reg r1,map_reg r2,map_reg r3)
  and map2 ins r1 r2 = ins (map_reg r1,map_reg r2) in

  fun ins -> match ins with
  (* #include "src_power_gen/map.gen" *)

  | `Pb_lbl _
  | `Pbcc_lbl _
  | `Pbl_lbl _
  | `Pblr_lbl
  | `Pcomment _
    -> ins

(* No addresses burried in PPC code *)
let fold_addrs _f c _ins = c

let map_addrs _f ins = ins

(* Go back to 32bits mode *)

let norm_ins ins = (* FIXME ??? *) ins (* match ins with *)
  (* | Pld (r1,cst,r2)  -> Plwz (r1,cst,r2) *)
  (* | Pstd (r1,cst,r2) -> Pstw (r1,cst,r2) *)
  (* | Pldx (r1,r2,r3)  -> Plwarx (r1,r2,r3) *)
  (* | Pstdx (r1,r2,r3) -> Pstwx (r1,r2,r3) *)

  (* (\* XXX I don't understand what I should do *\) *)
  (* | _ -> ins *)

let is_data r1 i = (* FIXME ??? *) false (* match i with *)
(* | Pstd (r,_,_) *)
(* | Pstwcx (r,_,_) *)
(* | Pstw (r,_,_) -> r1 = r *)
(* (\* XXX I don't understand what I should do *\) *)
(* | _ -> false *)

let get_next = function
  |`Pb_lbl lbl -> [Label.To lbl]
  |`Pbcc_lbl (_, lbl) -> [Label.Next;Label.To lbl]
        (* Hum *)
  |`Pbl_lbl _ -> [Label.Next]
  |`Pblr_lbl (*|`Pmtspr (8,_) |`Pmfspr (_,8) *)-> []

  (* XXX I'm not sure this is correct; eg. see `Pmtspr. *)
  | _ -> [Label.Next]

(* Macros *)

include Pseudo.Make
    (struct
      type ins = instruction
      type reg_arg = reg

      let get_naccesses =
        (* number of memory accesses *)
        (* XXX this should be guessable from pseudocode *)
        function _ ->  failwith "shouldn't need this for litmus"

      let fold_labels k f = function
        | `Pb_lbl lab
        | `Pbl_lbl lab
        | `Pbcc_lbl (_,lab) -> f k lab
        (* XXX there is no label in generated code *)
        | _ -> k

      let map_labels f = function
        | `Pb_lbl lab -> `Pb_lbl (f lab)
        | `Pbl_lbl lab -> `Pbl_lbl (f lab)
        | `Pbcc_lbl (cc,lab) -> `Pbcc_lbl (cc,f lab)
        (* XXX there is no label in generated code *)
        | ins -> ins

    end)

let get_macro name = raise Not_found
