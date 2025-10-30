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

(** Define registers, barriers, and instructions for ARM *)

open Printf

(* Who am i ? *)
let arch = Archs.arm
let endian = Endian.Little
let base_type = CType.Base "int"

(*************)
(* Registers *)
(*************)

type reg =
  | R0 | R1 | R2 | R3
  | R4 | R5 | R6 | R7
  | R8 | R9 | R10 | R11
  | R12
  | SP | LR | PC | FP

  | Z  (* condition flags *)

  | Symbolic_reg of string
  | Internal of int
  | RESADDR

type 'k shifter =
  S_LSL of 'k

let base =  Internal 0
and max_idx = Internal 1
and idx = Internal 2
and ephemeral = Internal 3
let loop_idx = Internal 4

let pc = PC

let regs =
  [
   R0, "R0" ;
   R1, "R1" ;
   R2, "R2" ;
   R3, "R3" ;
   R4, "R4" ;
   R5, "R5" ;
   R6, "R6" ;
   R7, "R7" ;
   R8, "R8" ;
   R9, "R9" ;
   R10, "R10" ;
   R11, "R11" ;
   R12, "R12" ;
   R12, "IP" ;
   SP, "SP" ;
   FP, "FP" ;
   LR, "LR" ;
   PC, "PC" ;
   Z, "Z" ;
   RESADDR, "RESADDR" ;
 ]

(* this is used by instructions which load registers in pairs*)
(* by specifying Rd1, and implicitly loading Rd2 - see LDRD  *)
let next_reg = function
  | R0 -> R1
  | R1 -> R2 ;
  | R2 -> R3 ;
  | R3 -> R4 ;
  | R4 -> R5 ;
  | R5 -> R6 ;
  | R6 -> R7 ;
  | R7 -> R8 ;
  | R8 -> R9 ;
  | R9 -> R10 ;
  | R10 -> R11 ;
  | R11 -> R12 ;
  | _ -> assert false

let to_parse =
  List.filter
    (fun (r,_) -> match r with
    | Z|RESADDR -> false
    | _ -> true)
    regs

let parse_list =
  List.map (fun (r,s) -> s,r)
    (List.filter
       (fun (r,_) -> match r with
       | Z|RESADDR -> false | _ -> true)
       regs)

let parse_reg s =
  try Some (List.assoc (Misc.uppercase s) parse_list)
  with Not_found -> None

let pp_reg r = match r with
| Symbolic_reg r -> "%"^r
| Internal i -> Printf.sprintf "i%i" i
| _ -> try List.assoc r regs with Not_found -> assert false


let reg_compare = compare

let symb_reg_name = function
  | Symbolic_reg r -> Some r
  | _ -> None

let symb_reg r = Symbolic_reg r
let type_reg _ = base_type

(************)
(* Barriers *)
(************)


type barrier_option =
  | SY
  | ST
  | ISH
  | ISHST
  | NSH
  | NSHST
  | OSH
  | OSHST

let fold_barrier_option f k =
  let k = f SY k in
  let k = f ST k in
  let k = f ISH k in
  let k = f ISHST k in
  let k = f NSH k in
  let k = f NSHST k in
  let k = f OSH k in
  let k = f OSHST k in
  k

type barrier =
  | DMB of barrier_option
  | DSB of barrier_option
  | ISB

let all_kinds_of_barriers = [DMB SY; ]

let pp_option = function
  | SY -> "SY"
  | ST -> "ST"
  | ISH -> "ISH"
  | ISHST -> "ISHST"
  | NSH -> "NSH"
  | NSHST -> "NSHST"
  | OSH -> "OSH"
  | OSHST -> "OSHST"

let pp_barrier_option memo o = match o with
| SY -> memo
| _ -> sprintf "%s.%s" memo (pp_option o)

let pp_barrier_ins memo o = match o with
| SY -> memo
| _ -> sprintf "%s %s" memo (pp_option o)

let pp_barrier b = match b with
| DMB o -> pp_barrier_option "DMB" o
| DSB o -> pp_barrier_option "DSB" o
| ISB -> "ISB"

let barrier_compare = compare

(****************)
(* Instructions *)
(****************)

type lbl = Label.t

type setflags = SetFlags | DontSetFlags

type condition = NE | EQ | AL (* ALWAYS *)

type increment = NO | IB

type 'k kinstruction =
  | I_NOP
  | I_ADD of setflags * reg * reg * 'k
  | I_ADD3 of setflags * reg * reg * reg
  | I_BX of reg
  | I_SUB of setflags * reg * reg * 'k
  | I_SUB3 of setflags * reg * reg * reg
  | I_AND of setflags * reg * reg * 'k
  | I_ANDC of condition * reg * reg * reg
  | I_ORR of setflags * reg * reg * 'k
  | I_B of lbl
  | I_BEQ of lbl
  | I_BNE of lbl (* Was maybeVal ??? *)
  | I_CB of bool * reg * lbl
  | I_CMPI of reg * 'k
  | I_CMP of reg * reg
  | I_LDR of reg * reg * condition
  | I_LDREX of reg * reg
  | I_LDA of reg * reg
  | I_LDAEX of reg * reg
  | I_LDRO of reg * reg * 'k * condition
  | I_LDM2 of reg * reg * reg * increment
  | I_LDM3 of reg * reg * reg * reg * increment
  | I_LDRD of reg * reg * reg * 'k option
  | I_LDR3 of reg * reg * reg * condition
  | I_LDR3_S of reg * reg * reg * 'k shifter * condition
  | I_STR of reg * reg * condition
  | I_STR3 of reg * reg * reg * condition
  | I_STR3_S of reg * reg * reg * 'k shifter * condition
  | I_STREX of reg * reg * reg * condition
  | I_STL of reg * reg * condition
  | I_STLEX of reg * reg * reg
  | I_MOVI of reg * 'k * condition
  | I_MOV of reg * reg * condition
  | I_MOVW of reg * 'k * condition
  | I_MOVT of reg * 'k * condition
  | I_XOR of setflags * reg * reg * reg
  | I_DMB of barrier_option
  | I_DSB of barrier_option
  | I_ISB
 (* SIMD *)
  | I_SADD16 of reg * reg * reg
  | I_SEL of reg * reg * reg

type instruction = int kinstruction
type parsedInstruction = MetaConst.k kinstruction

let pp_lbl = fun i -> i

open PPMode

let pp_hash m = match m with
| Ascii | Dot -> "#"
| Latex -> "\\#"
| DotFig -> "\\\\#"

let pp_k m v = pp_hash m ^ string_of_int v

type 'k basic_pp = { pp_k : 'k -> string; }

let pp_memo memo = function
  | SetFlags -> memo ^ "S"
  | DontSetFlags -> memo

let pp_condition = function
  | NE -> "NE"
  | EQ -> "EQ"
  | AL -> ""

let pp_memoc memo c = sprintf "%s%s" memo (pp_condition c)

let do_pp_instruction m =
  let ppi_rrr opcode s rt rn rm =
    pp_memo opcode s^" "^ pp_reg rt ^ "," ^ pp_reg rn ^ "," ^ pp_reg rm in
  let ppi_rrr_noflags opcode = ppi_rrr opcode DontSetFlags in
  let ppi_rri opcode s rt rn v =
     pp_memo opcode s^" "^pp_reg rt ^ ","^ pp_reg rn ^ "," ^ m.pp_k v in
  let ppi_rrmc opcode rt rn c=
    pp_memoc opcode c^" "^pp_reg rt ^
    ","^ "[" ^ pp_reg rn ^ "]" in
  let ppi_rrm opcode rt rn =  ppi_rrmc opcode rt rn AL in

  let pp_memom opcode = function
  | NO -> opcode
  | IB -> opcode ^ "IB" in

  let ppi_rr_multiple opcode rt rs i =
    pp_memom opcode i ^ " " ^ pp_reg rt ^ ", {" ^
        (String.concat "," (List.map pp_reg rs)) ^ "}"in

  let ppi_rrrmc opcode rt ri rn c =
    pp_memoc opcode c^" "^pp_reg rt ^ ","^
    "[" ^ pp_reg ri ^ "," ^ pp_reg rn ^ "]" in

  let pp_shift = function
  | S_LSL k -> "lsl " ^ m.pp_k k in

  let ppi_rrrmc_s opcode rt ri rn c s =
    pp_memoc opcode c^" "^pp_reg rt ^ ","^
    "[" ^ pp_reg ri ^ "," ^ pp_reg rn ^ ","^
    (pp_shift s) ^ "]" in

  let ppi_rrkmc opcode rt ri k =
    opcode ^" "^pp_reg rt ^ ","^
    "[" ^ pp_reg ri ^ "," ^ m.pp_k k ^ "]" in

  let ppi_strex opcode rt rn rm c =
     pp_memoc opcode c^" "^pp_reg rt ^ ","^
    pp_reg rn ^ ",[" ^ pp_reg rm ^ "]" in

  let ppi_stl opcode rt rn c =
     pp_memoc opcode c^" "^pp_reg rt ^ ",[" ^ pp_reg rn ^ "]" in

  let ppi_rr opcode rt rn = opcode^" "^pp_reg rt ^ ","^ pp_reg rn in
  let ppi_rrc opcode rt rn c=
    pp_memoc opcode c^" "^pp_reg rt ^ ","^ pp_reg rn in
  let ppi_ri opcode r i = opcode^" "^pp_reg r ^ "," ^ m.pp_k i in
  let ppi_ric opcode r i c=
    pp_memoc opcode c^" "^pp_reg r ^ "," ^ m.pp_k i in

  fun i -> match i with
  | I_NOP -> "NOP"
  | I_ADD(s,rt,rn,v) -> ppi_rri "ADD" s rt rn v
  | I_ADD3 (s,r1,r2,r3) -> ppi_rrr "ADD" s r1 r2 r3
  | I_SUB(s,rt,rn,v) -> ppi_rri "SUB" s rt rn v
  | I_SUB3 (s,r1,r2,r3) -> ppi_rrr "SUB" s r1 r2 r3
  | I_AND(s,rt,rn,v) -> ppi_rri "AND" s rt rn v
  | I_ANDC(c,rt,rn,v) -> sprintf "%s %s, %s, %s" (pp_memoc "AND" c)
    (pp_reg rt) (pp_reg rn) (pp_reg v)
  | I_ORR(s,rt,rn,v) -> ppi_rri "ORR" s rt rn v
  | I_B v -> "B " ^ pp_lbl v
  | I_BEQ(v) -> "BEQ "^ pp_lbl v
  | I_BNE(v) -> "BNE "^ pp_lbl v
  | I_CB (n,r,lbl) ->
      sprintf "CB%sZ" (if n then "N" else "") ^
      " " ^ pp_reg r ^ "," ^ pp_lbl lbl
  | I_CMPI (r,v) -> ppi_ri "CMP" r v
  | I_BX r -> "BX " ^ (pp_reg r)
  | I_CMP (r1,r2) -> ppi_rr "CMP" r1 r2
  | I_LDREX(rt,rn) -> ppi_rrm "LDREX" rt rn
  | I_LDA(rt,rn) -> ppi_rrm "LDA" rt rn
  | I_LDAEX(rt,rn) -> ppi_rrm "LDAEX" rt rn
  | I_LDM2(rt,r1,r2,i) -> ppi_rr_multiple "LDM" rt [r1; r2] i
  | I_LDM3(rt,r1,r2,r3,i) -> ppi_rr_multiple "LDM" rt [r1;r2;r3] i
  | I_LDR(rt,rn,c) -> ppi_rrmc "LDR" rt rn c
  | I_LDRD(rd1,rd2,rn,Some k) -> sprintf "LDRD %s, %s, [%s, %s]"
      (pp_reg rd1) (pp_reg rd2) (pp_reg rn) (m.pp_k k)
  | I_LDRD(rd1,rd2,rn,None) -> sprintf "LDRD %s, %s, [%s]"
      (pp_reg rd1) (pp_reg rd2) (pp_reg rn)
  | I_LDR3(rt,rn,rm,c) -> ppi_rrrmc "LDR" rt rn rm c
  | I_LDR3_S(rt,rn,rm,s,c) -> ppi_rrrmc_s "LDR" rt rn rm c s
  | I_LDRO(rt,rn,k,_) -> ppi_rrkmc "LDR" rt rn k
  | I_STR(rt,rn,c) -> ppi_rrmc "STR" rt rn c
  | I_STR3(rt,rn,rm,c) -> ppi_rrrmc "STR" rt rn rm c
  | I_STR3_S(rt,rn,rm,s,c) -> ppi_rrrmc_s "STR" rt rn rm c s
  | I_STREX(rt,rn,rm,c) -> ppi_strex "STREX" rt rn rm c
  | I_STL(rt,rn,c) -> ppi_stl "STL" rt rn c
  | I_STLEX(rt,rn,rm) -> ppi_strex "STLEX" rt rn rm AL
  | I_MOVI(r,i,c) -> ppi_ric "MOV" r i c
  | I_MOV(r1,r2,c) -> ppi_rrc "MOV" r1 r2 c
  | I_MOVW(r1,k,c) -> pp_memoc "MOVW" c ^ (pp_reg r1) ^ ", " ^ (m.pp_k k)
  | I_MOVT(r1,k,c) -> pp_memoc "MOVT " c ^ (pp_reg r1) ^ ", " ^ (m.pp_k k)
  | I_XOR(s,r1,r2,r3) -> ppi_rrr "EOR" s r1 r2 r3
  | I_DMB o -> pp_barrier_ins "DMB" o
  | I_DSB o -> pp_barrier_ins "DSB" o
  | I_ISB  -> "ISB"
  | I_SADD16 (r1,r2,r3) -> ppi_rrr_noflags "SADD16" r1 r2 r3
  | I_SEL (r1,r2,r3) -> ppi_rrr_noflags "SEL" r1 r2 r3


let pp_instruction m =
  do_pp_instruction
    {pp_k = pp_k m}

let dump_instruction =
  do_pp_instruction {pp_k = (fun v -> "#" ^ string_of_int v)}

and dump_parsedInstruction =
  do_pp_instruction  {pp_k = MetaConst.pp_prefix "#"; }

let dump_instruction_hash = dump_instruction

(****************************)
(* Symbolic registers stuff *)
(****************************)

let allowed_for_symb =
  [ R0 ; R1 ; R2 ; R3 ; R4 ; R5 ; R6 ;
    R7 ; R8 ; R9 ; R10; R11; R12 ]

let fold_regs (f_reg,f_sreg) =

  let fold_reg reg (y_reg,y_sreg) = match reg with
  | R0 | R1 | R2 | R3 | R4 | R5 | R6 | R7 | R8
  | R9 | R10 | R11 | R12 | SP | FP | LR | PC | Z | RESADDR ->  f_reg reg y_reg,y_sreg
  | Symbolic_reg reg -> y_reg,f_sreg reg y_sreg
  | Internal _ -> y_reg,y_sreg in

  fun c ins -> match ins with
  | I_ADD (_,r1, r2, _)
  | I_SUB (_,r1, r2, _)
  | I_AND (_,r1, r2, _)
  | I_ORR (_,r1, r2, _)
  | I_LDR (r1, r2, _)
  | I_LDREX (r1, r2)
  | I_LDA (r1, r2)
  | I_LDAEX (r1, r2)
  | I_LDRO (r1, r2,_,_)
  | I_STR (r1, r2, _)
  | I_STL (r1, r2, _)
  | I_MOV (r1, r2, _)
  | I_CMP (r1,r2)
      -> fold_reg r2 (fold_reg r1 c)
  | I_ANDC (_,r1, r2, r3)
  | I_LDR3 (r1, r2, r3, _)
  | I_LDR3_S (r1, r2, r3, _,_)
  | I_LDRD (r1, r2, r3, _)
  | I_LDM2 (r1, r2, r3,_)
  | I_STLEX (r1, r2, r3)
  | I_ADD3 (_, r1, r2, r3)
  | I_SUB3 (_, r1, r2, r3)
  | I_STR3 (r1, r2, r3, _)
  | I_STR3_S (r1, r2, r3, _,_)
  | I_STREX (r1, r2, r3, _)
  | I_XOR (_,r1, r2, r3)
  | I_SADD16 (r1, r2, r3)
  | I_SEL (r1, r2, r3)
      -> fold_reg r3 (fold_reg r2 (fold_reg r1 c))
  | I_LDM3 (r1, r2, r3,r4,_)
      -> fold_reg r4 (fold_reg r3 (fold_reg r2 (fold_reg r1 c)))
  | I_BX r
  | I_CMPI (r, _)
  | I_MOVI (r, _, _)
  | I_MOVW (r,_,_) | I_MOVT (r,_,_)
  | I_CB (_,r,_)
      -> fold_reg r c
  | I_NOP
  | I_B _
  | I_BEQ _
  | I_BNE _
  | I_DMB _
  | I_DSB _
  | I_ISB
      -> c


let map_regs f_reg f_symb =

  let map_reg  reg = match reg with
  | R0 | R1 | R2 | R3 | R4 | R5 | R6 | R7 | R8
  | R9 | R10 | R11 | R12 | SP | FP | LR | PC | Z | RESADDR -> f_reg reg
  | Symbolic_reg reg -> f_symb reg
  | Internal _ -> reg in

  fun ins -> match ins with
  | I_ADD (s,r1, r2, k) -> I_ADD (s,map_reg r1, map_reg r2, k)
  | I_ADD3 (s,r1, r2, r3) -> I_ADD3 (s,map_reg r1, map_reg r2, map_reg r3)
  | I_SUB (s,r1, r2, k) -> I_SUB (s,map_reg r1, map_reg r2, k)
  | I_SUB3 (s,r1, r2, r3) -> I_SUB3 (s,map_reg r1, map_reg r2, map_reg r3)
  | I_AND (s,r1, r2, k) -> I_AND (s,map_reg r1, map_reg r2, k)
  | I_ORR (s,r1, r2, k) -> I_ORR (s,map_reg r1, map_reg r2, k)
  | I_NOP
  | I_B _
  | I_BEQ _
  | I_BNE _ -> ins
  | I_BX r -> I_BX (map_reg r)
  | I_CB (n,r,lbl) -> I_CB (n,map_reg r,lbl)
  | I_CMPI (r, k) -> I_CMPI (map_reg r, k)
  | I_CMP (r1, r2) -> I_CMP (map_reg r1, map_reg r2)
  | I_LDREX (r1, r2) -> I_LDREX (map_reg r1, map_reg r2)
  | I_LDA (r1, r2) -> I_LDA (map_reg r1, map_reg r2)
  | I_LDAEX (r1, r2) -> I_LDAEX (map_reg r1, map_reg r2)
  | I_LDRO (r1, r2,k,c) -> I_LDRO (map_reg r1, map_reg r2,k,c)
  | I_LDR (r1, r2, c) -> I_LDR (map_reg r1, map_reg r2, c)
  | I_LDR3 (r1, r2, r3, c) -> I_LDR3 (map_reg r1, map_reg r2, map_reg r3, c)
  | I_LDRD (r1, r2, r3, k) -> I_LDRD (map_reg r1, map_reg r2, map_reg r3, k)
  | I_LDM2 (r1,r2,r3,i) -> I_LDM2 (map_reg r1, map_reg r2, map_reg r3,i)
  | I_LDM3 (r1,r2,r3,r4,i) -> I_LDM3 (map_reg r1, map_reg r2, map_reg r3,map_reg r4, i)
  | I_STR (r1, r2, c) -> I_STR (map_reg r1, map_reg r2, c)
  | I_STR3 (r1, r2, r3, c) -> I_STR3 (map_reg r1, map_reg r2, map_reg r3, c)
  | I_ANDC (c,r1, r2, r3) -> I_ANDC (c, map_reg r1, map_reg r2, map_reg r3)
  | I_STR3_S (r1, r2, r3, c,s) -> I_STR3_S (map_reg r1, map_reg r2, map_reg r3, c,s)
  | I_LDR3_S (r1, r2, r3, s,c) -> I_LDR3_S (map_reg r1, map_reg r2, map_reg r3, s,c)
  | I_STREX (r1, r2, r3, c) -> I_STREX (map_reg r1, map_reg r2, map_reg r3, c)
  | I_STL (r1, r2, c) -> I_STL (map_reg r1, map_reg r2, c)
  | I_MOVI (r, k, c) -> I_MOVI (map_reg r, k, c)
  | I_MOVW (r, k,c) -> I_MOVW (map_reg r, k,c)
  | I_MOVT (r, k,c) -> I_MOVT (map_reg r, k,c)
  | I_MOV (r1, r2, c) -> I_MOV (map_reg r1, map_reg r2, c)
  | I_XOR (s,r1, r2, r3) -> I_XOR (s,map_reg r1, map_reg r2, map_reg r3)
  | I_STLEX (r1, r2, r3) -> I_STLEX (map_reg r1, map_reg r2, map_reg r3)
  | I_DMB _
  | I_DSB _
  | I_ISB -> ins
  | I_SADD16 (r1, r2, r3) -> I_SADD16 (map_reg r1, map_reg r2, map_reg r3)
  | I_SEL (r1, r2, r3) -> I_SEL (map_reg r1, map_reg r2, map_reg r3)

(* No addresses burried in ARM code *)
let fold_addrs _f c _ins = c

let map_addrs _f ins = ins

(* PLDI submission, complete later *)
let is_data _ _ = assert false

(* Instruction continuation *)
let get_next = function
  | I_NOP
  | I_ADD _
  | I_ADD3 _
  | I_SUB _
  | I_SUB3 _
  | I_AND _
  | I_ANDC _
  | I_ORR _
  | I_CMPI _
  | I_CMP _
  | I_LDR _
  | I_LDM2 _
  | I_LDM3 _
  | I_LDREX _
  | I_LDA _
  | I_LDAEX _
  | I_LDRO _
  | I_LDRD _
  | I_LDR3 _
  | I_LDR3_S _
  | I_STR _
  | I_STR3 _
  | I_STR3_S _
  | I_STREX _
  | I_STL _
  | I_STLEX _
  | I_MOVI _
  | I_MOVW _
  | I_MOVT _
  | I_MOV _
  | I_XOR _
  | I_DMB _
  | I_DSB _
  | I_ISB
  | I_SADD16 _
  | I_SEL _
    -> [Label.Next]
  | I_B lbl -> [Label.To lbl]
  | I_BX _ -> [Label.Any]
  | I_BEQ lbl|I_BNE lbl|I_CB (_,_,lbl) -> [Label.Next; Label.To lbl]

include
  InstrUtils.WithNop
    (struct
      type instr = instruction
      let nop = I_NOP
      let compare = compare
    end)

include Pseudo.Make
    (struct
      type ins = instruction
      type pins = parsedInstruction
      type reg_arg = reg

      let parsed_tr = function
        | I_ADD (c,r1,r2,k) ->  I_ADD (c,r1,r2,MetaConst.as_int k)
        | I_SUB (c,r1,r2,k) ->  I_SUB (c,r1,r2,MetaConst.as_int k)
        | I_AND (c,r1,r2,k) ->  I_AND (c,r1,r2,MetaConst.as_int k)
        | I_ANDC (c,r1,r2,r3) ->  I_ANDC (c,r1,r2,r3)
        | I_ORR (c,r1,r2,k) ->  I_ORR (c,r1,r2,MetaConst.as_int k)
        | I_LDRO (r1,r2,k,c) ->  I_LDRO (r1,r2,MetaConst.as_int k,c)
        | I_LDRD (r1,r2,r3,Some k) ->  I_LDRD (r1,r2,r3,Some (MetaConst.as_int k))
        | I_STR3_S (r1,r2,r3,S_LSL k,c) ->
          I_STR3_S (r1,r2,r3,S_LSL (MetaConst.as_int k),c)
        | I_LDR3_S (r1,r2,r3,S_LSL k,c) ->
          I_LDR3_S (r1,r2,r3,S_LSL (MetaConst.as_int k),c)
        | I_LDRD (r1,r2,r3,None) ->  I_LDRD (r1,r2,r3,None)
        | I_BX r -> I_BX r
        | I_CMPI (r,k) -> I_CMPI (r,MetaConst.as_int k)
        | I_MOVI (r,k,c) -> I_MOVI (r,MetaConst.as_int k,c)
        | I_MOVW (r,k,c) -> I_MOVW (r,MetaConst.as_int k,c)
        | I_MOVT (r,k,c) -> I_MOVT (r,MetaConst.as_int k,c)
        | I_NOP
        | I_ADD3 _
        | I_SUB3 _
        | I_B _
        | I_BEQ _
        | I_BNE _
        | I_CB _
        | I_CMP _
        | I_LDR _
        | I_LDM2 _
        | I_LDM3 _
        | I_LDREX _
        | I_LDA _
        | I_LDAEX _
        | I_LDR3 _
        | I_STR _
        | I_STR3 _
        | I_STREX _
        | I_STL _
        | I_STLEX _
        | I_MOV _
        | I_XOR _
        | I_DMB _
        | I_DSB _
        | I_ISB
        | I_SADD16 _
        | I_SEL _
            as keep -> keep


      let get_naccesses = function
        | I_NOP
        | I_ADD _
        | I_ADD3 _
        | I_SUB _
        | I_SUB3 _
        | I_AND _
        | I_ANDC _
        | I_ORR _
        | I_B _
        | I_BX _
        | I_BEQ _
        | I_BNE _
        | I_CB _
        | I_CMPI _
        | I_CMP _
        | I_MOVI _
        | I_MOVW _
        | I_MOVT _
        | I_MOV _
        | I_XOR _
        | I_DMB _
        | I_DSB _
        | I_ISB
        | I_SADD16 _
        | I_SEL _
          -> 0
        | I_LDR _
        | I_LDM2 _
        | I_LDREX _
        | I_LDA _
        | I_LDAEX _
        | I_LDRO _
        | I_LDR3 _
        | I_LDR3_S _
        | I_STR _
        | I_STR3 _
        | I_STR3_S _
        | I_STREX _
        | I_STL _
        | I_STLEX _
            -> 1
        | I_LDM3 _
            -> 3
        | I_LDRD _
            -> 2

      let size_of_ins _ = 4

      let fold_labels k f = function
        | I_B lbl
        | I_BEQ lbl
        | I_BNE lbl
        | I_CB (_,_,lbl)
          -> f k lbl
        | _ -> k

      let map_labels f =
        let open BranchTarget in
        function
        | I_B lbl -> I_B (as_string_fun f lbl)
        | I_BEQ lbl -> I_BEQ (as_string_fun f lbl)
        | I_BNE lbl -> I_BNE (as_string_fun f lbl)
        | I_CB (n,r,lbl) -> I_CB (n,r,as_string_fun f lbl)
        | ins -> ins

    end)

let get_macro _name = raise Not_found

let get_id_and_list _i = Warn.fatal "get_id_and_list is only for Bell"

let hash_pteval _ = assert false

module Instr = Instr.No(struct type instr = instruction end)
