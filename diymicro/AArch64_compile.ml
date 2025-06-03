include MakeAArch64Base.Make(struct let is_morello = false end)

(** current state inside a single proc *)
type state = {
  free_registers: reg list; (* available registers *)
  next_addr: int;
}

let next_reg (st: state) = match st.free_registers with
  | r::rs -> r,{ st with free_registers=rs; }
  | [] -> failwith "No more free registers"




module TypBase = struct
  open MachSize

  type sgn = Signed | Unsigned

  type t =   Int | Std of sgn * MachSize.sz | Pteval

  let tags =
    ["int";
    "int8_t"; "uint8_t";
    "int16_t"; "uint16_t";
    "int32_t"; "uint32_t";
    "int64_t"; "uint64_t";
    "int128_t"; "uint128_t";
    "__int128"; "__uint128";
  ]

  let parse s = match s with
  | "int" -> Some Int
  | "int8_t" -> Some (Std (Signed,Byte))
  | "uint8_t" -> Some (Std (Unsigned,Byte))
  | "int16_t" -> Some (Std (Signed,Short))
  | "uint16_t" -> Some (Std (Unsigned,Short))
  | "int32_t" -> Some (Std (Signed,Word))
  | "uint32_t" -> Some (Std (Unsigned,Word))
  | "int64_t" -> Some (Std (Signed,Quad))
  | "uint64_t" -> Some (Std (Unsigned,Quad))
  | "int128_t" -> Some (Std (Signed,S128))
  | "uint128_t" -> Some (Std (Unsigned,S128))
  | "__int128" -> Some (Std (Signed,S128))
  | "__uint128" -> Some (Std (Unsigned,S128))
  | _ -> None

  let pp = function
  | Int ->  "int"
  | Std (Signed,Byte) ->  "int8_t"
  | Std (Unsigned,Byte) ->  "uint8_t"
  | Std (Signed,Short) ->  "int16_t"
  | Std (Unsigned,Short) ->  "uint16_t"
  | Std (Signed,Word) ->  "int32_t"
  | Std (Unsigned,Word) ->  "uint32_t"
  | Std (Signed,Quad) ->  "int64_t"
  | Std (Unsigned,Quad) ->  "uint64_t"
  | Std (Signed,S128) ->  "__int128"
  | Std (Unsigned,S128) ->  "__uint128"
  | Pteval -> "pteval_t"


  let sign_equal s1 s2 = match s1,s2 with
  | (Unsigned,Unsigned)
  | (Signed,Signed)
    -> true
  | (Unsigned,Signed)
  | (Signed,Unsigned)
    -> false

  let equal t1 t2 = match t1,t2 with
  | (Int,Int)
  | (Pteval,Pteval)
    -> true
  | (Std (s1,sz1),Std (s2,sz2))
    -> sign_equal s1 s2 && MachSize.equal sz1 sz2
  | ((Int|Pteval),Std _)
  | (Std _,(Int|Pteval))
  | (Int,Pteval)
  | (Pteval,Int)
    -> false

  let default = Int
  let is_default = function
    | Int -> true
    | _ -> false

  let pteval_t = Pteval
  let is_pteval_t = function
    | Pteval -> true
    | _ -> false

  let get_size = function
    | Int -> Word
    | Std (_,sz) -> sz
    | Pteval -> Quad
end

let nop = "NOP"

let pseudo l = List.map (fun i -> Instruction i) l

let vloc = let open TypBase in
  match TypBase.default with
  | Std (_,MachSize.S128) -> V128
  | Std (_,MachSize.Quad) -> V64
  | Int |Std (_,MachSize.Word) -> V32
  | Std (_,(MachSize.Short|MachSize.Byte)) -> V32
  | Pteval -> V64

let sz2v =
  let open MachSize in
  function
    | Byte|Short|Word -> V32
    | Quad -> V64
    | S128 -> V128

and v2sz =
  let open MachSize in
  function
  | V128 -> S128
  | V64 -> Quad
  | V32 -> Word

let szloc = v2sz vloc

let do_movi vdep r (i: int) = I_MOV (vdep,r,K i)
let mov = do_movi vloc

let mov_mixed sz r i =
  let sz =
    let open MachSize in
    match sz with
    | S128 -> Quad (* MOV C?,#X is not recognized *)
    | Byte|Short|Word|Quad -> sz in
  let v = sz2v sz in I_MOV (v,r,i)

let mov_reg_addr r1 r2 =  I_MOV (V64,r1,RV (V64,r2))
let mov_reg r1 r2 = I_MOV (vloc,r1,RV (vloc,r2))
let mov_reg_mixed sz r1 r2 = let v = sz2v sz in I_MOV (v,r1,RV (v,r2))
let movi_reg r1 i = I_MOVI_V (r1,i,S_NOEXT)

type instruction = pseudo

let mov r i = Instruction (mov r i)
let mov_mixed sz r i = Instruction (mov_mixed sz r (K i))
let mov_reg r1 r2 = Instruction (mov_reg r1 r2)
let mov_reg_mixed sz r1 r2 = Instruction (mov_reg_mixed sz r1 r2)

let op3i v op r1 r2 i  =
  let open OpExt in I_OP3 (v,op,r1,r2,Imm (i,0))
and op3r v op r1 r2 r3 =
  let open OpExt in I_OP3 (v,op,r1,r2,Reg (r3,no_shift))

let cbz r1 lbl = I_CBZ (vloc,r1,BranchTarget.Lbl lbl)
let do_cbnz v r1 lbl = I_CBNZ (v,r1,BranchTarget.Lbl lbl)
let cbnz = do_cbnz vloc

let do_cmpi v r i = op3i v SUBS ZR r i
let cmpi r i = do_cmpi vloc r i

let do_csel v r1 r2 r3 = I_CSEL (v,r1,r2,r3,EQ,Cpy)
let do_cinc v r1 r2 r3 = I_CSEL (v,r1,r2,r3,EQ,Inc)

let cmp r1 r2 = op3r vloc SUBS ZR r1 r2

let b lbl = I_B (BranchTarget.Lbl lbl)
let bne lbl = I_BC (NE,BranchTarget.Lbl lbl)

let eor sz r1 r2 r3 = op3r sz EOR r1 r2 r3

let andi sz r1 r2 k = op3i sz AND r1 r2 k

let incr r = op3i V32 ADD r r 1

let lsri64 r1 r2 k = op3i V64 LSR r1 r2 k

let do_addi v r1 r2 k = op3i v ADD r1 r2 k
let addi = do_addi vloc
let addi_64 = do_addi V64

let add v r1 r2 r3 = op3r v ADD r1 r2  r3
let add_simd r1 r2 = I_ADD_SIMD (r1,r1,r2)

let do_add64 v r1 r2 r3 =
  match v with
  | V64 -> add v r1 r2 r3
  | _ ->
      let ext = Ext.v2sext v in
      I_ADDSUBEXT (V64,Ext.ADD,r1,r2,(v,r3),(ext, None))

let do_addcapa r1 r2 r3 =
  I_ADDSUBEXT (V128,Ext.ADD,r1,r2,(V64,r3),Ext.no_ext)
let gctype r1 r2 = I_GC (GCTYPE,r1,r2)
let gcvalue r1 r2 = I_GC (GCVALUE,r1,r2)
let scvalue r1 r2 r3 = I_SC (SCVALUE,r1,r2,r3)
let seal r1 r2 r3 = I_SEAL (r1,r2,r3)
let cseal r1 r2 r3 = I_CSEAL (r1,r2,r3)

let subi sz r1 r2 k = op3i sz SUB r1 r2 k
let dec r1 r2 = subi vloc r1 r2 1

let ldr_mixed r1 r2 sz o =
  let idx = MemExt.Imm (o,Idx) in
  let open MachSize in
  match sz with
  | Byte -> I_LDRBH (B,r1,r2,idx)
  | Short -> I_LDRBH (H,r1,r2,idx)
  | Word -> I_LDR (V32,r1,r2,idx)
  | Quad -> I_LDR (V64,r1,r2,idx)
  | S128 -> I_LDR (V128,r1,r2,idx)

let do_ldr v r1 r2 = I_LDR (v,r1,r2,MemExt.Imm(0,Idx))
let ldg r1 r2 = I_LDG (r1,r2,0)
let ldct r1 r2 = I_LDCT(r1,r2)
let do_ldar vr r1 r2 = I_LDAR (vr,AA,r1,r2)
let do_ldapr vr r1 r2 = I_LDAR (vr,AQ,r1,r2)
let ldxr r1 r2 = I_LDAR (vloc,XX,r1,r2)
let ldaxr r1 r2 = I_LDAR (vloc,AX,r1,r2)
let sxtw r1 r2 = I_SXTW (r1,r2)

let do_ldr_idx v1 v2 r1 r2 idx =
  let open MemExt in
  let sext = match v2 with
    | V32 -> SXTW
    | V64|V128 -> LSL in
  I_LDR (v1,r1,r2,Reg (v2,idx,sext,0))

let ldr_mixed_idx v r1 r2 idx sz  =
  let idx = MemExt.v2idx_reg v idx in
  let open MachSize in
  match sz with
  | Byte -> I_LDRBH (B,r1,r2,idx)
  | Short -> I_LDRBH (H,r1,r2,idx)
  | Word -> I_LDR (V32,r1,r2,idx)
  | Quad -> I_LDR (V64,r1,r2,idx)
  | S128 -> I_LDR (V128,r1,r2,idx)

let str_mixed sz o r1 r2 =
  let idx = MemExt.Imm (o,Idx) in
  let open MachSize in
  match sz with
  | Byte -> I_STRBH (B,r1,r2,idx)
  | Short -> I_STRBH (H,r1,r2,idx)
  | Word -> I_STR (V32,r1,r2,idx)
  | Quad -> I_STR (V64,r1,r2,idx)
  | S128 -> I_STR (V128,r1,r2,idx)

let do_str v r1 r2 = I_STR (v,r1,r2,MemExt.Imm (0,Idx))
let str = do_str vloc
let stg r1 r2 = I_STG (r1,r2,(0,Idx))
let stct r1 r2 = I_STCT(r1,r2)
let do_stlr v r1 r2 = I_STLR (v,r1,r2)
let stlr = do_stlr vloc

let do_str_idx v r1 r2 idx = I_STR (vloc,r1,r2,MemExt.v2idx_reg v idx)
let str_idx = do_str_idx vloc
let stxr r1 r2 r3 = I_STXR (vloc,YY,r1,r2,r3)
let stlxr r1 r2 r3 = I_STXR (vloc,LY,r1,r2,r3)


let stxr_sz t sz r1 r2 r3 =
  let open MachSize in
  match sz with
  | Byte -> I_STXRBH (B,t,r1,r2,r3)
  | Short -> I_STXRBH (H,t,r1,r2,r3)
  | Word -> I_STXR (V32,t,r1,r2,r3)
  | Quad -> I_STXR (V64,t,r1,r2,r3)
  | S128 -> I_STXR (V128,t,r1,r2,r3)

let ldxr_sz t sz r1 r2 =
  let open MachSize in
  match sz with
  | Byte -> I_LDARBH (B,t,r1,r2)
  | Short -> I_LDARBH (H,t,r1,r2)
  | Word -> I_LDAR (V32,t,r1,r2)
  | Quad -> I_LDAR (V64,t,r1,r2)
  | S128 -> I_LDAR (V128,t,r1,r2)

let sumi_addr_gen tempo st rA o = match o with
| 0 -> rA,[],st
| _ ->
    let r,st = tempo st in
    r,[addi_64 r rA o],st


let str_mixed_idx sz v r1 r2 idx  =
  let idx = MemExt.v2idx_reg v idx in
  let open MachSize in
  match sz with
  | Byte -> I_STRBH (B,r1,r2,idx)
  | Short -> I_STRBH (H,r1,r2,idx)
  | Word -> I_STR (V32,r1,r2,idx)
  | Quad -> I_STR (V64,r1,r2,idx)
  | S128 -> I_STR (V128,r1,r2,idx)

let swp_mixed sz a rS rT rN =
  let open MachSize in
  match sz with
  | Byte -> I_SWPBH (B,a,rS,rT,rN)
  | Short ->  I_SWPBH (H,a,rS,rT,rN)
  | Word ->  I_SWP (V32,a,rS,rT,rN)
  | Quad ->  I_SWP (V64,a,rS,rT,rN)
  | S128 ->  I_SWP (V128,a,rS,rT,rN)

let swp a rS rT rN =  I_SWP (vloc,a,rS,rT,rN)

let sctag a rN rM = I_SC (SCTAG,a,rN,rM)

let cas_mixed sz a rS rT rN =
  let open MachSize in
  match sz with
  | Byte -> I_CASBH (B,a,rS,rT,rN)
  | Short ->  I_CASBH (H,a,rS,rT,rN)
  | Word ->  I_CAS (V32,a,rS,rT,rN)
  | Quad ->  I_CAS (V64,a,rS,rT,rN)
  | S128 ->  I_CAS (V128,a,rS,rT,rN)

let cas a rS rT rN =  I_CAS (vloc,a,rS,rT,rN)

let ldop_mixed op sz a rS rT rN =
  let open MachSize in
  match sz with
  | Byte -> I_LDOPBH (op,B,a,rS,rT,rN)
  | Short ->  I_LDOPBH (op,H,a,rS,rT,rN)
  | Word ->  I_LDOP (op,V32,a,rS,rT,rN)
  | Quad ->  I_LDOP (op,V64,a,rS,rT,rN)
  | S128 ->  I_LDOP (op,V128,a,rS,rT,rN)

let ldop op a rS rT rN =  I_LDOP (op,vloc,a,rS,rT,rN)

let stop_mixed op sz a rS rN =
  let open MachSize in
  match sz with
  | Byte -> I_STOPBH (op,B,a,rS,rN)
  | Short ->  I_STOPBH (op,H,a,rS,rN)
  | Word ->  I_STOP (op,V32,a,rS,rN)
  | Quad ->  I_STOP (op,V64,a,rS,rN)
  | S128 ->  I_STOP (op,V128,a,rS,rN)

let stop op a rS rN =  I_STOP (op,vloc,a,rS,rN)


let stlr_of_sz sz r1 r2 =
  let open MachSize in
  match sz with
  | Byte  -> I_STLRBH (B,r1,r2)
  | Short -> I_STLRBH (H,r1,r2)
  | Word -> I_STLR (V32,r1,r2)
  | Quad -> I_STLR (V64,r1,r2)
  | S128 -> I_STLR (V128,r1,r2)

  let do_ldp opt r1 r2 rA = I_LDP (opt,vloc,r1,r2,rA,(0,Idx))
and do_ldxp opt r1 r2 rA = I_LDXP (vloc,opt,r1,r2,rA)
let do_stp opt r1 r2 rA = I_STP (opt,vloc,r1,r2,rA,(0,Idx))
and do_stxp opt r r1 r2 rA = I_STXP (vloc,opt,r,r1,r2,rA)

(*********)
(* loads *)
(*********)

module type L = sig
  type sz
  val sz0 : sz
  val load : sz -> state -> reg -> reg -> instruction list * state
  val load_idx : sz -> sz -> state -> reg -> reg -> reg -> instruction list * state
  val next_reg : state -> Cycle.proc -> sz -> reg * state
end

let type_of_sz sz =
  let open TypBase in
  Std (Unsigned,MachSize.at_least_word sz)

(*
let next_reg_sz st p sz =
  let r,st = next_reg st in
  let loc = A.Reg (p,r) in
  let st = A.add_type loc (type_of_sz sz) st in
  r,st

let next_reg_var st p var = next_reg_sz st p (v2sz var)

let emit_load_mixed sz o st p init x =
  let rA,st = next_reg_sz st p sz in
  let rB,init,st = U.next_init st p init x in
  rA,init,lift_code [ldr_mixed rA rB sz o],st

let _emit_load_int_idx o st  _p init rA =
  let r1,st = next_reg st in
  r1,init,lift_code [ldr_mixed r1 rA szloc o],st
*)