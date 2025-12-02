let parse_barrier : string -> AArch64Base.barrier option = function
  | "DMB.ISH" -> Some (DMB (ISH, FULL))
  | "DMB.ISHLD" -> Some (DMB (ISH, LD))
  | "DMB.ISHST" -> Some (DMB (ISH, ST))
  | "DMB.OSH" -> Some (DMB (OSH, FULL))
  | "DMB.OSHLD" -> Some (DMB (OSH, LD))
  | "DMB.OSHST" -> Some (DMB (OSH, ST))
  | "DMB.SY" -> Some (DMB (SY, FULL))
  | "DMB.LD" -> Some (DMB (SY, LD))
  | "DMB.ST" -> Some (DMB (SY, ST))
  | "DSB.ISH" -> Some (DSB (ISH, FULL))
  | "DSB.ISHLD" -> Some (DSB (ISH, LD))
  | "DSB.ISHST" -> Some (DSB (ISH, ST))
  | "DSB.OSH" -> Some (DSB (OSH, FULL))
  | "DSB.OSHLD" -> Some (DSB (OSH, LD))
  | "DSB.OSHST" -> Some (DSB (OSH, ST))
  | "DSB.SY" -> Some (DSB (SY, FULL))
  | "DSB.LD" -> Some (DSB (SY, LD))
  | "DSB.ST" -> Some (DSB (SY, ST))
  | "ISB" -> Some ISB
  | _ -> None

let pp_barrier (b : AArch64Base.barrier) : string = AArch64Base.pp_barrier_dot b

let pp_dp : Dep.dp -> string = function
  | ADDR -> "addr"
  | DATA -> "data"
  | CTRL -> "ctrl"
  | CTRLISYNC -> "ctrlisb"
