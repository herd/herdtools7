include MakeAArch64Base.Make(struct let is_morello = false end)

let next_reg (proc: Cycle.proc) : reg =
  let _ = proc in ZR (* TODO *)