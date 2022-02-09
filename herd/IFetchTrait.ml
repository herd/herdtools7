module type S = sig
    type ifetch_instruction
    type ifetch_reg
    val is_link : ifetch_instruction -> ifetch_reg option
    val is_overwritable : ifetch_instruction -> bool
    val instruction_to_value : ifetch_instruction -> ('scalar,'pte) Constant.t
end

module NotImplemented(I:sig type arch_instruction type arch_reg end) :
S with type ifetch_instruction = I.arch_instruction
     and type ifetch_reg = I.arch_reg
  = struct

  type ifetch_instruction = I.arch_instruction

  type ifetch_reg = I.arch_reg

  let is_link _ = None

  let is_overwritable _ =
    Warn.fatal "Functionality not implemented for -variant self"

  let instruction_to_value _  =
    Warn.fatal "Functionality not implemented for -variant self"
end
