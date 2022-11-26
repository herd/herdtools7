module type S = sig
    type ifetch_instruction
    type ifetch_reg
    val is_link : ifetch_instruction -> ifetch_reg option
end

module NotImplemented(I:sig type arch_instruction type arch_reg end) :
S with type ifetch_instruction = I.arch_instruction
     and type ifetch_reg = I.arch_reg
  = struct

  type ifetch_instruction = I.arch_instruction

  type ifetch_reg = I.arch_reg

  let is_link _ = None

end
