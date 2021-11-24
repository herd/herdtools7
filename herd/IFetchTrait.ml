module type S = sig
    type ifetch_instruction
    val is_overwritable : ifetch_instruction -> bool
    val instruction_to_value : ifetch_instruction -> ('scalar,'pte) Constant.t
end

module NotImplemented(I:sig type arch_instruction end) :
S with type ifetch_instruction = I.arch_instruction
  = struct
    type ifetch_instruction = I.arch_instruction

    let is_overwritable _ =
        Warn.fatal "Functionality not implemented for -variant self"

    let instruction_to_value _  =
        Warn.fatal "Functionality not implemented for -variant self"
end
