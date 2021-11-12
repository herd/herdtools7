module type S = sig
    type ifetch_instruction
    val is_overwritable : Label.Set.t -> ifetch_instruction -> bool
    val instruction_to_value : ifetch_instruction -> 'scalar Constant.t
end

module NotImplemented(I:sig type arch_instruction end) = struct
    type ifetch_instruction = I.arch_instruction

    let is_overwritable (_: Label.Set.t) (_:ifetch_instruction) : bool =
        Warn.fatal "FIXME: functionality not implemented for -variant self"

    let instruction_to_value (_ : ifetch_instruction) : 'scalar Constant.t =
        Warn.fatal "FIXME: functionality not implemented for -variant self"
end