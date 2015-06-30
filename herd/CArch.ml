module Make (C:Arch.Config) (V:Value.S) = struct
  include CBase

   module V = V

    include ArchExtra.Make(C)        
	(struct
	  module V = V 

	  type arch_reg = reg
	  let pp_reg = pp_reg
	  let reg_compare = reg_compare

	  type arch_instruction = instruction
          let fromto_of_instr _ = None
	end)
  end
