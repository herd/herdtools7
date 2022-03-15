(** Define Java architecture *)

module Make (C:Arch_herd.Config) (V:Value.S) = struct
  
  include JavaBase

  let is_amo _            = false
  let pp_barrier_short    = pp_barrier
  let reject_mixed        = false
  let mem_access_size _   = None

  include NoSemEnv

  module V = V

  include NoLevelNorTLBI

    include
      IFetchTrait.NotImplemented
        (struct
          type arch_instruction = instruction
          type arch_reg = reg
        end)


  include ArchExtra_herd.Make(C)
      (struct
        module V = V
        let endian            = endian
        type arch_reg         = reg
        let pp_reg            = pp_reg
        let reg_compare       = reg_compare
        type arch_instruction = instruction
        let fromto_of_instr _ = None
        let get_val _ v       = v
      end)
  
  module MemType=MemoryType.No
  module Barrier = AllBarrier.No(struct type a = barrier end)
end