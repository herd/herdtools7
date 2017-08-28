let comment = "#"

module Make(O:Arch_litmus.Config)(V:Constant.S) = struct

  include RISCVBase
  module V =
    struct
      type v = Constant.v
      include V
      let maybevToV c = c
    end

  let reg_to_string r = match r with
  | Symbolic_reg _ -> assert false
  | Ireg _ -> pp_reg r
      

  include
      ArchExtra_litmus.Make(O)
      (struct
        module V = V

        type arch_reg = reg
        let arch = `RISCV
        let forbidden_regs = []
        let pp_reg = pp_reg
        let reg_compare = reg_compare
        let reg_to_string = reg_to_string
        let internal_init _r = None
        let reg_class _ = "=&r"
        let comment = comment
        let error _t1 _t2 = false
      end)
end
