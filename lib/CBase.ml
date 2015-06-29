let arch = Archs.c

type reg = string

let parse_reg s = Some s
let pp_reg r = r
let reg_compare = String.compare

type loc = 
  | Local of reg
  | Shared of reg

type barrier = F

let pp_barrier b = assert false
let barrier_compare F F = 0

type 'k expression = 
  | Const of 'k
  | Load of loc
  | Op of Op.op * 'k expression * 'k expression

type 'k instr = 
  | Fence of barrier
  | Seq of 'k instr list
  | If of 'k expression * 'k instr * 'k instr option
  | Store of loc * 'k expression
  | Symb of string

type instruction = int instr

type parsedInstruction = MetaConst.k instr

let pp_instruction mode ins = assert false
let dump_instruction ins = assert false

let allowed_for_symb = []

let fold_regs _ = assert false
let map_regs _ = assert false
let fold_addrs _ = assert false
let map_addrs _ = assert false
let norm_ins _ = assert false
let get_next _ = assert false

include Pseudo.Make
	  (struct
	    type ins = instruction
	    type pins = parsedInstruction
	    type reg_arg = reg

	    let rec parsed_expr_tr = function
	      | Const c -> Const(MetaConst.as_int c)
	      | Load _ as l -> l
	      | Op(op,e1,e2) -> Op(op,parsed_expr_tr e1,parsed_expr_tr e2)

	    and parsed_tr = function
	      | Fence _ as f -> f
	      | Seq(li) -> Seq(List.map parsed_tr li)
	      | If(e,it,ie) -> 
		 let tr_ie = match ie with
		   | None -> None
		   | Some ie -> Some(parsed_tr ie) in
		 If(parsed_expr_tr e,parsed_tr it,tr_ie)
	      | Store(l,e) -> Store(l,parsed_expr_tr e)
	      | Symb _ -> Warn.fatal "No term variable allowed"

	    let get_naccesses ins = 0
	    let fold_labels acc f ins = acc
	    let map_labels f ins = ins
	  end)

let get_macro s = assert false
