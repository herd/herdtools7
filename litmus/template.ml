(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2010-present Institut National de Recherche en Informatique et *)
(* en Automatique, ARM Ltd and the authors. All rights reserved.            *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

let debug = false

module type I = sig
  module V : Constant.S
  type arch_reg
  module RegSet : MySet.S with type elt = arch_reg
  module RegMap : MyMap.S with type key = arch_reg
  val arch : Archs.t
  val reg_compare : arch_reg -> arch_reg -> int
  val reg_to_string : arch_reg -> string
(* gas line comment char *)
  val comment : string
end


exception Error of string

module type Config = sig
  val verbose : int
  val memory : Memory.t
  val cautious : bool
  val hexa : bool
  val mode : Mode.t
end

module DefaultConfig = struct
  let verbose = 0
  let memory = Memory.Direct
  let cautious = false
  let hexa = false
  let mode = Mode.Std
end

type extra_args =
  { trashed: string list;
    inputs: ((CType.t * string) list * (string * string)) list;
    constants: (string * string) list;
    clobbers: string list;
    externs : (CType.t * string) list; }

let no_extra_args =
  { trashed=[]; inputs=[]; constants=[]; clobbers=[]; externs=[]; }

module type S = sig
  module V : Constant.S

  val comment : string
  type arch_reg
  module RegSet : MySet.S with type elt = arch_reg
  module RegMap : MyMap.S with type key = arch_reg

  type flow = Any | Next | Branch of string | Disp of int

  val add_next : flow -> flow list

  type ins =
      { memo:string ; inputs:arch_reg list ;  outputs:arch_reg list;
        reg_env: (arch_reg * CType.t) list; (* Register typing [ARMv8] *)
        (* Jumps *)
        label:string option ;  branch : flow list ;
        (* Memo contains align derective *)
        align: bool;
        (* A la ARM conditional execution *)
        cond: bool ;
        comment: bool;
        clobbers: arch_reg list; }

  val empty_ins : ins
  val get_branch : ins -> flow list

  type t = {
      init : (arch_reg * V.v) list ;
      addrs : string list ; (* addesses in code (eg X86) *)
      ptes : string list ;  (* pte in code (eg X86) *)
      stable : arch_reg list; (* stable registers, ie must be self-allocated by gcc *)
      final : arch_reg list ;
      code : ins list;
      fhandler : ins list ;
      name : Name.t ;
      all_clobbers : arch_reg list;
      nrets : int ; (* number of return instruction in code *)
      nnops : int ; (* number of nop instruction in code *)
      ty_env :  CType.t RegMap.t ;
      code_ty_env :  CType.t RegMap.t ;
    }

  val get_nrets : t -> int
  val get_nnops : t -> int
  val has_asmhandler : t -> bool
  val get_addrs_only : t -> string list
  val get_phys_only : t -> string list
  val get_addrs : t -> string list * string list (* addresses X ptes *)
  val get_labels : t -> Label.Full.full list
  val get_instructions : t -> V.Instr.t list
  val fmt_reg : arch_reg -> string
  val dump_label : string -> string
  val emit_label : (string -> string) -> string -> ins
  val dump_out_reg : int -> arch_reg -> string
  val dump_v : V.v -> string
  val dump_init_val : V.v -> string
  val addr_cpy_name : string -> int -> string

  val clean_reg : string -> string
  val tag_reg : arch_reg -> string


  val to_string : ins -> string
  val compile_out_reg : int -> arch_reg -> string
  val compile_presi_out_reg : int -> arch_reg -> string
  val compile_presi_out_ptr_reg : int -> arch_reg -> string

  val all_regs : ins list -> ins list -> arch_reg list -> RegSet.t
  val all_regs_in_tmpl : t -> RegSet.t
  val trashed_regs : t -> RegSet.t
  val get_reg_env :
      (CType.t -> CType.t -> bool)-> (* fail *)
        (CType.t -> CType.t -> bool)->  (* warn *)
          t -> CType.t RegMap.t

  val has_fault_handler : t -> bool
  val find_offset : Label.t -> t -> int

end

module Make(O:Config)(A:I) =
  struct

    module V = A.V

    open Printf
    open Constant

    let comment = A.comment

    type arch_reg = A.arch_reg
    module RegSet = A.RegSet
    module RegMap = A.RegMap

    type flow = Any | Next | Branch of string | Disp of int

    let add_next b = match b with
      | Next|Any-> [b;]
      | Branch _|Disp _ -> [Next; b;]

    type ins =
        { memo:string ; inputs:arch_reg list ;  outputs:arch_reg list;
          reg_env: (arch_reg * CType.t) list; (* Register typing [ARMv8] *)
          (* Jumps *)
          label:string option ;  branch : flow list ;
          align:bool;
          cond:bool ;
          comment:bool;
          clobbers: arch_reg list;
        }

    let empty_ins =
      { memo="" ; inputs=[]; outputs=[]; reg_env=[];
        label=None; branch=[Next]; align=false; cond=false; comment=false;
        clobbers=[]; }

    let get_branch  ins = ins.branch

    type t = {
        init : (arch_reg * V.v) list ;
        addrs : string list ;
        ptes : string list ;
        stable : arch_reg list;
        final : arch_reg list ;
        code : ins list;
        fhandler : ins list ;
        name : Name.t ;
        all_clobbers : arch_reg list;
        nrets : int ; nnops : int ;
        ty_env : CType.t RegMap.t ;
        code_ty_env :  CType.t RegMap.t ;
      }


    let get_nrets t = t.nrets
    and get_nnops t = t.nnops
    and has_asmhandler t = Misc.consp t.fhandler

    (* Generic function to extract some symbols *)
    let get_gen tr init addrs =
      let set =
        StringSet.union
          (StringSet.of_list addrs)
          (StringSet.of_list
             (List.fold_left
                (fun k (_,v) ->
                  let rec f v k = match v with
                  | Symbolic _ when is_label v -> k
                  | Symbolic sym ->
                      begin match tr sym with
                      | Some s -> s::k
                      | None -> k
                      end
                  | ConcreteVector vs ->
                      List.fold_right f vs k
                  | ConcreteRecord vs ->
                    StringMap.fold_values f vs k
                  |Concrete _|Tag _
                  |PteVal _|AddrReg _|Instruction _|Frozen _
                   -> k in
                  f v k)
                [] init)) in
      StringSet.elements set

    let get_addrs_only {init; addrs; _} =
      get_gen
        (function
          | Virtual {Constant.name=Symbol.Data s;_} -> Some s
          | _ -> None)
        init addrs

    let get_ptes_only {init; ptes; _} =
      get_gen
        (function
          | System (PTE,s) -> Some s
          | _ -> None)
        init ptes

    let get_phys_only {init; _} =
      get_gen
        (function
          | Physical (s,_) -> Some s
          | _ -> None)
        init []

    let get_addrs t =
      get_addrs_only t,get_ptes_only t

    let get_constants get {init;_} =
      let rec f v k = match v with
        | ConcreteVector vs ->
           List.fold_right f vs k
        | _ ->
           begin
             match get v with
             | Some r -> r::k
             | None -> k
           end in
      List.fold_left (fun k (_,v) -> f v k) [] init

    let list_unique lst =
      List.fold_left
        (fun l e -> if List.exists (fun elt -> elt = e) l then l else e::l)
        [] lst

    let get_labels t =
      let lbls = get_constants
        (function
         | Symbolic (Virtual {Constant.name=Symbol.Label (p,s); _}) -> Some (p,s)
         | _ -> None)
        t in
      list_unique lbls

    let get_instructions t =
      let insts = get_constants
        (function
         | Instruction i -> Some i
         | _ -> None)
        t in
      list_unique insts

    let get_stable { stable; _} = stable

    exception Internal of string
    let internal msg = raise (Internal msg)

    let error msg = raise (Error msg)


    let escape_percent s =
      Misc.map_string
        (fun c -> match c with
        | '%' -> "%%"
        | '$' -> "r"
        | _ -> String.make 1 c)
        s

    let pp_reg r = escape_percent (A.reg_to_string r)
    let fmt_reg = pp_reg

    let dump_label lbl = lbl

    let emit_label tr lbl =
      { empty_ins with
        memo=sprintf "%s:" (dump_label (tr lbl)) ;
        label = Some lbl ; branch=[Next] ; }

    let clean_reg s =
      Misc.map_string
        (fun c -> match c with
        | '%' -> ""
        | '$' -> "r"
        | _  -> String.make 1 c)
        s

    let tag_reg reg = clean_reg (A.reg_to_string reg)

    let tag_reg_ref w reg =
      sprintf "%%%s[%s]" (match w with Some c -> String.make 1 c | None -> "") (tag_reg reg)

    let dump_out_reg proc reg =
      OutUtils.fmt_out_reg
        proc
        (clean_reg (A.reg_to_string reg))

    let compile_out_reg proc reg =
      OutUtils.fmt_index (dump_out_reg proc reg)

    let compile_presi_out_reg proc reg =
      OutUtils.fmt_presi_index (dump_out_reg proc reg)

    let compile_presi_out_ptr_reg proc reg =
      OutUtils.fmt_presi_ptr_index (dump_out_reg proc reg)

    let get_reg k rs =
      try List.nth rs k
      with _ ->
        internal
          (sprintf "get_reg %i in {%s}"
             k (String.concat ","
                  (List.map pp_reg rs)))

    let escape_percent s =
      let len = String.length s in
      let buff = Buffer.create 16 in
      let rec do_rec i =
        if i < len then begin
          begin match s.[i] with
          | '%' -> Buffer.add_string buff "%%"
          | c -> Buffer.add_char buff c
          end ;
          do_rec (i+1)
        end in
      do_rec 0 ; Buffer.contents buff

    let to_string t =

      let digit i =
        let c = Char.code t.memo.[i] in
        let n = c - Char.code '0' in
        if 0 <= n && n <= 6 then n
        else internal (sprintf "bad digit '%i' (%c)" n t.memo.[i])

      and substring i j =
        try String.sub t.memo i (j-i)
        with _ -> internal (sprintf "substring %i-%i" i j)

      and look_escape i =
        try String.index_from t.memo i '^'
        with
        | Not_found -> raise Not_found
        | _ -> internal (sprintf "look_escape %i" i) in


      let b = Buffer.create 20 in
      let add = Buffer.add_string b in
      let len = String.length t.memo in

      let rec do_rec i =
        if i < len then
          try
            let j = look_escape i in
            add (substring i j) ;
            match t.memo.[j+1] with
            | '^' ->
                add "^" ;
                do_rec (j+2)
            | _ ->
                let c,ty,n,nxt =
                  match t.memo.[j+1] with
                  | 'w' | 'b' | 'h' | 's' | 'd' | 'q'
                    -> Some t.memo.[j+1],t.memo.[j+2],digit (j+3),4
                  | _ -> None ,t.memo.[j+1],digit (j+2),3 in
                begin match ty with
                | 'i' -> add (tag_reg_ref c (get_reg n t.inputs))
                | 'o' -> add (tag_reg_ref c (get_reg n t.outputs))
                | c -> internal (sprintf "bad escape '%c'" c)
                end ;
                do_rec (j+nxt)
          with Not_found -> add (substring i len) in
      try
        if t.comment then sprintf "%s%s" A.comment (escape_percent t.memo)
        else begin
          do_rec 0  ; Buffer.contents b
        end
      with Internal msg ->
        error (sprintf "memo: %s, error: %s" t.memo msg)

    include OutUtils.Make(O)(V)

    let dump_init_val = dump_v

    let all_regs code fhandler final =
      let all_ins ins =
        RegSet.union (RegSet.of_list (ins.inputs@ins.outputs)) in
      let k = List.fold_right all_ins code  (RegSet.of_list final) in
      List.fold_right all_ins fhandler  k

    let all_regs_in_tmpl t = all_regs t.code t.fhandler t.final

    let trashed_code code =
      List.map
        (fun ins -> RegSet.of_list ins.outputs)
        code
      |> RegSet.unions

    let trashed_regs t =
      RegSet.diff
        (RegSet.union
           (trashed_code t.code)
           (trashed_code t.fhandler))
        (RegSet.union
           (RegSet.of_list t.final)
           (RegSet.of_list t.stable))

    let get_reg_env error warn tst =
      let pp_type t =
        match t with
        | CType.Array _ -> CType.debug t
        | _ -> CType.dump t in
      let m = tst.ty_env in
      let m =
        List.fold_left
          (fun m t ->
            List.fold_left
              (fun m (r,t) ->
                let t0 = RegMap.safe_find t r m in
                if (t <> t0) then begin
                  if error t0 t then begin
                    Warn.user_error
                      "Register %s has different types: <%s> and <%s>"
                      (A.reg_to_string r)
                      (pp_type t0) (pp_type t)
                  end else
                    if warn t0 t
                    then
                        Warn.warn_always
                      "File \"%s\" Register %s has different types: <%s> and <%s>"
                      tst.name.Name.file
                      (A.reg_to_string r)
                      (pp_type t0) (pp_type t)
                end ;
                let tfinal = CType.larger t0 t in
                RegMap.add r tfinal m)
              m  t.reg_env)
          m tst.code in
      m

    let has_fault_handler t = t.fhandler <> []

    type r = Ok of int | No of int

    let rec find_offset_code lbl k code =
      match code with
      | [] -> No k
      | { label=Some lbl0; _}::code ->
         if Label.equal lbl0 lbl then Ok k
         else find_offset_code lbl k code
      | { label=None; align; _}::code ->
         let k = if align then k else k+1 in
         find_offset_code lbl k code

    let find_offset lbl t =
      match find_offset_code lbl 0 t.code with
      | Ok off -> off
      | No sz ->
         match find_offset_code lbl sz t.fhandler with
         | Ok off -> off
         | No _ -> raise Not_found
  end
