(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2010-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
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
  val memory : Memory.t
  val cautious : bool
  val hexa : bool
end

module DefaultConfig = struct
  let memory = Memory.Direct
  let cautious = false
  let hexa = false
end


module type S = sig
  module V : Constant.S

  val comment : string
  type arch_reg

  type flow = Any | Next | Branch of string
  type ins =
      { memo:string ; inputs:arch_reg list ;  outputs:arch_reg list;
        reg_env: (arch_reg * CType.t) list; (* Register typing [ARMv8] *)
        (* Jumps *)
        label:string option ;  branch : flow list ;
        (* A la ARM conditional execution *)
        cond: bool ;
        comment: bool;
        clobbers: arch_reg list; }

  val empty_ins : ins
  val get_branch : ins -> flow list

  type t = {
      init : (arch_reg * V.v) list ;
      addrs : string list ; (* addesses in code (eg X86) *)
      stable : arch_reg list; (* stable registers, ie must be self-allocated by gcc *)
      final : arch_reg list ;
      code : ins list;
      name : Name.t ;
      all_clobbers : arch_reg list;
      nrets : int ; (* number of return instruction in code *)
      ty_env :  (arch_reg * CType.t) list ;
    }

  val get_nrets : t -> int
  val get_addrs : t -> string list
  val get_labels : t -> (int * string) list
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

  module RegSet : MySet.S with type elt = arch_reg
  module RegMap : MyMap.S with type key = arch_reg

  val all_regs : t -> RegSet.t
  val trashed_regs : t -> RegSet.t
  val get_reg_env :
      (CType.t -> CType.t -> bool) -> t -> CType.t RegMap.t
end

module Make(O:Config)(A:I) =
  struct

    module V = A.V

    open Printf
    open Constant

    let comment = A.comment

    type arch_reg = A.arch_reg

    type flow = Any | Next | Branch of string
    type ins =
        { memo:string ; inputs:arch_reg list ;  outputs:arch_reg list;
          reg_env: (arch_reg * CType.t) list; (* Register typing [ARMv8] *)
          (* Jumps *)
          label:string option ;  branch : flow list ;
          cond:bool ;
          comment:bool;
          clobbers: arch_reg list;
        }

    let empty_ins =
      { memo="" ; inputs=[]; outputs=[]; reg_env=[];
        label=None; branch=[Next]; cond=false; comment=false;
        clobbers=[]; }

    let get_branch  ins = ins.branch

    type t = {
        init : (arch_reg * V.v) list ;
        addrs : string list ;
        stable : arch_reg list;
        final : arch_reg list ;
        code : ins list;
        name : Name.t ;
        all_clobbers : arch_reg list;
        nrets : int ;
        ty_env :  (arch_reg * CType.t) list ;
      }


    let get_nrets t = t.nrets

    let get_addrs { init=init; addrs=addrs; _ } =
      let set =
        StringSet.union
          (StringSet.of_list addrs)
          (StringSet.of_list
             (List.fold_left
                (fun k (_,v) ->
                  match v with
                  | Symbolic ((s,_),_) -> s::k
                  | Concrete _|Label _|Tag _ -> k)
                [] init)) in
      StringSet.elements set

    let get_labels { init; _} =
      List.fold_left
        (fun k (_,v) ->
          match v with
          | Label (p,s) -> (p,s)::k
          | Concrete _|Symbolic _|Tag _ -> k)
        [] init

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
      sprintf "%%%s[%s]" (if w then "w" else "") (tag_reg reg)

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
        if 0 <= n && n <= 2 then n
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
                let w,ty,n,nxt =
                  match t.memo.[j+1] with
                  | 'w' -> true,t.memo.[j+2],digit (j+3),4
                  | _ -> false,t.memo.[j+1],digit (j+2),3 in
                begin match ty with
                | 'i' -> add (tag_reg_ref w (get_reg n t.inputs))
                | 'o' -> add (tag_reg_ref w (get_reg n t.outputs))
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

    module RegSet = A.RegSet
    module RegMap = A.RegMap

    let all_regs t =
      let all_ins ins =
        RegSet.union (RegSet.of_list (ins.inputs@ins.outputs)) in
      List.fold_right all_ins t.code  (RegSet.of_list t.final)


    let trashed_regs t =
      let trashed_ins ins = RegSet.union (RegSet.of_list ins.outputs) in
      let all_trashed =
        List.fold_right trashed_ins t.code RegSet.empty in
      RegSet.diff all_trashed
        (RegSet.union
           (RegSet.of_list t.final)
           (RegSet.of_list t.stable))

    let get_reg_env error tst =
      let m =
        List.fold_left (fun m (r,t) -> RegMap.add r t m)
          RegMap.empty tst.ty_env in
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
                      (A.reg_to_string r) (CType.dump t0) (CType.dump t)
                  end else
                    if not
                        ((CType.is_ptr t0 && CType.is_ptr t) ||
                        CType.same_base t0 t)
                    then
                        Warn.warn_always
                      "File \"%s\" Register %s has different types: <%s> and <%s>"
                      tst.name.Name.file
                      (A.reg_to_string r) (CType.dump t0) (CType.dump t)
                end ;
                RegMap.add r t m)
              m  t.reg_env)
          m tst.code in
      m

  end
