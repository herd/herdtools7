(*********************************************************************)
(*                          Litmus                                   *)
(*                                                                   *)
(*        Luc Maranget, INRIA Paris-Rocquencourt, France.            *)
(*        Susmit Sarkar, University of Cambridge, UK.                *)
(*                                                                   *)
(*  Copyright 2010 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

module type I = sig
  type arch_reg
  val arch : Archs.t
  val reg_compare : arch_reg -> arch_reg -> int
  val reg_to_string : arch_reg -> string
(* gas line comment char *)
  val comment : char
end


exception Error of string

module type Config = sig
  val memory : Memory.t
  val cautious : bool
end

module type S = sig

  type arch_reg

  type flow = Next | Branch of string
  type ins =
      { memo:string ; inputs:arch_reg list ;  outputs:arch_reg list;
        (* Jumps *)
        label:string option ;  branch : flow list ;
        (* A la ARM conditional execution *)
        cond: bool ;
        comment: bool; }

  val empty_ins : ins

  type t = {
      init : (arch_reg * Constant.v) list ;
      addrs : string list ; (* addesses in code (eg X86) *)
      stable : arch_reg list; (* stable registers, ie must be self-allocated by gcc *)
      final : arch_reg list ;
      code : ins list;
    }
  val get_addrs : t -> string list
  val fmt_reg : arch_reg -> string
  val dump_label : string -> string
  val dump_out_reg : int -> arch_reg -> string
  val dump_v : Constant.v -> string
  val addr_cpy_name : string -> int -> string

  val clean_reg : string -> string
  val tag_reg : arch_reg -> string


  val to_string : ins -> string
  val compile_out_reg : int -> arch_reg -> string
  val compile_presi_out_reg : int -> arch_reg -> string
  val compile_presi_out_ptr_reg : int -> arch_reg -> string
  val dump_type : ('a * CType.t) list -> 'a -> string
end

module Make(O:Config) (A:I) (V:Constant.S) =
struct
  open Printf
  open Constant

  type arch_reg = A.arch_reg

  type flow = Next | Branch of string
  type ins =
      { memo:string ; inputs:arch_reg list ;  outputs:arch_reg list;
        (* Jumps *)
        label:string option ;  branch : flow list ;
        cond:bool ;
        comment:bool;}

  let empty_ins =
    { memo="" ; inputs=[]; outputs=[];
      label=None; branch=[Next]; cond=false; comment=false;}

  type t = {
      init : (arch_reg * Constant.v) list ;
      addrs : string list ;
      stable : arch_reg list;
      final : arch_reg list ;
      code : ins list;
    }


  let get_addrs { init=init; addrs=addrs; _ } =
    let set =
      StringSet.union
        (StringSet.of_list addrs)
        (StringSet.of_list
           (List.fold_left
              (fun k (_,v) ->
                match v with Symbolic s -> s::k
                | Concrete _ -> k)
              [] init)) in
    StringSet.elements set

  let get_stable { stable; _} = stable

  exception Internal of string
  let internal msg = raise (Internal msg)

  exception Error of string
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

  let clean_reg s =
    Misc.map_string
      (fun c -> match c with
      | '%' -> ""
      | '$' -> "r"
      | _  -> String.make 1 c)
      s

  let tag_reg reg = clean_reg (A.reg_to_string reg)

  let tag_reg_ref reg = sprintf "%%[%s]" (tag_reg reg)

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
      else internal (sprintf "bad digit '%i'" n)

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
          let n = digit (j+2) in
          begin match t.memo.[j+1] with
          | 'i' -> add (tag_reg_ref (get_reg n t.inputs))
          | 'o' -> add (tag_reg_ref (get_reg n t.outputs))
          | c -> internal (sprintf "bad escape '%c'" c)
          end ;
          do_rec (j+3)
        with Not_found -> add (substring i len) in
    try
      if t.comment then sprintf "%c%s" A.comment (escape_percent t.memo)
      else begin
        do_rec 0  ; Buffer.contents b
      end
    with Internal msg ->
      error (sprintf "memo: %s, error: %s" t.memo msg)

  let dump_type env reg =
    try CType.dump (List.assoc reg env) with
      | Not_found -> "int"


  include OutUtils.Make(O)

end
