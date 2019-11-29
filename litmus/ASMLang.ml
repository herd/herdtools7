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

module type Config = sig
  val memory : Memory.t
  val cautious : bool
  val mode : Mode.t
  val asmcommentaslabel : bool
end

module type I = sig
  include Template.I
(* Forbidden registers *)
  val forbidden_regs :  arch_reg list
(* Initial value of internal registers *)
  val internal_init : arch_reg -> (string * string) option
(* gcc assembly template register class *)
  val reg_class : arch_reg -> string
  val reg_class_stable : arch_reg -> string
(* type errors *)
  val error : CType.t -> CType.t  -> bool
end

module Make
    (O:Config)
    (A:I)
    (Tmpl:Template.S with
     module V = A.V and
type arch_reg = A.arch_reg and
module RegSet = A.RegSet and
module RegMap = A.RegMap)
    (AL:Arch_litmus.S)
    = struct

      type arch_reg = Tmpl.arch_reg
      type t = Tmpl.t

      open Printf

      let checkVal f v = f v

      let compile_addr_inline = match O.mode with
      | Mode.Std -> sprintf "_a->%s[_i]"
      | Mode.PreSi -> sprintf "*%s"

      and compile_addr_fun x = sprintf "*%s" x

      and compile_val_inline = match O.mode with
      | Mode.Std -> checkVal Tmpl.dump_v
      | Mode.PreSi -> checkVal A.V.pp_v

      module RegSet = Tmpl.RegSet
      module RegMap = Tmpl.RegMap

      let dump_clobbers chan t =
        fprintf chan ":%s\n"
          (String.concat ","
             (List.map (fun s -> sprintf "\"%s\"" s)
                ("cc"::"memory"::
                 List.map A.reg_to_string
                   (t.Tmpl.all_clobbers@A.forbidden_regs))))

      let copy_name s = sprintf "_tmp_%s" s

      let tag_reg_def reg = sprintf "[%s]" (Tmpl.tag_reg reg)

      let dump_trashed_reg reg =
        sprintf "trashed_%s"
          (Tmpl.clean_reg (A.reg_to_string reg))

      let dump_stable_reg reg =
        sprintf "stable_%s"
          (Tmpl.clean_reg (A.reg_to_string reg))

      let init_val reg test =
        try Some (List.assoc reg test.Tmpl.init)
        with Not_found -> None

      let strip_equal s =
        let explode s =
          let rec expl i l =
            if i < 0 then l else
              expl (i - 1) (s.[i] :: l) in
          expl (String.length s - 1) [] in
        let implode l =
          let result = String.make (List.length l) 'c' in
          String.mapi (fun i _ -> List.nth l i) result in
        let rec seq s =
          match s with
          | [] -> []
          | c::r when c = '&' || c ='=' -> seq r
          | c::r -> c :: seq r in
        implode (seq (explode s))

      let dump_inputs compile_val chan t trashed =
        let stable = RegSet.of_list t.Tmpl.stable in
        let all = Tmpl.all_regs t in
        let init_set =
            (List.fold_right
               (fun (reg,_) -> RegSet.add reg) t.Tmpl.init RegSet.empty) in
        let in_outputs =
          RegSet.unions [trashed;stable;RegSet.of_list t.Tmpl.final] in
(*
  let pp_reg chan r = fprintf chan "%s" (A.reg_to_string r) in
  eprintf "Trashed in In: %a\n"
  (fun chan rs -> RegSet.pp chan "," pp_reg rs)
  trashed ;
  eprintf "Outputs in In: %a\n"
  (fun chan rs -> RegSet.pp chan "," pp_reg rs)
  in_outputs ;
 *)
        let dump_pair reg v =
          let dump_v = compile_val in
          let dump_v = (* catch those addresses that are saved in a variable *)
            if O.cautious then
              (fun v -> match v with
              | Constant.Symbolic _ -> copy_name (Tmpl.tag_reg reg)
              | _ -> dump_v v)
            else dump_v  in
          if RegSet.mem reg in_outputs then begin
            match A.internal_init reg with
            | None -> sprintf "\"%s\" (%s)" (tag_reg_def reg) (dump_v v)
            | Some (s,_) -> sprintf "\"%s\" (%s)" (tag_reg_def reg) s
          end else match A.internal_init reg with
          | None ->
              sprintf "%s \"%s\" (%s)" (tag_reg_def reg) (strip_equal (A.reg_class reg)) (dump_v v)
          | Some (s,_) ->
              sprintf "%s \"%s\" (%s)" (tag_reg_def reg) (strip_equal (A.reg_class reg)) s in

        (* Input from state *)
        let ins =
          List.map
            (fun (reg,v) ->  dump_pair reg v)
            (List.filter
               (fun (r,_) -> not (RegSet.mem r stable))
               t.Tmpl.init) in
        (* All other inputs, apparently needed to get gcc to
           allocate registers avoiding all registers in template *)
        let rem =
          RegSet.diff
            (RegSet.diff all stable)
            (RegSet.unions [in_outputs;init_set]) in
        let rem =
          if AL.arch = `X86_64 then
            RegSet.unions [rem;(RegSet.diff all init_set)]
          else rem in
        let rem =
          RegSet.fold
            (fun reg k ->
              let v = A.V.zero in
              dump_pair reg v::k)
            rem [] in
        let out =  (String.concat "," (ins@rem)) in
(*        eprintf "IN: {%s}\n" out ; *)
        fprintf chan ":%s\n" out

      let (@@) f k  = f k

      let debug = false

      let pp_regs rs = String.concat "," (List.map A.reg_to_string (RegSet.elements rs))

      let dump_outputs compile_addr compile_out_reg chan proc t trashed =
        let stable = RegSet.of_list t.Tmpl.stable in
        let final = RegSet.of_list t.Tmpl.final in
        if debug then
          eprintf "P%i: stable={%s}, final={%s}, all={%s}\n"
            proc (pp_regs stable) (pp_regs final) (pp_regs (Tmpl.all_regs t));
        let outs =
          String.concat ","
            (List.fold_right
               (match O.memory with
               | Memory.Direct ->
                   (fun a k -> sprintf "[%s] \"=m\" (%s)" a (compile_addr a)::k)
               | Memory.Indirect ->
                   (fun a k -> sprintf "[%s] \"=m\" (*%s)" a
                       (compile_addr a)::k))
               t.Tmpl.addrs
             @@RegSet.fold
                  (fun reg k ->
                    (if O.cautious then
                      sprintf "%s \"%s\" (%s)"
                        (tag_reg_def reg)
                        (A.reg_class reg)
                        (copy_name (Tmpl.dump_out_reg proc reg))
                    else if RegSet.mem reg stable then
                      sprintf "%s \"%s\" (%s)"
                        (tag_reg_def reg)
                        (A.reg_class_stable reg)
                        (dump_stable_reg reg)
                    else
                      sprintf "%s \"%s\" (%s)"
                        (tag_reg_def reg)
                        (A.reg_class reg)
                        (compile_out_reg proc reg))::k)
                  final
             @@RegSet.fold
                  (fun reg k ->
                    sprintf "%s \"%s\" (%s)"
                      (tag_reg_def reg)
                      (A.reg_class reg)
                      (dump_trashed_reg reg)::k)
                  trashed
             @@RegSet.fold
                  (fun reg k ->
                    sprintf "%s \"%s\" (%s)"
                      (tag_reg_def reg)
                      (A.reg_class_stable reg)
                      (dump_stable_reg reg)::k)
                  (RegSet.diff stable final) []) in
        fprintf chan ":%s\n" outs

      let dump_copies
          compile_out_reg compile_val compile_cpy chan indent env proc t =
(*
  List.iter
  (fun (_,a) ->
  fprintf chan "%sint *%s = %s;\n" indent (copy_name a)
  (match O.memory with
  | Memory.Direct -> sprintf "&%s[_i]" a
  | Memory.Indirect -> sprintf "%s[_i]" a) ;
  fprintf chan "%smbar();\n" indent)
  t.addrs ;
 *)
        List.iter
          (fun reg ->
            fprintf chan "%s%s %s = %s;\n" indent
              (let ty =
                RegMap.safe_find Compile.base reg env in
              CType.dump ty)
              (copy_name (Tmpl.dump_out_reg proc reg))
              (compile_out_reg proc reg) ;
            fprintf chan "%smcautious();\n" indent)
          t.Tmpl.final ;
        begin match O.memory with
        | Memory.Indirect ->
            List.iter
              (fun (reg,v) -> match v with
              | Constant.Symbolic ((a,None),_) ->
                  let cpy =  copy_name (Tmpl.tag_reg reg) in
                  fprintf chan "%svoid *%s = %s;\n" indent
                    cpy
                    (compile_val v) ;
                  fprintf chan "%s%s = %s;\n" indent
                    (compile_cpy a)  cpy ;
                  fprintf chan "%smcautious();\n" indent
              | _ -> ())
              t.Tmpl.init
        | Memory.Direct ->
            List.iter
              (fun (reg,v) -> match v with
              | Constant.Symbolic _ ->
                  let cpy =  copy_name (Tmpl.tag_reg reg) in
                  fprintf chan "%svoid *%s = %s;\n" indent
                    cpy
                    (compile_val v) ;
              |  _ -> ())
              t.Tmpl.init
        end ;
        ()

      let dump_save_copies compile_out_reg chan indent proc t =
        List.iter
          (fun reg ->
            fprintf chan "%smcautious();\n" indent ;
            fprintf chan "%s%s = %s;\n" indent
              (compile_out_reg proc reg)
              (copy_name (Tmpl.dump_out_reg proc reg)))
          t.Tmpl.final ;
        ()

      let after_dump compile_out_reg chan indent proc t =
        if O.cautious then begin
          dump_save_copies compile_out_reg chan indent proc t
        end ;
        let stable = RegSet.of_list t.Tmpl.stable
        and finals = RegSet.of_list t.Tmpl.final in
        RegSet.iter
          (fun reg ->
            fprintf chan "%s%s = %s;\n" indent
              (compile_out_reg proc reg) (dump_stable_reg reg))
          (RegSet.inter stable finals)

      let before_dump compile_out_reg compile_val compile_cpy
          chan indent env proc t trashed =

        let reg_env = Tmpl.get_reg_env A.error t in
        RegSet.iter
          (fun reg ->
            let ty = match A.internal_init reg with
            | Some (_,ty) -> ty
            | None -> CType.dump (RegMap.safe_find CType.word reg reg_env) in
            fprintf chan "%s%s %s;\n"
              indent ty (dump_trashed_reg reg))
          trashed ;
        List.iter
          (fun reg ->
            let ty =
              try RegMap.find reg env
              with Not_found -> Compile.base in
            fprintf chan "%sregister %s %s asm(\"%s\")%s;\n"
              indent (CType.dump ty) (dump_stable_reg reg)
              (A.reg_to_string reg)
              (match init_val reg t with
              | None -> ""
              | Some v -> sprintf " = %s" (compile_val v)))
          t.Tmpl.stable ;

        if O.cautious then begin
          dump_copies compile_out_reg compile_val compile_cpy chan
            indent env proc t
        end

      let do_dump compile_val compile_addr compile_cpy compile_out_reg
          chan indent env proc t =
        let rec dump_ins k ts = match ts with
        | [] -> ()
        | t::ts ->
            begin if not O.asmcommentaslabel then
              match t.Tmpl.label with
              | Some _ ->
                  fprintf chan "\"%s_litmus_P%i_%i\\n\"\n" Tmpl.comment proc k
              | None ->
                  fprintf chan "\"%s_litmus_P%i_%i\\n%s\"\n"
                    Tmpl.comment proc k
                    (if t.Tmpl.comment then "" else "\\t")
            end ;
            fprintf chan "\"%s\\n\"\n" (Tmpl.to_string t) ;
(*
  fprintf chan "\"%-20s%c_litmus_P%i_%i\\n\\t\"\n"
  (to_string t) A.comment proc k ;
 *)
            dump_ins (k+1) ts in
        let trashed = Tmpl.trashed_regs t in
        before_dump
          compile_out_reg compile_val compile_cpy chan indent env proc t trashed;
        fprintf chan "asm __volatile__ (\n" ;
        fprintf chan "\"\\n\"\n" ;
        begin if O.asmcommentaslabel then
          fprintf chan "\"%s:\\n\"\n"
            (LangUtils.start_label proc)
        else
          fprintf chan "\"%s\\n\"\n"
            (LangUtils.start_comment Tmpl.comment proc)
        end ;
        begin match t.Tmpl.code with
        | [] -> fprintf chan "\"\"\n"
        | code -> dump_ins 0 code
        end ;
        begin if O.asmcommentaslabel then
          fprintf chan "\"%s:\\n\"\n"
            (LangUtils.end_label proc)
        else
          fprintf chan "\"%s\\n\\t\"\n"
            (LangUtils.end_comment Tmpl.comment proc)
        end ;
        dump_outputs compile_addr compile_out_reg chan proc t trashed ;
        dump_inputs compile_val chan t trashed ;
        dump_clobbers chan t  ;
        fprintf chan ");\n" ;
        after_dump compile_out_reg chan indent proc t;
        ()

      let debug_globEnv e =
        let pp =
          List.map
            (fun (a,ty) ->
              sprintf "%s -> %s" a (SkelUtil.dump_global_type a ty))
            e in
        eprintf "ENV: [%s]\n"
          (String.concat " " pp)


      let dump chan indent env globEnv _volatileEnv proc t =

        if debug then debug_globEnv globEnv ;

        let compile_out_reg = match O.mode with
        | Mode.Std -> Tmpl.compile_out_reg
        | Mode.PreSi ->
            fun proc reg ->
              let ty =
                try RegMap.find reg env with Not_found -> assert false in
              if CType.is_ptr ty then
                Tmpl.compile_presi_out_ptr_reg proc reg
              else
                Tmpl.compile_presi_out_reg proc reg in

        do_dump
          compile_val_inline compile_addr_inline
          (fun x -> sprintf "_a->%s[_i]" (Tmpl.addr_cpy_name x proc))
          compile_out_reg
          chan indent env proc t


      let compile_val_fun =
        let open Constant in
        fun v -> match v with
        | Symbolic ((s,None),_) ->
            sprintf "%s%s"
              (match O.memory with Memory.Direct -> "" | Memory.Indirect -> "*")
              s
        | Concrete _ -> Tmpl.dump_v v
        | Label (p,lbl) -> OutUtils.fmt_lbl_var p lbl
        | Symbolic _|Tag _ -> assert false

      let compile_init_val_fun = compile_val_fun

      let compile_cpy_fun proc a = sprintf "*%s" (Tmpl.addr_cpy_name a proc)

      let dump_fun chan env globEnv _volatileEnv proc t =
        if debug then debug_globEnv globEnv ;
        let labels = Tmpl.get_labels t in
        let labels =
          List.map
            (fun (p,lbl) -> sprintf "ins_t *%s" (OutUtils.fmt_lbl_var p lbl))
            labels in
        let addrs_proc = Tmpl.get_addrs t in
        let addrs =
          List.map
            (fun x ->
              let ty =
                try List.assoc x globEnv
                with Not_found -> Compile.base in
              let ty = SkelUtil.dump_global_type x ty in
              match O.memory with
              | Memory.Direct ->
                  sprintf "%s *%s" ty x
              | Memory.Indirect ->
                  sprintf "%s **%s" ty x)
            addrs_proc in
        let cpys =
          if O.memory = Memory.Indirect && O.cautious then
            List.map
              (fun x ->
                let ty =
                  try List.assoc x globEnv
                  with Not_found -> assert false in
                sprintf "%s **%s"
                  (CType.dump ty)
                  (Tmpl.addr_cpy_name x proc))
              addrs_proc
          else [] in
        let outs =
          List.map
            (fun x ->
              let ty =
                try RegMap.find x env
                with Not_found -> assert false in
              let x = Tmpl.dump_out_reg proc x in
              sprintf "%s *%s" (CType.dump ty) x) t.Tmpl.final in
        let params =  String.concat "," (labels@addrs@cpys@outs) in
        LangUtils.dump_code_def chan true proc params ;
        do_dump
          compile_init_val_fun
          compile_addr_fun
          (compile_cpy_fun proc)
          (fun p r  -> sprintf "*%s" (Tmpl.dump_out_reg p r))
          chan "  " env proc t ;
        fprintf chan "}\n\n" ;
        ()

      let compile_label_call (p,lbl) =
        OutUtils.(
        sprintf "&_a->%s[_i*_a->%s+_a->%s+%s]"
          (fmt_code p) (fmt_code_size p)
          (fmt_prelude p) (fmt_lbl_offset p lbl))

      let compile_addr_call x = sprintf "&_a->%s[_i]" x
      let compile_cpy_addr_call proc x =
        sprintf "&_a->%s[_i]" (Tmpl.addr_cpy_name x proc)
      let compile_out_reg_call proc reg =
        sprintf "&%s" (Tmpl.compile_out_reg proc reg)

      let dump_call f_id _tr_idx chan indent _env _globEnv _volatileEnv proc t =
        let labels = List.map compile_label_call (Tmpl.get_labels t) in
        let addrs_proc = Tmpl.get_addrs t in
        let addrs = List.map compile_addr_call addrs_proc
        and addrs_cpy =
          if O.memory = Memory.Indirect && O.cautious then
            List.map (compile_cpy_addr_call proc) addrs_proc
          else []
        and outs = List.map (compile_out_reg_call proc) t.Tmpl.final in
        let args = String.concat "," (labels@addrs@addrs_cpy@outs) in
        LangUtils.dump_code_call chan indent f_id args

    end
