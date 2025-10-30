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
  val hexa : bool
  val memory : Memory.t
  val cautious : bool
  val mode : Mode.t
  val asmcommentaslabel : bool
  val noinline : bool
  val variant : Variant_litmus.t -> bool
end

module type I = sig
  include Template.I
(* Forbidden registers *)
  val forbidden_regs :  arch_reg list
(* Initial value of internal registers *)
  val internal_init : arch_reg -> string option -> (string * string) option
(* gcc assembly template register class *)
  val reg_class : arch_reg -> string
  val reg_class_stable : bool -> arch_reg -> string
end

module Make
    (O:Config)
    (A:I)
    (Tmpl:Template.S with
     module V = A.V and
type arch_reg = A.arch_reg and
module RegSet = A.RegSet and
module RegMap = A.RegMap)
    (AL:Arch_litmus.S with type instruction = A.V.Instr.t)
    = struct

      let do_self = O.variant Variant_litmus.Self

      type arch_reg = Tmpl.arch_reg
      type t = Tmpl.t

      let has_explicit_handler = Tmpl.has_asmhandler

      open Printf

      let debug = false

      let compile_addr_inline = match O.mode with
      | Mode.Std -> sprintf "_a->%s[_i]"
      | Mode.PreSi|Mode.Kvm -> sprintf "*%s"

      and compile_addr_fun x = sprintf "*%s" x

      and compile_val_inline v = AL.GetInstr.dump_instr Tmpl.dump_v v

      module RegSet = Tmpl.RegSet
      module RegMap = Tmpl.RegMap

      let dump_clobbers chan clobs t =
        let clobs =
          clobs@List.map A.reg_to_string (t.Tmpl.all_clobbers@A.forbidden_regs)
          |> StringSet.of_list
        and stable =
          List.map  A.reg_to_string t.Tmpl.stable |> StringSet.of_list in
        let clobs = StringSet.diff clobs stable |> StringSet.elements in
        fprintf chan ":%s\n"
          (String.concat ","
             (List.map (fun s -> sprintf "\"%s\"" s)
                ("cc"::"memory"::clobs)))

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

      let has_init reg test = List.mem_assoc reg test.Tmpl.init

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

      let dump_inputs args0 compile_val chan t trashed =
        let stable = RegSet.of_list t.Tmpl.stable in
        let all = Tmpl.all_regs_in_tmpl t in
        let init_set =
            (List.fold_right
               (fun (reg,_) -> RegSet.add reg) t.Tmpl.init RegSet.empty) in
        let in_outputs =
          RegSet.unions [trashed;stable;RegSet.of_list t.Tmpl.final] in

        if debug then begin
          let pp_reg chan r = fprintf chan "%s" (A.reg_to_string r) in
          eprintf "Trashed in In: %a\n"
            (fun chan rs -> RegSet.pp chan "," pp_reg rs)
            trashed ;
          eprintf "Outputs in In: %a\n"
            (fun chan rs -> RegSet.pp chan "," pp_reg rs)
            in_outputs
        end ;


        let dump_pair reg v =
          let dump_v = compile_val in
          let dump_v = (* catch those addresses that are saved in a variable *)
            if O.cautious then
              (fun v -> match v with
              | Constant.Symbolic _ -> copy_name (Tmpl.tag_reg reg)
              | _ -> dump_v v)
            else dump_v  in

          let dump_v_typed v =
            match v with
            | Constant.Concrete _ ->
               begin try
                 let ty = RegMap.find reg t.Tmpl.code_ty_env in
                 sprintf "(%s)(%s)" (CType.dump ty) (dump_v v)
               with Not_found -> dump_v v
               end
            | _ -> dump_v v in

          if RegSet.mem reg in_outputs then begin
            match A.internal_init reg (Some(dump_v v)) with
            | None ->
               sprintf "\"%s\" (%s)" (tag_reg_def reg)
                 (dump_v_typed v)
            | Some (s,_) -> sprintf "\"%s\" (%s)" (tag_reg_def reg) s
          end else match A.internal_init reg (Some(dump_v v)) with
          | None ->
              sprintf "%s \"%s\" (%s)"
                (tag_reg_def reg)
                (strip_equal (A.reg_class reg))
                (dump_v_typed v)
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
          (* Beware: in X86_64 non-initialised registers have to be zero-ed,
             even when not in live-in, as they may be initialised only
             partially by code. *)
          if AL.arch = `X86_64 then
            RegSet.unions [rem;(RegSet.diff all init_set)]
          else rem in
        let rem =
          RegSet.fold
            (fun reg k ->
              let v = A.V.zero in
              dump_pair reg v::k)
            rem [] in
        let consts =
          List.map
            (fun (tag,v) ->  sprintf "[%s] \"i\" (%s)" tag v)
            args0.Template.constants in
        let args0 =
          List.map
            (fun (_,(tag,v)) -> sprintf "[%s] \"r\" (%s)" tag v)
            args0.Template.inputs in
        let out =  (String.concat "," (consts@args0@ins@rem)) in
(*        eprintf "IN: {%s}\n" out ; *)
        fprintf chan ":%s\n" out

      let (@@) f k  = f k

      let pp_regs rs = String.concat "," (List.map A.reg_to_string (RegSet.elements rs))

      let reg_class_stable reg t = A.reg_class_stable (has_init reg t) reg

      let dump_outputs args0 compile_addr compile_out_reg chan proc t trashed =
        let stable = RegSet.of_list t.Tmpl.stable in
        let final = RegSet.of_list t.Tmpl.final in
        if debug then
          eprintf "P%i: stable={%s}, final={%s}, all={%s}\n"
            proc (pp_regs stable) (pp_regs final)
            (pp_regs (Tmpl.all_regs_in_tmpl t));
        let outs =
          String.concat ","
            (List.fold_right
               (fun tr k -> sprintf "[%s] \"=&r\" (%s)" tr tr::k)
               args0.Template.trashed
             @@List.fold_right
               (match O.memory with
               | Memory.Direct ->
                   (fun a k -> sprintf "[%s] \"=m\" (%s)" a (compile_addr a)::k)
               | Memory.Indirect ->
                   (fun a k -> sprintf "[%s] \"=m\" (*%s)" a
                       (compile_addr a)::k))
               t.Tmpl.addrs
             @@List.fold_right
                  (fun a k ->
                    let a = Misc.add_pte a in
                    sprintf "[%s] \"=m\" (*(_vars->%s))" a a::k)
                  t.Tmpl.ptes
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
                        (reg_class_stable reg t)
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
                      (reg_class_stable reg t)
                      (dump_stable_reg reg)::k)
                  (RegSet.diff stable final) []) in
        fprintf chan ":%s\n" outs

      let dump_copies
        compile_out_reg compile_val compile_cpy chan indent proc t =
        List.iter
          (fun reg ->
            fprintf chan "%s%s %s = %s;\n" indent
              (let ty =
                RegMap.safe_find Compile.base reg t.Tmpl.ty_env in
              CType.dump ty)
              (copy_name (Tmpl.dump_out_reg proc reg))
              (compile_out_reg proc reg) ;
            fprintf chan "%smcautious();\n" indent)
          t.Tmpl.final ;
        begin match O.memory with
        | Memory.Indirect ->
            List.iter
              (fun (reg,v) -> match v with
              | Constant.Symbolic a ->
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
            let ty =
              RegMap.safe_find Compile.base reg t.Tmpl.ty_env in
            match ty with
            | CType.Array _ when A.arch = `AArch64 ->
              fprintf chan "%s%s out_%s = cast(%s);\n" indent
                (CType.dump CType.int32x4_t) (dump_stable_reg reg) (dump_stable_reg reg);
              fprintf chan "%smemcpy(%s, &out_%s, sizeof(%s));\n" indent
                (compile_out_reg proc reg) (dump_stable_reg reg) (CType.dump ty)
            | _ ->
              fprintf chan "%s%s = %s;\n" indent
                (compile_out_reg proc reg) (dump_stable_reg reg))
          (RegSet.inter stable finals)

      let before_dump args0 compile_out_reg compile_val compile_cpy
          chan indent proc t trashed =
        List.iter
          (fun (t,n) ->
             fprintf chan "  extern %s %s;\n" (CType.dump t) n)
          args0.Template.externs ;
        begin match args0.Template.trashed with
        | [] -> ()
        | trs ->
            fprintf chan "  uint64_t %s;\n" (String.concat "," trs)
        end ;
        RegSet.iter
          (fun reg ->
            let ty = match A.internal_init reg None with
            | Some (_,ty) -> ty
            | None ->
               CType.dump
                 (RegMap.safe_find CType.word reg t.Tmpl.ty_env) in
            fprintf chan "%s%s %s;\n"
              indent ty (dump_trashed_reg reg))
          trashed ;
        List.iter
          (fun reg ->
            let ty = match A.internal_init reg None with
            | Some (_,ty) -> ty
            | None ->
               CType.dump (RegMap.safe_find Compile.base reg t.Tmpl.ty_env) in
            let init = match init_val reg t with
            | None -> ""
            | Some v ->
                begin
                  match A.internal_init reg (Some(compile_val v)) with
                  | Some (init,_) -> sprintf " = %s" init
                  | None ->
                    begin
                      match v with
                      | Constant.Symbolic _ ->
                          sprintf " = (%s)%s" ty (compile_val v)
                      | _ ->
                          sprintf " = %s" (compile_val v)
                    end
                end in
            fprintf chan "%sregister %s %s asm(\"%s\")%s;\n"
              indent ty (dump_stable_reg reg)
                (A.reg_to_string reg) init)
          t.Tmpl.stable ;

        if O.cautious then begin
          dump_copies compile_out_reg compile_val compile_cpy chan
            indent proc t
        end

      let dump_code chan proc func code =
        let dump_ins k ins =
          begin match ins.Tmpl.label with
          | Some _ ->
             fprintf chan "\"%s_litmus_P%i%s_%i\\n\"\n" Tmpl.comment proc func k
          | None ->
             fprintf chan "\"%s_litmus_P%i%s_%i\\n%s\"\n" Tmpl.comment proc func k
               (if ins.Tmpl.comment then "" else "\\t")
          end;
          fprintf chan "\"%s\\n\"\n" (Tmpl.to_string ins) ;
          k + 1 in
        fprintf chan "\"%s%s\\n\"\n" (LangUtils.start_comment Tmpl.comment proc) func ;
        let _ = List.fold_left dump_ins 0 code in
        fprintf chan "\"%s%s\\n\"\n" (LangUtils.end_comment Tmpl.comment proc) func

      let dump_code_labels chan proc func code =
        let dump_ins k ins =
          fprintf chan "\"%s\\n\"\n" (Tmpl.to_string ins) ;
          k + 1 in
        fprintf chan "\"%s%s:\\n\"\n" (LangUtils.start_label proc) func ;
        let _ = List.fold_left dump_ins 0 code in
        fprintf chan "\"%s%s:\\n\"\n" (LangUtils.end_label proc) func

      let dump_main chan proc code =
        if O.asmcommentaslabel then
          dump_code_labels chan proc "" code
        else
          dump_code chan proc "" code

      let dump_fh chan proc code =
        if O.asmcommentaslabel then
          dump_code_labels chan proc ".F" code
        else
          dump_code chan proc ".F" code

      let do_dump args0 compile_val compile_addr compile_cpy compile_out_reg
          chan indent proc t =
        let trashed = Tmpl.trashed_regs t in
        before_dump args0
         compile_out_reg compile_val compile_cpy
         chan indent proc t trashed;
        fprintf chan "asm __volatile__ (\n" ;
        fprintf chan "\"\\n\"\n" ;
        dump_main chan proc t.Tmpl.code ;
        if t.Tmpl.fhandler <> [] then
          dump_fh chan proc t.Tmpl.fhandler ;
        dump_outputs args0 compile_addr compile_out_reg chan proc t trashed ;
        dump_inputs args0 compile_val chan t trashed ;
        dump_clobbers chan args0.Template.clobbers t  ;
        fprintf chan ");\n" ;
        after_dump compile_out_reg chan indent proc t ;
        ()

      let find_global_type x globEnv =
        try List.assoc x globEnv
        with Not_found -> Compile.base

      let debug_globEnv e =
        let pp =
          List.map
            (fun (a,ty) ->
              sprintf "%s -> %s" a (SkelUtil.dump_global_type a ty))
            e in
        eprintf "ENV: [%s]\n"
          (String.concat " " pp)


      let dump chan indent (globEnv,_) _volatileEnv proc t =

        if debug then debug_globEnv globEnv ;

        let compile_out_reg = match O.mode with
        | Mode.Std ->
           fun proc reg -> "_a->" ^ Tmpl.compile_out_reg proc reg
        | Mode.Kvm|Mode.PreSi ->
            fun proc reg ->
              let ty =
                try RegMap.find reg t.Tmpl.ty_env
                with Not_found -> assert false in
              if CType.is_ptr ty || CType.is_pte ty then
                Tmpl.compile_presi_out_ptr_reg proc reg
              else
                Tmpl.compile_presi_out_reg proc reg in

        do_dump
          Template.no_extra_args compile_val_inline compile_addr_inline
          (fun x -> sprintf "_a->%s[_i]" (Tmpl.addr_cpy_name (Constant.as_address x) proc))
          compile_out_reg
          chan indent proc t

      let add_pteval k = sprintf "_pteval%d" k

      let add_parel1val k = sprintf "_parel1%d" k

      let find_pteval_index p =
        let rec find_rec k = function
          | [] -> assert false
          | q::rem ->
              if A.V.PteVal.eq p q then k
              else find_rec (k+1) rem in
        find_rec 0

      let find_parel1_index p =
        let rec find_rec k = function
          | [] -> assert false
          | q::rem ->
              if A.V.AddrReg.eq p q then k
              else find_rec (k+1) rem in
        find_rec 0

      let check_memory =
        let open Memory in
        match O.memory with
        | Direct -> Misc.identity
        | Indirect -> sprintf "*%s"

      let compile_symbol_fun =
        let open Constant in
        let open Mode in
        fun env sym ->
          match sym with
          | Virtual { name; tag=None; cap=0L; offset=0; _ } ->
              check_memory name
          | Virtual { name; tag=None; cap=0L; offset; _ } ->
              let ty = find_global_type name env in
              if CType.is_array ty then
                let t = check_memory name in
                sprintf "&(*%s)[%d]" t offset
              else Constant.pp_symbol_old sym |> check_memory
          | _ -> Constant.pp_symbol_old sym |> check_memory

      let compile_val_fun =
        let open Constant in
        fun globEnv ptevalEnv parel1Env v -> match v with
        | Symbolic (Virtual a)
          when not (PAC.is_canonical a.pac) ->
            Warn.user_error "Litmus cannot initialize a virtual address with a non-canonical PAC field"
        | Symbolic sym ->
            compile_symbol_fun globEnv sym
        | Concrete _ | ConcreteVector _ | Instruction _
          -> AL.GetInstr.dump_instr Tmpl.dump_v v
        | Label (p,lbl) -> OutUtils.fmt_lbl_var p lbl
        | PteVal p ->
            let idx = find_pteval_index p ptevalEnv in
            add_pteval idx
        | AddrReg a ->
          let idx = find_parel1_index a parel1Env in
            add_parel1val idx
        | Tag _|Frozen _ | ConcreteRecord _-> assert false

      let compile_init_val_fun = compile_val_fun

      let compile_cpy_fun proc a = sprintf "*%s" (Tmpl.addr_cpy_name a proc)

      let extract_ptevals t =
        List.fold_left
          (fun k (_,v) -> match v with
          | Constant.PteVal p -> p::k
          | _ -> k)
          [] t.Tmpl.init

      let extract_parel1s t =
        List.fold_left
          (fun k (_,v) -> match v with
          | Constant.AddrReg p -> p::k
          | _ -> k)
          [] t.Tmpl.init

      let is_nop_v = function
        | Constant.Instruction i -> A.V.Instr.is_nop i
        | _ -> false

      let nop_init t = List.exists (fun (_,v) -> is_nop_v v) t.Tmpl.init

      let dump_fun  chan args0 globEnv _volatileEnv proc t =
        if debug then debug_globEnv globEnv ;
        let ptevalEnv = extract_ptevals t in
        let parel1Env = extract_parel1s t in
        let instrs = Tmpl.get_instructions t in
        let instrs =
          List.map
            (fun i ->  sprintf "ins_t %s" (AL.GetInstr.instr_name i))
            instrs in
        let labels = Tmpl.get_labels t in
        let labels =
          List.map
            (fun (p,lbl) -> sprintf "ins_t *%s" (OutUtils.fmt_lbl_var p lbl))
            labels in
        let addrs_proc,ptes_proc = Tmpl.get_addrs t
        and phys_proc = Tmpl.get_phys_only t in
        let addrs =
          List.map
            (fun x ->
              let ty = find_global_type x globEnv in
              let ty = SkelUtil.dump_global_type x ty in
              match O.memory with
              | Memory.Direct ->
                  sprintf "%s *%s" ty x
              | Memory.Indirect ->
                  sprintf "%s **%s" ty x)
            addrs_proc in
        let params0 =
          List.map
            (fun (tns,_) ->
               List.map
                 (fun (t,n)  ->
                    sprintf "%s %s" (CType.dump t) n)
                 tns)
          args0.Template.inputs |> List.flatten in
        let ptes =
          List.map
            (fun x -> sprintf "pteval_t *%s" (Misc.add_pte x))
            ptes_proc in
        let phys =
          List.map
            (fun x -> sprintf "pteval_t %s" (Misc.add_physical x))
            phys_proc in
        let ptevals =
          List.mapi
            (fun i _ -> sprintf "pteval_t %s" (add_pteval i))
            ptevalEnv in
        let parel1s =
          List.mapi
            (fun i _ -> sprintf "parel1_t %s" (add_parel1val i))
            parel1Env in
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
                try RegMap.find x t.Tmpl.ty_env
                with Not_found -> assert false in
              let x = Tmpl.dump_out_reg proc x in
              sprintf "%s *%s" (CType.dump ty) x) t.Tmpl.final in
        let params =
          String.concat ","
            (params0@labels@instrs@addrs@ptes@phys@ptevals@parel1s@cpys@outs) in
        LangUtils.dump_code_def chan O.noinline O.mode proc params ;
        do_dump
          args0
          (compile_init_val_fun globEnv ptevalEnv parel1Env)
          compile_addr_fun
          (fun sym -> compile_cpy_fun proc (Constant.as_address sym))
          (fun p r  -> sprintf "*%s" (Tmpl.dump_out_reg p r))
          chan "  " proc t ;
        fprintf chan "}\n\n" ;
        ()

      let compile_label_call (p,lbl) =
        let open OutUtils in
        match O.mode,do_self with
        | Mode.Std,false ->
           sprintf "_a->%s" (fmt_lbl_var p lbl)
        | Mode.Std,true ->
           sprintf "&_a->%s[_i*_a->%s+_a->%s+%s]"
             (fmt_code p) (fmt_code_size p)
             (fmt_prelude p) (fmt_lbl_offset p lbl)
        | (Mode.PreSi|Mode.Kvm),_ ->
           sprintf "_vars->labels.%s" (fmt_lbl_var p lbl)

      let compile_instr_call i = AL.GetInstr.instr_name i
      let indirect_star =
        let open Memory in
        match O.memory with
        | Direct -> ""
        | Indirect -> "*"

      let compile_addr_call_std env x =
        let pp = sprintf "&_a->%s[_i]" x in
        try
          let t = List.assoc x env in
          sprintf "(%s %s*)%s" (CType.dump t) indirect_star pp
        with Not_found  -> pp

      let compile_addr_call_kvm _ x = x

      let compile_addr_call =
        let open Mode in
        match O.mode with
        | Std -> compile_addr_call_std
        | Kvm|PreSi -> compile_addr_call_kvm

      let compile_cpy_addr_call proc x =
        sprintf "&_a->%s[_i]" (Tmpl.addr_cpy_name x proc)

      let compile_out_reg_call_std proc reg =
        sprintf "&_a->%s" (Tmpl.compile_out_reg proc reg)

      let compile_out_reg_call_kvm env proc reg =
        let ty =
          try A.RegMap.find reg env
          with Not_found -> assert false in
        sprintf "&%s"
          ((if CType.is_ptr ty ||  CType.is_pte ty then
            Tmpl.compile_presi_out_ptr_reg
          else Tmpl.compile_presi_out_reg) proc reg)

      let compile_out_reg_call env =
        let open Mode in
        match O.mode with
        | Std -> compile_out_reg_call_std
        | Kvm|PreSi -> compile_out_reg_call_kvm env

      module PU = SkelUtil.PteValUtil(A.V.PteVal)

      let dump_call f_id args0
            _tr_idx chan indent (_,alignedEnv) _volatileEnv proc t =
        let env = t.Tmpl.ty_env in
        let labels = List.map compile_label_call (Tmpl.get_labels t) in
        let instrs = List.map compile_instr_call (Tmpl.get_instructions t) in
        let addrs_proc,ptes = Tmpl.get_addrs t
        and phys = Tmpl.get_phys_only t in
        let addrs =
          List.map (compile_addr_call alignedEnv) addrs_proc @
          List.map OutUtils.fmt_pte_kvm ptes @
          List.map OutUtils.fmt_phy_kvm phys in
        let ptevals = extract_ptevals t in
        let ptevals =
          List.map
            (fun p ->
              match A.V.PteVal.as_physical p with
              | None|Some "" ->
                  Warn.user_error "litmus cannot handle pte initialisation with '%s'"
                    (A.V.PteVal.pp O.hexa p)
              | Some s ->
                  PU.dump_pteval_flags (OutUtils.fmt_phy_kvm s) p)
            ptevals in
        let parel1s = extract_parel1s t in
        let parel1s = List.map (fun a -> A.V.AddrReg.dump_pack SkelUtil.data_symb_id a) parel1s in
        let addrs_cpy =
          if O.memory = Memory.Indirect && O.cautious then
            List.map (compile_cpy_addr_call proc) addrs_proc
          else []
        and outs = List.map (compile_out_reg_call env proc) t.Tmpl.final in
        let args =
          String.concat ","
            (args0@labels@instrs@addrs@ptevals@parel1s@addrs_cpy@outs) in
        LangUtils.dump_code_call chan indent f_id args

    end
