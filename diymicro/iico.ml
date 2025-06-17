open Edge

module C = struct
  open Cycle
end

module A = struct
  include AArch64_compile
end

module CasNoMem = struct
  let compileRnM ok =
   fun st dep _ _ ->
    let src_reg, v_opt =
      match dep with
      | DepReg (r, v) -> r, v
      | _ -> Warn.fatal "Event has not forwarded any register"
    in
    let rs, st = A.next_reg st in
    let rt, st = A.next_reg st in
    let loc, rn, st = A.assigned_next_loc st in

    let ins_zero, reg_zero, st = A.calc_value st 0 src_reg v_opt in
    let m_addr, st = A.next_reg st in
    let rcheck, st = A.next_reg st in
    let dst_reg, st = A.next_reg st in

    (* For srcM, that allows to check that STR is before CAS *)
    let m_initial = if ok then 0 else 1 in
    let final_expected_val = if ok then 2 else 1 in
    let st = A.set_initial st loc m_initial in
    let st = A.add_condition st rcheck final_expected_val in

    let ins =
      ins_zero
      @ A.pseudo [A.do_add64 A.vloc m_addr rn reg_zero]
      @ [A.mov rt 2]
      @ A.pseudo
          [
            A.cas A.RMW_P rs rt m_addr;
            A.do_ldr A.vloc rcheck rn;
            A.do_ldr A.vloc dst_reg rn;
          ]
    in
    ins, DepReg (dst_reg, Some final_expected_val), st

  let compile ok src dst =
    let cas_base st dep _ _ =
      let src_reg, _ =
        match dep with
        | DepReg (r, v) -> r, v
        | _ -> Warn.fatal "Event has not forwarded any register"
      in
      let rs, st = A.next_reg st in
      let rt, st = A.next_reg st in
      let loc, rn, st = A.assigned_next_loc st in
      let rcheck, st = A.next_reg st in

      (* For srcM, that allows to check that STR is before CAS *)
      let m_initial = if ok = (src <> "M") then 0 else 1 in
      let final_expected_val = if ok then 2 else 1 in
      let st = A.set_initial st loc m_initial in
      let st = A.add_condition st rcheck final_expected_val in

      let pre_ins, st =
        match src with
        | "Rs" -> A.pseudo [A.do_eor rs src_reg src_reg], st
        | "Rt" -> A.pseudo [A.do_eor rt src_reg src_reg], st
        | "Rn" ->
            let r_eor, st = A.next_reg st in
            let ins =
              A.pseudo
                [A.do_eor r_eor src_reg src_reg; A.do_add64 A.vloc rn rn r_eor]
            in
            ins, st
        | "M" ->
            let m_value = if ok then 0 else 1 in
            let reg_str, st = A.next_reg st in
            let ins =
              A.pseudo
                [
                  A.do_eor reg_str src_reg src_reg;
                  A.addi reg_str reg_str m_value;
                  A.str reg_str rn;
                ]
            in
            ins, st
        | _ -> assert false
      in
      let ins =
        A.pseudo
          [A.addi rt rt 2; A.cas A.RMW_P rs rt rn; A.do_ldr A.vloc rcheck rn]
      in

      let post_ins, st, dst_dep =
        match dst with
        | "Rs" -> [], st, DepReg (rs, None)
        | "M" ->
            let rn_reg, st = A.next_reg st in
            ( A.pseudo [A.do_ldr A.vloc rn_reg rn],
              st,
              DepReg (rn_reg, Some final_expected_val) )
        | _ -> assert false
      in
      pre_ins @ ins @ post_ins, dst_dep, st
    in
    match src, dst with "Rn", "M" -> compileRnM ok | _ -> cas_base

  let repr ok = "cas-no-mem:" ^ if ok then "ok" else "no"
end

module Cas = struct
  let compile ok src dst =
    let cas_base st dep src_evt dst_evt =
      let rcheck, st = A.next_reg st in

      let _, rn, read_value, write_value, st =
        match src_evt, dst_evt with
        | None, None ->
            let loc, rn, st = A.assigned_next_loc st in
            let read_v = if ok then 1 else 0 in
            let write_v = if ok then 2 else 0 in
            let st = A.set_initial st loc read_v in
            loc, rn, read_v, write_v, st
        | Some (loc, read_v, AnnotNone, _), None ->
            let write_v = if ok then read_v + 1 else read_v in
            let rn = A.get_register st loc in
            loc, rn, read_v, write_v, st
        | None, Some (loc, write_v, AnnotNone, _) ->
            let m_initial = write_v - 1 in
            if not ok then Warn.fatal "cas:no .->M not possible";
            let rn = A.get_register st loc in
            loc, rn, m_initial, write_v, st
        | Some (loc, read_v, AnnotNone, _), Some (loc2, write_v, AnnotNone, _)
          when loc = loc2 ->
            if not ok then Warn.fatal "cas:no M->M not possible";
            let rn = A.get_register st loc in
            loc, rn, read_v, write_v, st
        | _, _ -> Warn.fatal "cas received inconsistent event data"
      in
      (* We set up:
      - Rt=write_value
      - Rs=read_value if ok else read_value+1
      - and we already have [Rn]=read_value *)

      let st = A.add_condition st rcheck write_value in
      let rs_value = if ok then read_value else read_value + 1 in

      let pre_ins, rs, rt, rn, st =
        match src with
        | "M" ->
            let rs, st = A.next_reg st in
            let rt, st = A.next_reg st in
            let st = A.add_condition st rs read_value in
            (* if the src event is memory, the read is significant in all cases *)
            [A.mov rt write_value; A.mov rs rs_value], rs, rt, rn, st
        | _ -> (
            let src_reg, v_opt =
              match dep with
              | DepReg (r, v) -> r, v
              | _ -> Warn.fatal "Event has not forwarded any register"
            in
            match src with
            | "Rs" ->
                let ins_zero, r_eor, st = A.calc_value st 0 src_reg v_opt in
                let rs, st = A.next_reg st in
                let rt, st = A.next_reg st in
                let ins =
                  ins_zero
                  @ A.pseudo [A.addi rs r_eor rs_value]
                  @ [A.mov rt write_value]
                in
                ins, rs, rt, rn, st
            | "Rt" ->
                let rs, st = A.next_reg st in
                let ins_rt, rt, st =
                  A.calc_value st write_value src_reg v_opt
                in
                ins_rt @ [A.mov rs rs_value], rs, rt, rn, st
            | "Rn" ->
                let reg_zero, st = A.next_reg st in
                let rn2, st = A.next_reg st in
                let rs, st = A.next_reg st in
                let rt, st = A.next_reg st in
                let ins =
                  A.pseudo
                    [
                      A.do_eor reg_zero src_reg src_reg;
                      A.do_add64 A.vloc rn2 rn reg_zero;
                    ]
                  @ [A.mov rs rs_value; A.mov rt write_value]
                in
                ins, rs, rt, rn2, st
            | _ -> assert false)
      in
      let ins = A.pseudo [A.cas A.RMW_P rs rt rn; A.do_ldr A.vloc rcheck rn] in

      let dst_dep =
        match dst with
        | "Rs" -> DepReg (rs, Some read_value)
        | "M" -> DepNone
        | _ -> assert false
      in
      pre_ins @ ins, dst_dep, st
    in
    cas_base

  let repr ok = "cas:" ^ if ok then "ok" else "no"
end

module Csel = struct
  let compile ok src =
   fun st dep _ _ ->
    let src_reg, v_opt =
      match dep with
      | DepReg (r, v) -> r, v
      | _ -> Warn.fatal "Event has not forwarded any register"
    in

    let cmp_ins, cmp, st =
      if src = "cmp" then A.calc_value st 1 src_reg v_opt
      else
        let cmp, st = A.next_reg st in
        [A.mov cmp 1], cmp, st
    in
    let rn_ins, rn, st =
      if src = "Rn" then A.calc_value st 1 src_reg v_opt
      else
        let rn, st = A.next_reg st in
        [A.mov rn 1], rn, st
    in
    let rm_ins, rm, st =
      if src = "Rm" then A.calc_value st 2 src_reg v_opt
      else
        let rm, st = A.next_reg st in
        [A.mov rm 2], rm, st
    in
    let rd, st = A.next_reg st in
    let rcheck, st = A.next_reg st in

    let compared_value = if ok then 1 else 0 in
    let final_expected_val = if ok then 1 else 2 in
    let st = A.add_condition st rcheck final_expected_val in

    let ins =
      cmp_ins @ rn_ins @ rm_ins
      @ A.pseudo [A.cmpi cmp compared_value; A.do_csel A.vloc rd rn rm]
      @ [A.mov_reg rcheck rd]
    in

    ins, DepReg (rd, Some final_expected_val), st

  let repr ok = "csel:" ^ if ok then "ok" else "no"
end

module Swp = struct
  let compile src dst =
   fun st dep _ _ ->
    let src_reg, v_opt =
      match dep with
      | DepReg (r, v) -> r, v
      | _ -> Warn.fatal "Event has not forwarded any register"
    in

    let ins_zero, reg_zero, st = A.calc_value st 0 src_reg v_opt in

    let rd, st = A.next_reg st in
    let rm, st = A.next_reg st in
    let _, rn, st = A.assigned_next_loc st in

    let dep_ins, st =
      match src with
      | "Rd" -> A.pseudo [A.add A.vloc rd rd reg_zero], st
      | "Rm" -> A.pseudo [A.add A.vloc rm rm reg_zero], st
      | "Rn" -> A.pseudo [A.do_add64 A.vloc rn rn reg_zero], st
      | _ -> assert false
    in

    let post_ins, dep, st =
      match dst with
      | "Rd" -> [], DepReg (rd, None), st
      | "Rm" -> [], DepReg (rm, None), st
      | "Rn" ->
          let rn_reg, st = A.next_reg st in
          A.pseudo [A.do_ldr A.vloc rn_reg rn], DepReg (rn_reg, None), st
      | _ -> assert false
    in

    let ins =
      ins_zero @ dep_ins @ A.pseudo [A.swp A.RMW_P rd rm rn] @ post_ins
    in
    ins, dep, st

  let mem st dep src_evt dst_evt =
    (match dep with
    | DepNone -> ()
    | _ -> Warn.fatal "Dependency provided to Swp.mem");

    let rd, st = A.next_reg st in
    let rm, st = A.next_reg st in

    let rn_loc, read_value, is_read_significant =
      match src_evt with
      | Some (loc, v, AnnotNone, is_significant) -> loc, v, is_significant
      | _ -> Warn.fatal "Source event not provided or invalid"
    in
    let st =
      if is_read_significant then A.add_condition st rd read_value else st
    in
    let write_value =
      match dst_evt with
      | Some (loc, v, AnnotNone, _) when loc = rn_loc -> v
      | _ ->
          Warn.fatal
            "Destination event not provided, invalid or incompatible with \
             source event"
    in
    let rn = A.get_register st rn_loc in
    let ins = A.mov rm write_value :: A.pseudo [A.swp A.RMW_P rd rm rn] in

    ins, DepReg (rd, Some read_value), st
end

let init () =
  List.iter
    (fun ok ->
      add_iico
        {
          instruction_name = CasNoMem.repr ok;
          to_edge =
            (fun src dst ->
              {
                repr = "";
                compile_edge = CasNoMem.compile ok src dst;
                direction = RegEvent, RegEvent;
                ie = Internal;
                sd = Same;
                significant_source = false;
                significant_dest = false;
              });
          inputs = ["Rn"; "Rs"; "Rt"; "M"];
          outputs = ["Rs"; "M"];
        })
    [true; false];

  List.iter
    (fun ok ->
      add_iico
        {
          instruction_name = Cas.repr ok;
          to_edge =
            (fun src dst ->
              {
                repr = "";
                compile_edge = Cas.compile ok src dst;
                direction =
                  ( (if src = "M" then Rm true else RegEvent),
                    if dst = "M" then Wm true else RegEvent );
                ie = Internal;
                sd = Same;
                significant_source = src = "M";
                significant_dest = false;
              });
          inputs = ["Rn"; "Rs"; "Rt"; "M"];
          outputs = ["Rs"; "M"];
        })
    [true; false];

  List.iter
    (fun ok ->
      add_iico
        {
          instruction_name = Csel.repr ok;
          to_edge =
            (fun src _ ->
              {
                repr = "";
                compile_edge = Csel.compile ok src;
                direction = RegEvent, RegEvent;
                ie = Internal;
                sd = Same;
                significant_source = false;
                significant_dest = false;
              });
          inputs = ["cmp"; "Rn"; "Rm"];
          outputs = ["Rd"];
        })
    [true; false];

  add_iico
    {
      instruction_name = "swp";
      to_edge =
        (fun src dst ->
          {
            repr = "";
            compile_edge = Swp.compile src dst;
            direction = RegEvent, RegEvent;
            ie = Internal;
            sd = Same;
            significant_source = false;
            significant_dest = false;
          });
      inputs = ["Rd"; "Rm"; "Rn"];
      outputs = ["Rd"; "Rm"; "Rn"];
    };

  add_iico
    {
      instruction_name = "swp:mem";
      to_edge =
        (fun _ _ ->
          {
            repr = "";
            compile_edge = Swp.mem;
            direction = Rm true, Wm true;
            ie = Internal;
            sd = Same;
            significant_source = false;
            significant_dest = true;
          });
      (* TODO : we could prefer to be able to specify nothing, here and in the cli *)
      inputs = ["M"];
      outputs = ["M"];
    }
