module A = AArch64_compile

module Cas = struct
  let compile ok src dst =
    let cas_base st dep src_evt dst_evt =
      let rcheck, st = State.next_reg st in

      let _, rn, read_value, write_value, st =
        match src_evt, dst_evt with
        | None, None ->
            let loc, rn, st = State.assigned_next_loc st in
            let read_v = if ok then 1 else 0 in
            let write_v = if ok then 2 else 0 in
            let st = State.set_initial st loc read_v in
            loc, rn, read_v, write_v, st
        | Some (loc, read_v, Edge.AnnotNone, _), None ->
            let write_v = if ok then read_v + 1 else read_v in
            let rn = State.get_register st loc in
            loc, rn, read_v, write_v, st
        | None, Some (loc, write_v, Edge.AnnotNone, _) ->
            let m_initial = write_v - 1 in
            if not ok then Warn.fatal "cas:no .->M not possible";
            let rn = State.get_register st loc in
            loc, rn, m_initial, write_v, st
        | ( Some (loc, read_v, Edge.AnnotNone, _),
            Some (loc2, write_v, Edge.AnnotNone, _) )
          when loc = loc2 ->
            if not ok then Warn.fatal "cas:no M->M not possible";
            let rn = State.get_register st loc in
            loc, rn, read_v, write_v, st
        | _, _ -> Warn.fatal "cas received inconsistent event data"
      in

      (* We set up:
         - Rt=write_value
         - Rs=read_value if ok else read_value+1
         - and we already have [Rn]=read_value *)
      let st = State.add_condition st rcheck write_value in
      let rs_value = if ok then read_value else read_value + 1 in
      let rt_value = if ok then write_value else read_value + 1 in

      let pre_ins, rs, rt, rn, st =
        match src, dep with
        | "M", Edge.DepNone ->
            let rs, st = State.next_reg st in
            let rt, st = State.next_reg st in
            let st = State.add_condition st rs read_value in
            (* if the src event is memory, the read is significant in all cases *)
            [A.mov rt rt_value; A.mov rs rs_value], rs, rt, rn, st
        | "Rs", Edge.DepReg (src_reg, v_opt) ->
            let ins_zero, r_eor, st = State.calc_value st 0 src_reg v_opt in
            let rs, st = State.next_reg st in
            let rt, st = State.next_reg st in
            let ins =
              ins_zero
              @ A.pseudo [A.addi rs r_eor rs_value]
              @ [A.mov rt rt_value]
            in
            ins, rs, rt, rn, st
        | "Rt", Edge.DepReg (src_reg, v_opt) ->
            let rs, st = State.next_reg st in
            let ins_rt, rt, st = State.calc_value st rt_value src_reg v_opt in
            ins_rt @ [A.mov rs rs_value], rs, rt, rn, st
        | "Rn", Edge.DepReg (src_reg, _) ->
            let reg_zero, st = State.next_reg st in
            let rn2, st = State.next_reg st in
            let rs, st = State.next_reg st in
            let rt, st = State.next_reg st in
            let ins =
              A.pseudo
                [
                  A.do_eor reg_zero src_reg src_reg;
                  A.do_add64 A.vloc rn2 rn reg_zero;
                ]
              @ [A.mov rs rs_value; A.mov rt rt_value]
            in
            ins, rs, rt, rn2, st
        | _ ->
            Warn.user_error
              "Dependency missing or invalid, or src invalid for cas"
      in
      let ins = A.pseudo [A.cas A.RMW_P rs rt rn; A.do_ldr A.vloc rcheck rn] in

      let dst_dep =
        match dst with
        | "Rs" -> Edge.DepReg (rs, Some read_value)
        | "M" -> Edge.DepNone
        | _ -> Warn.fatal "Unknown destination %s" dst
      in
      pre_ins @ ins, dst_dep, st
    in
    cas_base

  let repr ok = "cas:" ^ if ok then "ok" else "no"
end

module Csel = struct
  let compile ok src st dep _ _ =
    let src_reg, v_opt =
      match dep with
      | Edge.DepReg (r, v) -> r, v
      | _ -> Warn.fatal "Event has not forwarded any register"
    in

    let cmp_ins, cmp, st =
      if src = "cmp" then State.calc_value st 1 src_reg v_opt
      else
        let cmp, st = State.next_reg st in
        [A.mov cmp 1], cmp, st
    in
    let rn_ins, rn, st =
      if src = "Rn" then State.calc_value st 1 src_reg v_opt
      else
        let rn, st = State.next_reg st in
        [A.mov rn 1], rn, st
    in
    let rm_ins, rm, st =
      if src = "Rm" then State.calc_value st 2 src_reg v_opt
      else
        let rm, st = State.next_reg st in
        [A.mov rm 2], rm, st
    in
    let rd, st = State.next_reg st in
    let rcheck, st = State.next_reg st in

    let compared_value = if ok then 1 else 0 in
    let final_expected_val = if ok then 1 else 2 in
    let st = State.add_condition st rcheck final_expected_val in

    let ins =
      cmp_ins @ rn_ins @ rm_ins
      @ A.pseudo [A.cmpi cmp compared_value; A.do_csel A.vloc rd rn rm]
      @ [A.mov_reg rcheck rd]
    in

    ins, Edge.DepReg (rd, Some final_expected_val), st

  let repr ok = "csel:" ^ if ok then "ok" else "no"
end

module Swp = struct
  let repr = "swp"

  let compile src dst st dep src_evt dst_evt =
    let rn, read_value, write_value, st =
      match src_evt, dst_evt with
      | None, None ->
          let _, rn, st = State.assigned_next_loc st in
          rn, Some 0, 1, st
      | Some (loc, read_v, Edge.AnnotNone, _), None ->
          let rn = State.get_register st loc in
          rn, Some read_v, read_v, st
      | None, Some (loc, write_v, Edge.AnnotNone, _) ->
          let rn = State.get_register st loc in
          rn, None, write_v, st
      | ( Some (loc, read_v, Edge.AnnotNone, _),
          Some (loc2, write_v, Edge.AnnotNone, _) )
        when loc = loc2 ->
          let rn = State.get_register st loc in
          rn, Some read_v, write_v, st
      | _, _ -> Warn.fatal "swp received inconsistent event data"
    in

    (* We set up:
       SWP(Rs, Rt, Rn)
         Rs=write_value (to be stored in [Rn])
         Rt=no specific value (0) (will get the value of [Rn])
        [Rn]=read_value needs no action
    *)
    let pre_ins, rs, rt, st =
      match src, dep with
      | "M", Edge.DepNone ->
          let rs, st = State.next_reg st in
          let rt, st = State.next_reg st in
          [A.mov rs write_value], rs, rt, st
      | "Rt", Edge.DepReg (src_reg, v_opt) ->
          let rs, st = State.next_reg st in
          let rt, st = State.next_reg st in
          let ins_zero, reg_zero, st = State.calc_value st 0 src_reg v_opt in
          let ins =
            ins_zero
            @ A.pseudo [A.add A.vloc rt rt reg_zero]
            @ [A.mov rs write_value]
          in
          ins, rs, rt, st
      | "Rs", Edge.DepReg (src_reg, v_opt) ->
          let rt, st = State.next_reg st in
          let ins_rs, rs, st = State.calc_value st write_value src_reg v_opt in
          ins_rs, rs, rt, st
      | "Rn", Edge.DepReg (src_reg, _) ->
          let rs, st = State.next_reg st in
          let rt, st = State.next_reg st in
          let ins_zero, reg_zero, st = State.calc_value st 0 src_reg None in
          let ins =
            ins_zero
            @ A.pseudo [A.do_add64 A.vloc rn rn reg_zero]
            @ [A.mov rs write_value]
          in
          ins, rs, rt, st
      | _ ->
          Warn.user_error
            "Dependency missing or invalid, or src invalid for swp"
    in

    let st =
      match read_value with None -> st | Some v -> State.add_condition st rt v
    in

    let post_ins, dep, st =
      match dst with
      | "Rt" -> [], Edge.DepReg (rt, read_value), st
      | "Rs" -> [], Edge.DepReg (rs, None), st
      | "Rn" ->
          let rn_reg, st = State.next_reg st in
          A.pseudo [A.do_ldr A.vloc rn_reg rn], Edge.DepReg (rn_reg, None), st
      | "M" ->
          ( [],
            Edge.DepReg (rt, read_value),
            st (* is used afterwards if event is significant *) )
      | _ -> Warn.fatal "Unknown destination %s" dst
    in

    let ins = pre_ins @ A.pseudo [A.swp A.RMW_P rs rt rn] @ post_ins in
    ins, dep, st
end

module LdAdd = struct
  let do_ldadd r1 r2 r3 = A.Instruction (A.ldop A.A_ADD A.RMW_P r1 r2 r3)
  let repr use_zr = "ldadd" ^ if use_zr then ":zr" else ""

  let compile use_zr src dst st dep src_evt dst_evt =
    let rn, is_read_significant, read_value, write_value, st =
      match src_evt, dst_evt with
      | None, None ->
          let _, rn, st = State.assigned_next_loc st in
          rn, false, 0, 1, st
      | Some (loc, read_v, Edge.AnnotNone, is_significant), None ->
          let rn = State.get_register st loc in
          rn, is_significant, read_v, read_v, st
      | None, Some (loc, write_v, Edge.AnnotNone, _) ->
          let rn = State.get_register st loc in
          ( rn,
            false,
            write_v - 1,
            write_v,
            st
            (* we assume the previous value is always 1, which is the case in diymicro *)
          )
      | ( Some (loc, read_v, Edge.AnnotNone, _),
          Some (loc2, write_v, Edge.AnnotNone, _) )
        when loc = loc2 ->
          let rn = State.get_register st loc in
          rn, false, read_v, write_v, st
      | _, _ -> Warn.fatal "swp received inconsistent event data"
    in

    (* We set up:
       Rs=(write_value - read_value)
       Rt=no specific value
       [Rn]=read_value needs no action
    *)
    let rs_value = write_value - read_value in
    let rt, st = if use_zr then A.ZR, st else State.next_reg st in

    let pre_ins, rs, st =
      match src with
      | "M" ->
          (match dep with
          | Edge.DepNone -> ()
          | _ -> Warn.user_error "Dependency provided to ldadd M->.");
          let rs, st = State.next_reg st in
          [A.mov rs rs_value], rs, st
      | _ -> (
          let src_reg, v_opt =
            match dep with
            | Edge.DepReg (r, v) -> r, v
            | _ -> Warn.fatal "Event has not forwarded any register"
          in
          match src with
          | "Rs" -> State.calc_value st rs_value src_reg v_opt
          | "Rt" ->
              let rs, st = State.next_reg st in
              [A.mov rs rs_value; A.mov_reg rt src_reg], rs, st
          | "Rn" ->
              let rs, st = State.next_reg st in
              let ins_zero, reg_zero, st = State.calc_value st 0 src_reg None in
              let ins =
                ins_zero
                @ A.pseudo [A.do_add64 A.vloc rn rn reg_zero]
                @ [A.mov rs write_value]
              in
              ins, rs, st
          | _ -> Warn.fatal "Unknown source %s" src)
    in

    let st =
      if is_read_significant && not use_zr then
        State.add_condition st rt read_value
      else st
    in

    let post_ins, dep, st =
      match dst with
      | "Rs" -> [], Edge.DepReg (rs, Some rs_value), st
      | "Rt" ->
          if use_zr then
            let zero_reg, st = State.next_reg st in
            let fence_ins = A.Instruction (A.I_FENCE (A.DMB (A.SY, A.LD))) in
            [fence_ins], Edge.DepReg (zero_reg, Some 0), st
          else [], Edge.DepReg (rt, Some read_value), st
      | "Rn" ->
          let rn_reg, st = State.next_reg st in
          A.pseudo [A.do_ldr A.vloc rn_reg rn], Edge.DepReg (rn_reg, None), st
      | "M" ->
          ( [],
            Edge.DepReg (rt, Some (if use_zr then 0 else read_value)),
            st (* is used afterwards if event is significant *) )
      | _ -> Warn.fatal "Unknown destination %s" dst
    in

    let ins = pre_ins @ [do_ldadd rs rt rn] @ post_ins in
    ins, dep, st
end

let init () =
  List.iter
    (fun ok ->
      Edge.add_iico
        Edge.
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
            outputs = (["Rs"] @ if ok then ["M"] else []);
          })
    [true; false];

  List.iter
    (fun ok ->
      Edge.add_iico
        Edge.
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

  Edge.add_iico
    Edge.
      {
        instruction_name = Swp.repr;
        to_edge =
          (fun src dst ->
            {
              repr = "";
              compile_edge = Swp.compile src dst;
              direction =
                ( (if src = "M" then Rm true else RegEvent),
                  if dst = "M" then Wm true else RegEvent );
              ie = Internal;
              sd = Same;
              significant_source = false;
              significant_dest = false;
            });
        inputs = ["Rs"; "Rt"; "Rn"; "M"];
        outputs = ["Rs"; "Rt"; "Rn"; "M"];
      };

  List.iter
    (fun use_zr ->
      Edge.add_iico
        Edge.
          {
            instruction_name = LdAdd.repr use_zr;
            to_edge =
              (fun src dst ->
                {
                  repr = "";
                  compile_edge = LdAdd.compile use_zr src dst;
                  direction =
                    ( (if src = "M" then Rm true else RegEvent),
                      if dst = "M" then Wm true else RegEvent );
                  ie = Internal;
                  sd = Same;
                  significant_source = false;
                  significant_dest = dst = "M";
                });
            inputs = ["Rs"; "Rt"; "Rn"; "M"];
            outputs = ["Rs"; "Rt"; "Rn"; "M"];
          })
    [true; false]
