open Edge

module C = struct
  open Cycle
end

module A = struct
  include AArch64_compile
end

module Cas = struct
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

  let compile src dst ok =
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
      let m_initial = if ok = (src <> `M) then 0 else 1 in
      let final_expected_val = if ok then 2 else 1 in
      let st = A.set_initial st loc m_initial in
      let st = A.add_condition st rcheck final_expected_val in

      let pre_ins, st =
        match src with
        | `Rs -> A.pseudo [A.do_eor rs src_reg src_reg], st
        | `Rt -> A.pseudo [A.do_eor rt src_reg src_reg], st
        | `Rn ->
            let r_eor, st = A.next_reg st in
            let ins =
              A.pseudo
                [A.do_eor r_eor src_reg src_reg; A.do_add64 A.vloc rn rn r_eor]
            in
            ins, st
        | `M ->
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
      in
      let ins =
        A.pseudo
          [A.addi rt rt 2; A.cas A.RMW_P rs rt rn; A.do_ldr A.vloc rcheck rn]
      in

      let post_ins, st, dst_dep =
        match dst with
        | `Rs -> [], st, DepReg (rs, None)
        | `M ->
            let rn_reg, st = A.next_reg st in
            ( A.pseudo [A.do_ldr A.vloc rn_reg rn],
              st,
              DepReg (rn_reg, Some final_expected_val) )
      in
      pre_ins @ ins @ post_ins, dst_dep, st
    in
    match src, dst with `Rn, `M -> compileRnM ok | _ -> cas_base

  let repr src dst ok =
    "cas:"
    ^ (if ok then "ok" else "no")
    ^ " "
    ^ (match src with `Rs -> "Rs" | `Rn -> "Rn" | `Rt -> "Rt" | `M -> "M")
    ^ "->"
    ^ match dst with `Rs -> "Rs" | `M -> "M"
end

module Csel = struct
  let compile src ok =
   fun st dep _ _ ->
    let src_reg, v_opt =
      match dep with
      | DepReg (r, v) -> r, v
      | _ -> Warn.fatal "Event has not forwarded any register"
    in

    let cmp_ins, cmp, st =
      if src = `cmp then A.calc_value st 1 src_reg v_opt
      else
        let cmp, st = A.next_reg st in
        [A.mov cmp 1], cmp, st
    in
    let rn_ins, rn, st =
      if src = `Rn then A.calc_value st 1 src_reg v_opt
      else
        let rn, st = A.next_reg st in
        [A.mov rn 1], rn, st
    in
    let rm_ins, rm, st =
      if src = `Rm then A.calc_value st 2 src_reg v_opt
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

  let repr src ok =
    "csel:"
    ^ (if ok then "ok" else "no")
    ^ " "
    ^ match src with `cmp -> "cmp" | `Rn -> "Rn" | `Rm -> "Rm"
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
      | `Rd -> A.pseudo [A.add A.vloc rd rd reg_zero], st
      | `Rm -> A.pseudo [A.add A.vloc rm rm reg_zero], st
      | `Rn -> A.pseudo [A.add A.vloc rn rn reg_zero], st
    in

    let post_ins, dep, st =
      match dst with
      | `Rd -> [], DepReg (rd, None), st
      | `Rm -> [], DepReg (rm, None), st
      | `Rn ->
          let rn_reg, st = A.next_reg st in
          A.pseudo [A.do_ldr A.vloc rn_reg rn], DepReg (rn_reg, None), st
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

    let rn_loc, read_value =
      match src_evt with
      | Some (loc, v, AnnotNone) -> loc, v
      | _ -> Warn.fatal "Source event not provided or invalid"
    in
    let write_value =
      match dst_evt with
      | Some (loc, v, AnnotNone) when loc = rn_loc -> v
      | _ ->
          Warn.fatal
            "Destination event not provided, invalid or incompatible with \
             source event"
    in
    let rn = A.get_register st rn_loc in
    let ins = A.mov rm write_value :: A.pseudo [A.swp A.RMW_P rd rm rn] in
    ins, DepReg (rd, Some read_value), st

  let pp_swp_reg = function `Rd -> "Rd" | `Rm -> "Rm" | `Rn -> "Rn"
  let repr src dst = "swp " ^ pp_swp_reg src ^ "->" ^ pp_swp_reg dst
end

let cartesian2 l1 l2 = List.concat_map (fun x -> List.map (fun y -> x, y) l2) l1

let cartesian3 l1 l2 l3 =
  List.concat_map
    (fun x -> List.concat_map (fun y -> List.map (fun z -> x, y, z) l3) l2)
    l1

let init () =
  List.iter
    (fun (ok, src, dst) ->
      add_iico
        {
          repr = Cas.repr src dst ok;
          compile_edge = Cas.compile src dst ok;
          direction = RegEvent, RegEvent;
          ie = Internal;
          sd = Same;
          significant_source = false;
          significant_dest = false;
        })
    (cartesian3 [true; false] [`Rn; `Rs; `Rt; `M] [`Rs; `M]);

  List.iter
    (fun (ok, src) ->
      add_iico
        {
          repr = Csel.repr src ok;
          compile_edge = Csel.compile src ok;
          direction = RegEvent, RegEvent;
          ie = Internal;
          sd = Same;
          significant_source = false;
          significant_dest = false;
        })
    (cartesian2 [true; false] [`cmp; `Rn; `Rm]);

  List.iter
    (fun (src, dst) ->
      add_iico
        {
          repr = Swp.repr src dst;
          compile_edge = Swp.compile src dst;
          direction = RegEvent, RegEvent;
          ie = Internal;
          sd = Same;
          significant_source = false;
          significant_dest = false;
        })
    (cartesian2 [`Rd; `Rm; `Rn] [`Rd; `Rm; `Rn]);

  add_iico
    {
      repr = "swp:mem";
      compile_edge = Swp.mem;
      direction = Rm true, Wm true;
      ie = Internal;
      sd = Same;
      significant_source = false;
      significant_dest = true;
    }
