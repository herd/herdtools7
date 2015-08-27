open Printf

module type Config = sig
  val debug : bool
  val verbose : int
  val libfind : string -> string
end

module Make (C: Config) = struct

  let interpret_bell model =
(* phew, a lot of work to set up the interpreter *)

    let module InterpreterConfig = struct
(* Model *)      
      let m = model
(* Bell stuff *)
      let bell = true
      let bell_fname = None
(* Model, restricted *)
      let showsome = false
      let debug = C.debug
      let verbose = C.verbose
      let skipchecks = StringSet.empty
      let strictskip = false
(* Show control, useless.. *)
      let doshow = StringSet.empty
      let showraw = StringSet.empty
      let symetric = StringSet.empty
      let libfind = C.libfind
    end in

 (* A dummy semantics! *)

    let module UnitS = struct
      module E = struct
        type event = unit
        let pp_eiid () = "a"

        module Ordered = struct
          type t = unit
          let compare () () = 0
        end

        module EventSet = MySet.Make(Ordered)
        module EventRel = InnerRel.Make(Ordered)
      end

      type test = unit
      type concrete = unit

      type event_set = E.EventSet.t
      type event_rel = E.EventRel.t
      type rel_pp = (string * event_rel) list

    end in

    let module I = Interpreter.Make
        (InterpreterConfig)
        (UnitS)
        (struct
          (* Should not be called *)
          let partition_events _ = assert false
          let loc2events _ _ = assert false
          let check_through _ = assert false
          let pp_failure _ _ msg _ =
            if C.debug then eprintf "%s\n" msg
        end) in

    let empty_test = () in

    (* construct an empty ks *)
    let conc = () in
    let evts = UnitS.E.EventSet.empty
    and id = lazy UnitS.E.EventRel.empty
    and unv = lazy UnitS.E.EventRel.empty in
    let ks = {I.id; unv; evts; conc;} in
    let vb_pp = lazy [] in

    (* Continuation: notice that it should be called once at most *)
    let function_arg st res = match res with
    | None -> Some st.I.out_bell_info
    | Some _ -> assert false in

    (* call the interpreter  and collect bell info *)
    match I.interpret
        empty_test Misc.identity ks I.init_env_empty vb_pp
        function_arg None with
    | None -> assert false (* Continuation must be called at least once *)
    | Some i ->
        if C.debug then begin
          eprintf "Bell file execute, result:\n" ;
          eprintf "%s" (BellModel.pp_info i)
        end ;
        i
end
