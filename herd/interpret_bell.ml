open Printf

module type Config = sig
  val model : Model.t option
  val through : Model.through
  val skipchecks : StringSet.t
  val strictskip : bool
  val check_name : string -> bool
  val check_rename : string -> string option
  include GenParser.Config
  include Top.Config
  include Sem.Config
end

module Make (C: Config) = struct

  open Model

  let interpret_bell model =
    (* phew, a lot of work to set up the interpreter *)

    (* first get the modelconfig *)
    let debug = C.debug.Debug.barrier in
    let module ModelConfig = struct
      let showsome =
	begin match C.outputdir with Some _ -> true | None -> false end
      || C.PC.gv || C.PC.evince
      let through = C.through
      let debug = debug
      let verbose = C.verbose
      let skipchecks = C.skipchecks
      let strictskip = C.strictskip
      let optace = C.optace
    end in

    (* get the model as the correct type *)
    let generic_m = match model with
    | Generic m -> m
    | _ -> Warn.fatal "Expected generic bell model"
    in

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
        (struct 
	  let m = generic_m
          let bell = true
          let bell_fname = None
	  include ModelConfig
          let doshow = StringSet.empty
          let showraw = StringSet.empty
          let symetric = StringSet.empty
        end)
        (UnitS)
        (struct
          let partition_events _ = []
          let pp_failure _ _ msg _ =
            if ModelConfig.debug then eprintf "%s\n" msg
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
    | None -> Some st.I.bell_info
    | Some _ -> assert false) in

    (* call the interpreter  and collect bell info *)
    match I.interpret empty_test ks I.env_empty vb_pp function_arg None with
    | None -> assert false (* Continuation must be called at least once *)
    | Some i ->
        if debug then begin
          eprintf "Bell file execute, result:\n" ;
          eprintf "%s" (BellCheck.pp_info i)
        end ;
        i
end
