
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
    let module ModelConfig = struct
      let showsome =
	begin match C.outputdir with Some _ -> true | None -> false end
	|| C.PC.gv || C.PC.evince
      let through = C.through
      let debug = C.debug.Debug.barrier
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

    (* instantiate the interpreter module *)
    let module BellS = BellSem.Make(C)(SymbValue) in
    let module I = Interpreter.Make
	  (struct 
	    let m = generic_m
	    include ModelConfig
	   end)
	  (BellS) in

    (* construct an empty test *)
    let module Bell = BellArch.Make(C.PC)(SymbValue) in
    let module T = Test.Make(Bell) in
    let empty_test = T.empty_test in

    (* construct an empty ks *)
    let module E = BellS.E in
    let conc = BellS.conc_zero in
      let evts =
        E.EventSet.filter
          (fun e -> E.is_mem e || E.is_barrier e)
          conc.BellS.str.E.events in
      let id =
        lazy begin
          E.EventRel.of_list
            (List.rev_map
               (fun e -> e,e)
               (E.EventSet.elements evts))
        end in
    let unv = lazy begin E.EventRel.cartesian evts evts  end in
    let ks = {I.id; unv; evts; conc;} in

    (* create vb_pp (not entirely sure what this does, but maybe
       this will suffice) *)
    let vb_pp = lazy [] in

    (* Continuation: notice that it should be called once at most *)
    let function_arg =
      (fun st res -> match res with
      | None -> Some (I.extract_bell_dec st.I.bell_dec)
      | Some _ -> assert false) in

    (* call the interpreter  and collect bell info *)
    match I.interpret empty_test ks I.env_empty vb_pp function_arg None with
    | None -> assert false (* Continuation must be called at least once *)
    | Some (events,relations,orders) ->
        (* build the bell info *)
        Bell_info.build_bell_info events relations orders    
end
