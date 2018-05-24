open Printf

module Make (O:Top_herd.Config)(S:Sem.Semantics)
  =
  struct
    module S = S
    module E = S.E
    module A = S.A
(*    module U = MemUtils.Make(S)*)
    module M = Mem.Make(O)(S)

    let debug_proc chan p = fprintf chan "%i" p

    let debug_event chan e = fprintf chan "%s:%s - " (E.pp_eiid e) (S.E.Act.pp_action e.action)

    let debug_cnstrnts chan e = fprintf chan "\n[ %s ]" (S.M.VC.pp_cnstrnts e)

    let debug_set chan r =
      E.EventRel.pp chan ","
        (fun chan (e1, e2) -> fprintf chan "%a -> %a"
                                debug_event e1 debug_event e2)
        r

(*    let preproc (rfms : (_ * S.M.VC.cnstrnts * S.E.event_structure) list) =
      let _ = List.iter
            (fun (_, (x : S.M.VC.cnstrnts), (y : E.event_structure)) ->
              let _ =  E.EventSet.iter
                         (fun z -> debug_event stdout z)
                         y.events in
              let _ = debug_cnstrnts stdout x in printf "\n") rfms in
      rfms *)





    let solve_step test es cs =
      match M.solve_regs test es cs with
      | None -> (es, cs)
      | Some (es, rfm, cs) -> (es, cs)

end
