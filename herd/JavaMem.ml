
(** Entry to models for Java  *)

module type Config = sig
  val model : Model.t
  val bell_model_info : (string * BellModel.info) option
  include Model.Config

  val statelessrc11 : bool
end

module Make
    (O:Config)
    (S:Sem.Semantics)
 :
    (XXXMem.S with module S = S)
    =
  struct

    open Model

    let model = O.model

    module S = S

    let check_event_structure test = match O.model with
    | Generic m ->
        let module X =
            MachModelChecker.Make
              (struct
                let m = m
                include O
              end)(S) in
        X.check_event_structure test
    | File _ -> assert false
    | m ->
        Warn.fatal "Model %s not implemented for Java" (Model.pp m)
end
