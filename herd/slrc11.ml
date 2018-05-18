open Printf

module type Cfg = sig
  include Model.Config
end

module Make (O:Cfg)(S:Sem.Semantics)
  =
  struct
    module S = S
    module E = S.E
    module A = S.A



    let check_event_structure test conc kfail kont res =
      ()

end
