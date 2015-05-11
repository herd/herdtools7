module type Config = sig
  val model : Model.t
  include Model.Config
end

module Make
    (O:Config)
    (S:Sem.Semantics)
    (B:AArch64Barrier.S with type a = S.barrier)
 :
    XXXMem.S with
module S = S
    =
  struct

    open Model


    module S = S

    module ModelConfig = (O : Model.Config)
    let model = O.model

    let check_event_structure test = match O.model with
    (*| Minimal uni ->
        let module X = 
          Minimal.Make
            (struct
              let uniproc = uni
              include ModelConfig
            end)
            (S) in
        X.check_event_structure test
    | CAV12 opt ->
        let module X = 
          CAV12.Make
            (struct
              let opt = opt
              include ModelConfig
            end)
            (S)
            (AllBarrier.FromAArch64(B)) in
        X.check_event_structure test
    | File _ -> assert false        
   *)    
      | Generic m ->
         let module X =
           MachModelChecker.Make
             (struct
               let m = m
               let bell_model_info = None
               include ModelConfig                  
             end)(S) in
         X.check_event_structure test
      | _ -> failwith "[AArch64Mem.ml] Unimplemented model for AArch64."
  end
