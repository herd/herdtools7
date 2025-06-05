open Edge

module C = struct
  open Cycle
end

let init () =
  add_iico (* Example edge *)
    {
      repr = "pod:rr";
      compile_edge = (fun st _ -> [], [], DepNone, st);
      direction = Rm, Rm;
      ie = Internal;
      sd = Different;
      significant_source = false;
      significant_dest = false;
    }
