(* Logging function that takes a verbosity level and a formatted message *)
let log level channel fmt =
  if level <= !Config.verbose then
    Printf.kfprintf
      (fun oc -> flush oc)
      channel
      fmt
  (* ignores the output *)
  else Printf.ifprintf channel fmt

let info level fmt = log level stderr fmt
