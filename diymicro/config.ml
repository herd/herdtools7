let verbose = ref (-1)

let prog_name =
  let prog = if Array.length Sys.argv > 0 then Sys.argv.(0) else "XXX" in
  Filename.basename prog
