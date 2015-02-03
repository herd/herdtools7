let () =
  if compare Sys.ocaml_version "4.02.0" >= 0  then
    Printf.printf "ok\n"
  else
    Printf.printf "no\n"

