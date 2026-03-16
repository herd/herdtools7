open Uoaim

module NP = NameParser.Make (struct
  let includes = []
  let libdir = "./libdir"
  let cat = "aarch64.cat"
end)

let test_cases =
  [
    "ImpW";
    "M";
    "BCC";
    "Exp";
    "ExpM";
    "ImpR";
    "Wreg";
    "NoRet";
    "Rreg";
    "DSBFULL";
  ]

let () =
  try
    test_cases
    |> List.iter (fun tc ->
        let ss = NP.parse_names tc in
        Format.printf "%s -> %s@." tc (String.concat " " ss))
  with Misc.Fatal err -> Format.printf "%s@." err
