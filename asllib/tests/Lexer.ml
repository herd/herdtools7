open Helpers

let token_utils () =
  let cmly_file = Sys.getenv "ASL_PARSER_CMLY" in
  let module G = MenhirSdk.Cmly_read.Read (struct
    let filename = cmly_file
  end) in
  let module T = G.Terminal in
  let module A = G.Attribute in
  T.iter (fun t ->
      let is_regular = match T.kind t with `REGULAR -> true | _ -> false in
      let is_external =
        List.for_all
          (fun a -> not @@ String.equal (A.label a) "internal")
          (T.attributes t)
      in
      let name = T.name t in
      let is_not_complex_token =
        not
        @@ List.exists (String.equal name)
             [
               "EOF";
               "INT_LIT";
               "REAL_LIT";
               "STRING_LIT";
               "BOOL_LIT";
               "BITVECTOR_LIT";
               "MASK_LIT";
               "IDENTIFIER";
             ]
      in
      if is_regular && is_external && is_not_complex_token then
        let tok =
          match Asllib.Lexer.token_of_string name with
          | None ->
              raise
              @@ Failure
                   (Printf.sprintf "Token %s does not have a stinrg mapping."
                      name)
          | Some t -> t
        in
        Asllib.Lexer.token_to_symbol tok |> fun _ -> ())

let () = exec_tests [ ("Lexer/token_utils", token_utils) ]
