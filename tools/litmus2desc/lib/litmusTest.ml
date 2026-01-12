type test = AArch64Base.pseudo MiscParser.t

let prog (t : test) : (MiscParser.proc * AArch64Base.instruction list) list =
  t.prog
  |> List.map (fun (proc, code) ->
      let code =
        code
        |> List.filter_map (fun (ins : AArch64Base.pseudo) ->
            match ins with Instruction i -> Some i | _ -> None)
      in
      (proc, code))

let with_cmd_stdout (cmd : string) (f : in_channel -> 'a) : 'a =
  let inp = Unix.open_process_in cmd in
  let r = f inp in
  match Unix.close_process_in inp with
  | Unix.WEXITED code when code = 0 -> r
  | Unix.WEXITED code -> failwith (Printf.sprintf "Exited with %d\n" code)
  | Unix.WSIGNALED s -> failwith (Format.sprintf "Killed by signal %d\n" s)
  | Unix.WSTOPPED s -> failwith (Format.sprintf "Stopped by signal %d\n" s)

let run_herd ~libdir ~herd_path file_path =
  let executions =
    let herd_path =
      match herd_path with Some path -> path | None -> "herd7"
    in
    let libdir_opt =
      match libdir with Some s -> [ "-set-libdir"; s ] | None -> []
    in
    let herd_cmd =
      String.concat " "
      @@ [
           herd_path;
           "-show";
           "prop";
           "-showevents";
           "all";
           "-through";
           "invalid";
           "-doshow";
           "ob";
           "-showraw";
           "ob";
           "-o -";
           "-output-format json";
         ]
      @ libdir_opt @ [ file_path ]
    in
    let json_lines =
      with_cmd_stdout herd_cmd (fun ch ->
          let in_json_section = ref false in
          let json_lines = ref [] in
          let () =
            Util.Iter.of_in_channel_lines ch (fun line ->
                if Misc.String.starts_with ~prefix:"JSONBEGIN" line then
                  in_json_section := true
                else if Misc.String.starts_with ~prefix:"JSONEND" line then
                  in_json_section := false
                else if !in_json_section then json_lines := line :: !json_lines
                else ())
          in
          List.rev !json_lines)
    in
    let json_contents = String.concat "\n" json_lines in
    match JsonGraph.decode_json_string json_contents with
    | Ok execs -> execs
    | Error err ->
        (* FIXME: do proper error reporting *)
        failwith err
  in
  executions

module MakeArch (C : sig
  val is_morello : bool
end) =
struct
  module SP = Splitter.Make (struct
    let debug = false
    let check_rename = fun s -> Some s
  end)

  module AArch64Value = CapabilityValue.Make (C)
  module AArch64 = MakeAArch64Base.Make (C)

  module LexConfig = struct
    let debug = false
  end

  module AArch64LexParse = struct
    type instruction = AArch64.parsedPseudo
    type token = AArch64Parser.token

    module Lexer = AArch64Lexer.Make (struct
      include LexConfig

      let is_morello = C.is_morello
    end)

    let lexer = Lexer.token
    let parser = AArch64Parser.main
  end

  module P0 =
    GenParser.Make (GenParser.DefaultConfig) (AArch64) (AArch64LexParse)

  type instruction = AArch64Base.instruction

  let parse chan =
    let (splitted : Splitter.result) = SP.split "T" chan in
    P0.parse chan splitted

  let parse_from_file = Misc.input_protect (fun ch -> parse ch)

  let show_instruction ~latex (ins : AArch64Base.instruction) =
    if latex then
      AArch64.do_pp_instruction
        { AArch64.m_int with pp_k = Format.sprintf "\\#%i" }
        ins
    else AArch64.dump_instruction ins
end
