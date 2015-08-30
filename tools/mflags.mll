(* Performs flag comparison *)
{
open Printf

let verbose = ref false

let add_flag tst flag k =
  let old =
    try StringMap.find tst k
    with Not_found -> StringSet.empty in
  StringMap.add tst (StringSet.add flag old) k

}


let digit = [ '0'-'9' ]
let num = digit+
let float = num |  (num '.' num?)
let hexa = ['0'-'9' 'a'-'f' 'A'-'F' ]
let alpha = [ 'a'-'z' 'A'-'Z']
let name = alpha (alpha|digit)*
let blank = [' ' '\t']
let testname  = (alpha|digit|'_' | '/' | '.' | '-' | '+')+

rule main name_ok tst k = parse
| "Test" blank+ (testname as name) ('\n'|blank+ [^'\n']* '\n')
  { main name_ok (Some name)  k lexbuf }
| "Flag" blank+ (name as flag) [^'\n']* '\n'
  { 
    let tst = match tst with Some tst -> tst | None -> assert false in
    if name_ok tst then
      main name_ok None (add_flag tst flag k) lexbuf
    else
      main name_ok None k lexbuf
  }
| [^'\n']* '\n'
  { main name_ok tst k lexbuf }
| eof|"" { k }

{

(* Call lexer *)
let zyva name_ok chan =
  main name_ok None StringMap.empty (Lexing.from_channel chan)

let zyva name_ok fname = Misc.input_protect (zyva name_ok) fname



let args = ref []
let names = ref []

let () = 
  Arg.parse
    [
     "-v", Arg.Unit (fun () -> verbose := true), "be verbose" ;
     "-names", Arg.String (fun s -> names := !names @ [s]),
     "<name> read name file";
    ]
    (fun s -> args := !args @ [s])
    "Usage: mflags [opts] log1 log2"

(* Read names *)
let name_ok = match !names with
| [] -> fun _ -> true
| names -> 
    let set = ReadNames.from_files names StringSet.add StringSet.empty in
    fun n -> StringSet.mem n set

let flag name tst fs =
  printf "Only in %s, flags {%s}, test %s\n" name
    (StringSet.pp_str "," Misc.identity fs) tst


let check name k1 k2 =
  StringMap.iter
    (fun tst fs1 ->
        let fs2 =
          try StringMap.find tst k2
          with Not_found -> StringSet.empty in
        let d = StringSet.diff fs1 fs2 in
        if not (StringSet.is_empty d) then
          flag name tst d)
    k1

let () = match !args with
| [f1;f2;] ->
    let k1 = zyva name_ok f1
    and k2 = zyva name_ok f2 in
    check f1 k1 k2 ;
    check f2 k2 k1 ;
    exit 0
| _ ->
    eprintf "usage: mfalgs f1 f2\n" ;
    exit 2

}
