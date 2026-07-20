let temp_name root =
  let name = Random.bits64 () in
  Printf.sprintf "%s/%Ld" root name

let temp_dir () =
 let rec try_name counter =
   let name = temp_name (Filename.get_temp_dir_name ()) in
   try Sys.mkdir name 0o700 ; name
   with Sys_error _ as e ->
  if counter >= 10 then raise e else try_name (counter + 1)
 in try_name 0

let with_dir f =
  let root = temp_dir () in
  let destroy () = Filesystem.remove_recursive root in
  Fun.protect ~finally:destroy (fun () -> f root)

let ( let@ ) f x = f x

let list_dir_test =
  let test () : unit =
    let@ root = with_dir in
    let listed =
       Filesystem.list_dir root
       |> Seq.filter_map (function "." | ".." -> None | e -> Some e)
    in
    Alcotest.(check @@ seq string) "Directory is empty" Seq.empty listed ;

    ()
  in
  ("list_dir lists all files in directory", `Quick, test)

let tests = ("List_dir", [list_dir_test])

let () = Alcotest.run "Filesystem functions" [tests]
