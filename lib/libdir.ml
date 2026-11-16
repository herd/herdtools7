let path tool =
  let directory =
    match Herdtools_paths.Sites.libdir with
    | dir :: _ -> Filename.dirname dir
    | [] ->
        Filename.concat Root_path.v
          "../install/default/share/herdtools7"
  in
  Filename.concat directory tool
