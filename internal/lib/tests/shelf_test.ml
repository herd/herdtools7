(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2010-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

(** Tests for the Shelf module. *)

let tests = [
  "Shelf.of_file reads simple valid files", (fun () ->
    let tmp_dir = Filesystem.new_temp_dir () in
    let shelf_path = Filename.concat tmp_dir "shelf.py" in
    let shelf_py o =
      Printf.fprintf o "record = 'AArch64'\n" ;
      Printf.fprintf o "cats = [ 'cats/sc.cat' ]\n" ;
      Printf.fprintf o "cfgs = [ 'cfgs/web.cfg' ]\n" ;
      Printf.fprintf o "bells = [ 'bells/tabby.bell' ]\n" ;
      Printf.fprintf o "illustrative_tests = [ \n" ;
      Printf.fprintf o "  'tests/meow.litmus',\n" ;
      Printf.fprintf o "  'tests/mew.litmus',\n" ;
      Printf.fprintf o "]\n" ;
      Printf.fprintf o "references = [ { 'title': 'Garfield' } ]\n" ;
      ()
    in
    Filesystem.write_file shelf_path shelf_py ;

    let expected = let open Shelf in {
      record = "AArch64" ;
      cats = [ "cats/sc.cat" ] ;
      configs = [ "cfgs/web.cfg" ] ;
      tests = [ "tests/meow.litmus"; "tests/mew.litmus" ] ;
      bells = Some [ "bells/tabby.bell" ] ;
      compatibilities = None ;
    } in

    let actual = Shelf.of_file shelf_path in

    Filesystem.remove_recursive tmp_dir ;

    if Shelf.compare expected actual <> 0 then
      Test.fail (Printf.sprintf "expected %s, got %s"
        (Shelf.to_ocaml_string expected)
        (Shelf.to_ocaml_string actual)
      )
  );

  "Shelf.of_file reads valid files with non-standard names", (fun () ->
    let tmp_dir = Filesystem.new_temp_dir () in
    let shelf_path = Filename.concat tmp_dir "shlaf shelf.py" in
    let shelf_py o =
      Printf.fprintf o "record = 'AArch64'\n" ;
      Printf.fprintf o "cats = [ 'cats/sc.cat' ]\n" ;
      Printf.fprintf o "cfgs = [ 'cfgs/web.cfg' ]\n" ;
      Printf.fprintf o "illustrative_tests = [ \n" ;
      Printf.fprintf o "  'tests/meow.litmus',\n" ;
      Printf.fprintf o "  'tests/mew.litmus',\n" ;
      Printf.fprintf o "]\n" ;
      ()
    in
    Filesystem.write_file shelf_path shelf_py ;

    let expected = let open Shelf in {
      record = "AArch64" ;
      cats = [ "cats/sc.cat" ] ;
      configs = [ "cfgs/web.cfg" ] ;
      tests = [ "tests/meow.litmus"; "tests/mew.litmus" ] ;
      bells = None ;
      compatibilities = None ;
    } in

    let actual = Shelf.of_file shelf_path in

    Filesystem.remove_recursive tmp_dir ;

    if Shelf.compare expected actual <> 0 then
      Test.fail (Printf.sprintf "expected %s, got %s"
        (Shelf.to_ocaml_string expected)
        (Shelf.to_ocaml_string actual)
      )
  );

  "Shelf.of_file sorts lists for stability", (fun () ->
    let tmp_dir = Filesystem.new_temp_dir () in
    let shelf_path = Filename.concat tmp_dir "shlaf shelf.py" in
    let shelf_py o =
      Printf.fprintf o "record = 'AArch64'\n" ;
      Printf.fprintf o "cats = [ 'cats/sc.cat' ]\n" ;
      Printf.fprintf o "cfgs = [ 'cfgs/web.cfg' ]\n" ;
      Printf.fprintf o "illustrative_tests = [ \n" ;
      Printf.fprintf o "  'tests/b.litmus',\n" ;
      Printf.fprintf o "  'tests/a.litmus',\n" ;
      Printf.fprintf o "]\n" ;
      ()
    in
    Filesystem.write_file shelf_path shelf_py ;

    let expected = let open Shelf in {
      record = "AArch64" ;
      cats = [ "cats/sc.cat" ] ;
      configs = [ "cfgs/web.cfg" ] ;
      tests = [ "tests/a.litmus"; "tests/b.litmus" ] ;
      bells = None ;
      compatibilities = None ;
    } in

    let actual = Shelf.of_file shelf_path in

    Filesystem.remove_recursive tmp_dir ;

    if Shelf.compare expected actual <> 0 then
      Test.fail (Printf.sprintf "expected %s, got %s"
        (Shelf.to_ocaml_string expected)
        (Shelf.to_ocaml_string actual)
      )
  );

  "Shelf.of_file reads valid files with globs", (fun () ->
    let tmp_dir = Filesystem.new_temp_dir () in
    let shelf_path = Filename.concat tmp_dir "shelf.py" in
    let shelf_py o =
      Printf.fprintf o "import glob\n" ;
      Printf.fprintf o "record = 'AArch64'\n" ;
      Printf.fprintf o "cats = [ 'cats/sc.cat' ]\n" ;
      Printf.fprintf o "cfgs = [ 'cfgs/web.cfg' ]\n" ;
      Printf.fprintf o "illustrative_tests = glob.glob( '*.litmus' ) \n" ;
      ()
    in
    Filesystem.write_file (Filename.concat tmp_dir "mew.litmus") (fun _ -> ()) ;
    Filesystem.write_file (Filename.concat tmp_dir "purr.litmus") (fun _ -> ()) ;
    Filesystem.write_file shelf_path shelf_py ;

    let expected = let open Shelf in {
      record = "AArch64" ;
      cats = [ "cats/sc.cat" ] ;
      configs = [ "cfgs/web.cfg" ] ;
      tests = [ "mew.litmus"; "purr.litmus" ] ;
      bells = None ;
      compatibilities = None ;
    } in

    let actual = Shelf.of_file shelf_path in

    Filesystem.remove_recursive tmp_dir ;

    if Shelf.compare expected actual <> 0 then
      Test.fail (Printf.sprintf "expected %s, got %s"
        (Shelf.to_ocaml_string expected)
        (Shelf.to_ocaml_string actual)
      )
  );
]

let () = Test.run tests
