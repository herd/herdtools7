(*********************************************************************)
(*                         Diy                                       *)
(*                                                                   *)
(*   Luc Maranget INRIA Paris-Rocquencourt, France.                  *)
(*                                                                   *)
(*  Copyright 2015 Institut National de Recherche en Informatique et *)
(*  en Automatique. All rights reserved. This file is distributed    *)
(*  under the terms of the Lesser GNU General Public License.        *)
(*********************************************************************)

module type Config = sig
  val naturalsize : MachSize.sz option
end

open MachSize
type offset = int
type t = sz * offset

module Make(C:Config) = struct

  open Printf

  let pp_mixed = function (sz,o) -> sprintf "%s%i" (MachSize.pp sz) o

  let do_fold f sz xs r = List.fold_right (fun o r -> f (sz,o) r) xs r

  let off_word = function
    | Byte -> [0;1;2;3;4;]
    | Short -> [0;2;]
    | Word -> [0;]
    | Quad -> []

  let off_quad = function
    | Byte -> [0;1;2;3;4;5;6;7;]
    | Short -> [0;2;4;6;]
    | Word -> [0;4;]
    | Quad -> [0;]

  let get_off = match C.naturalsize with
  | None -> fun _ -> []
  | Some (Byte|Short) -> assert false
  | Some Word -> off_word
  | Some Quad -> off_quad


  let fold_mixed f r =
    let r = do_fold f Byte (get_off Byte) r in
    let r = do_fold f Short (get_off Short) r in
    let r = do_fold f Word (get_off Word) r in
    let r = do_fold f Quad (get_off Quad) r in
    r

  let rec tr_value sz v = match sz with
  | Byte -> v
  | Short -> v lsl 8 + v
  | Word ->  v lsl 24 + v lsl 16 + v lsl 8 + v
  | Quad ->
      let x = tr_value Word v in
      x lsl 32 + x
end
