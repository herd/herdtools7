(*********************************************************************)
(*                          Litmus                                   *)
(*                                                                   *)
(*        Luc Maranget, INRIA Paris-Rocquencourt, France.            *)
(*        Susmit Sarkar, University of Cambridge, UK.                *)
(*                                                                   *)
(*  Copyright 2010 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

(***********)
(* Control *)
(***********)

val save_temps : bool ref
val debug : bool ref

(****************************)
(* Write & read lines       *)
(****************************)
val output_line : out_channel -> string -> unit
val output_lines : out_channel -> string list -> unit
val print_lines : string list -> unit
val read_by_line : in_channel -> (string -> 'a -> 'a) -> 'a -> 'a
val read_list : in_channel -> (string -> 'a option) -> 'a list

(****************)
(* Remove files *)
(****************)
val remove : string -> unit


(****************)
(* Select lines *)
(****************)
val grep : out_channel -> string -> string -> unit

(***********************************)
(* Call function on each file line *)
(***********************************)

val cat_chan : in_channel -> (string -> unit) -> unit
val cat : string -> (string -> unit) -> unit
val cp : in_channel -> string -> unit

(* and remove file *)
val cat_and_remove : string -> (string -> unit) -> unit

(**********************)
(* Call unix commands *)
(**********************)

val exec_stdout : string -> unit
val exec : string -> (string -> unit) -> unit
val mkdir : string -> unit
val mktmpdir : unit -> string
val rmdir : string -> unit
