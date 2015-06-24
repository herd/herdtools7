(*********************************************************************)
(*                         Diy                                       *)
(*                                                                   *)
(*   Luc Maranget INRIA Paris-Rocquencourt, France.                  *)
(*                                                                   *)
(*  Copyright 2014 Institut National de Recherche en Informatique et *)
(*  en Automatique. All rights reserved. This file is distributed    *)
(*  under the terms of the Lesser GNU General Public License.        *)
(*********************************************************************)

(* Atomicity of events *)
module type Config = sig
  val naturalsize : MachSize.sz option
  val endian : MachSize.endian
end

module Make(C:Config) = struct
  open MachSize

  module Mixed = MachMixed.Make(C)

  let bellatom = false

  type hidden_atom = Atomic | Reserve | Mixed of MachMixed.t
  type atom = hidden_atom

  let default_atom = Atomic

  open Code

  let applies_atom a d = match a,d with
  | Reserve,W -> false
  | _,_ -> true

  let applies_atom_rmw ar aw = match ar,aw with
  | None,None -> true
  | _,_ -> false

  let pp_plain = Code.plain
  let pp_as_a = None

  let pp_atom = function
    | Atomic -> "A"
    | Reserve -> "R"
    | Mixed mix -> Mixed.pp_mixed mix

  let compare_atom = Pervasives.compare

  let fold_mixed f r = Mixed.fold_mixed (fun mix r -> f (Mixed mix) r) r

  let fold_atom f r =
    let r = fold_mixed f r in
    f Reserve (f Atomic r)

  let worth_final = function
    | Atomic -> true
    | Reserve -> false
    | Mixed _ -> false

  let varatom_dir _d f = f None

  let tr_value ao v = match ao with
  | None| Some (Atomic|Reserve) -> v
  | Some (Mixed (sz,_)) -> Mixed.tr_value sz v


  let correct_offset = match C.endian with
  | Little -> fun _ o -> o
  | Big ->
      begin match C.naturalsize with
      | None -> fun _ _ -> assert false
      | Some nsz ->
          fun sz o ->
            let bsz = nbytes sz in
            let bo = o / bsz in
            let no = bsz * ((nbytes nsz/bsz)-bo-1) in
(*            Printf.eprintf "tr: %i -> %i\n" o no ; *)
            no
      end

  let overwrite_value v ao w = match ao with
  | None| Some (Atomic|Reserve) -> w (* total overwrite *)
  | Some (Mixed (sz,o)) ->
      if sz = Misc.as_some C.naturalsize then w
      else
        let o = correct_offset sz o in
        let sz_bits =  MachSize.nbits sz in
        let nshift =  o * 8 in
        let wshifted = w lsl nshift in
        let mask = lnot (((1 lsl sz_bits) - 1) lsl nshift) in
        (v land mask) lor wshifted

  let extract_value v ao = match ao with
  | None| Some (Atomic|Reserve) -> v
  | Some (Mixed (sz,o)) ->
      let sz_bits =  MachSize.nbits sz in
      let o = correct_offset sz o in
      let nshift =  o * 8 in
      let mask =
        match sz with
        | Quad -> -1
        | _ -> (1 lsl sz_bits) - 1 in
      let r = (v lsr nshift) land mask in
(*      Printf.eprintf "EXTRACT (%s,%i)[0x%x]: 0x%x -> 0x%x\n"
        (MachSize.pp sz) o mask v r ; *)
      r

end
