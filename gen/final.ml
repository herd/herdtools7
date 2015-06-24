(*********************************************************************)
(*                         Diy                                       *)
(*                                                                   *)
(*   Luc Maranget INRIA Paris-Rocquencourt, France.                  *)
(*                                                                   *)
(*  Copyright 2014 Institut National de Recherche en Informatique et *)
(*  en Automatique. All rights reserved. This file is distributed    *)
(*  under the terms of the Lesser GNU General Public License.        *)
(*********************************************************************)

module type Config = sig
  val verbose : int
  val cond : Config.cond
  val optcond : bool
  val coherence_decreasing : bool
  val hexa : bool
end

module Make : functor (O:Config) -> functor (C:ArchRun.S) ->
  sig
    type fenv = (C.A.location * IntSet.t) list
    type eventmap = C.A.location C.C.EventMap.t

(* Add an observation to fenv *)
    val add_final_v :
        Code.proc -> C.A.arch_reg -> IntSet.t -> fenv -> fenv
    val add_final :
        Code.proc -> C.A.arch_reg option -> C.C.node ->
          eventmap * fenv -> eventmap * fenv

    type final

    val check : fenv -> final
    val observe : fenv -> final
    val run : C.C.event list list -> C.A.location C.C.EventMap.t -> final

    val dump_final : out_channel ->  final -> unit

  end = functor (O:Config) -> functor (C:ArchRun.S) ->
  struct
    type fenv = (C.A.location * IntSet.t) list
    type eventmap = C.A.location C.C.EventMap.t

    let show_in_cond =
      if O.optcond then
        let valid_edge e =
          let open C.E in
          match e.C.E.edge with
          | Rf _ | Fr _ | Ws _ | Hat|Detour _|DetourWs _
          | Back _|Leave _ -> true
          | Po _ | Fenced _ | Dp _|Rmw -> false
          | Store -> assert false in
        (fun n -> valid_edge n.C.C.prev.C.C.edge || valid_edge n.C.C.edge)
      else
        (fun _ -> true)

    let add_final_v p r v finals = (C.A.of_reg p r,v)::finals

    let prev_value =
      if O.coherence_decreasing then fun v -> v+1
      else fun v -> v-1

    let add_final p o n finals = match o with
    | Some r ->
        let m,fs = finals in
        let evt = n.C.C.evt in
        let v = match evt.C.C.dir with
        | Code.R -> evt.C.C.v
        | Code.W -> prev_value evt.C.C.v in
        if show_in_cond n then
          C.C.EventMap.add n.C.C.evt (C.A.of_reg p r) m,
          add_final_v p r (IntSet.singleton v) fs
        else finals
    | None -> finals


    type final =
      | Exists of fenv 
      | Forall of (C.A.location * Code.v) list list
      | Locations of C.A.location list

    module Run = Run.Make(O)(C)

    let check f = Exists f
    let observe f = Locations (List.map fst f)
    let run evts m = Forall (Run.run evts m)

(* Dumping *)
    open Printf

    let dump_atom =
      if O.hexa then
        fun r v -> sprintf "%s=0x%x" (C.A.pp_location r) v
      else
        fun r v -> sprintf "%s=%i" (C.A.pp_location r) v

    let dump_state fs =
      String.concat " /\\ "
        (List.map
           (fun (r,vs) ->
             match IntSet.as_singleton vs with
             | Some v ->
                 dump_atom r v
             | None ->
                 let pp =
                   IntSet.pp_str " \\/ "
                     (fun v -> dump_atom r v)
                     vs in
                 sprintf "(%s)" pp)
           fs)

    let dump_final chan f = match f with
    | Exists fs ->
        fprintf chan "%sexists\n" (if !Config.neg then "~" else "") ;
        fprintf chan "(%s)\n" (dump_state fs)
    | Forall ffs ->
        fprintf chan "forall\n" ;
        fprintf chan "%s\n" (Run.dump_cond ffs)
    | Locations locs ->
        fprintf chan "locations [%s]\n"
          (String.concat ""
             (List.map (fun loc -> sprintf "%s;" (C.A.pp_location loc)) locs))

  end

