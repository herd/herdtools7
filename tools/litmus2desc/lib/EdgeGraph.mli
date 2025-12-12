type t
type vertex = int
type edge = { label : string; source : vertex; target : vertex }

val make : vertex list -> edge list -> t

val simple_paths_iter :
  src:vertex ->
  dst:vertex ->
  pred:(src:vertex -> tgt:vertex -> string -> bool) ->
  t ->
  vertex list Util.Iter.t
