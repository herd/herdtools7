(* Java Access Modes *)

type t = Plain | Opaque | Acquire | Release | Volatile | NA

let compare = compare

let pp_access_modes = function
| Plain   -> "Plain"
| Opaque  -> "Opaque"
| Acquire -> "Acquire"
| Release -> "Release"
| Volatile -> "Volatile"
| NA      -> "NA"