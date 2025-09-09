module type S = sig
  type t

  val default : t
end

module No: S = struct
  type t = unit

  let default = ()
end

