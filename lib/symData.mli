module type S = sig
  type t

  val default : t
end

module No: S

