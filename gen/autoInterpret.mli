module type S = sig
  type t
  type outcome
  type relax
  type relax_set
  type count

  val pp : out_channel -> t -> unit

  val interpret : relax_set -> outcome -> t

  val intest : t -> relax_set

  val expand_cumul : t -> t

  val get_relaxed_assuming : relax_set -> t -> relax list -> relax list 

  val shows_relax : relax_set -> relax -> t -> bool

  val simplify_for_safes : relax_set -> relax_set -> t -> t option

  val safe_by_inter : t -> relax_set

  val safe_by_cardinal : t -> (relax_set * int) list -> (relax_set * int) list

  val unexplained : relax_set -> t -> t option

  val count : string -> relax_set -> t -> count -> count

end
