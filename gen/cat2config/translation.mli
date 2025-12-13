type relax (* Type of diy relaxations *)

val pp_relax : relax -> string

(* Translate a cat relation into a (possibly-empty) list of diy relaxations.

   Each relaxation from this list is to be interpreted as a stand-alone,
   alternative representation, out of possibly many, of the input relation.
 *)
val translate : binding:string -> Ir.rel_nf -> relax list
