type 'a inter = Inter of 'a list
type 'a seq = Seq of 'a list
type 'a union = Union of 'a list

val inter : 'a inter -> 'a inter -> 'a inter
val get_inter : 'a inter -> 'a list
val get_union : 'a union -> 'a list

type fence = AArch64Base.barrier
type prim_rel = Fence of fence | Prim of string

type prim_set =
  | Domain of seq_item seq
  | Range of seq_item seq
  | Fence of fence option
  | Comp of prim_set
  | Prim of string

and seq_item = Set of prim_set inter | Rel of prim_rel inter
and set_nf = prim_set inter union
and rel_nf = seq_item seq union

include
  Normalization.S
    with type fence := fence
     and type rel_nf := rel_nf
     and type set_nf := set_nf

(* Merges adjacent reflexive relations.
   Mostly needed for pretty-printing.

   Example:

     r1; [s1]; [s2]; r2   -->   r1; [s1 & s2]; r2
 *)
val compress : rel_nf -> rel_nf

(* Turns eligible instances of reflexive relations built using the set-valued
   functions `domain` and `range` into relations that are equivalent w.r.t.
   translation to diy7 relaxations.

   Currently only instances of domain/range containing `amo` relations are
   supported.

   Example:

     [M & range([A]; amo; [L])]; r   -->   [M]; [A]; amo; [L]; r
 *)
val expand_domain_range : rel_nf -> rel_nf

(* Expands occurrences of `A` and `L` effect variables into a set expression
   which is the union of the original variable and a set expression generating
   the same acquire/release effect via atomic memory operations.

   Example:

     [M & A]  -->  [M & (A | domain([A]; amo))]
              -->  [M & A] | [M & domain([A]; amo))]
 *)
val expand_acq_rel : rel_nf -> rel_nf
