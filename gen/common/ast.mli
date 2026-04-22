(* Abstract syntax tree for the relax-input grammar.
   The type parameter `'prim` is the leaf token type produced by the parser.
   - `One x` is a single primitive token.
   - `Opt t` is an optional sub-expression, i.e., `(t)?`.
   - `Seq ts` is an ordered sequence of sub-expressions, i.e., `A B C`
     or `A,B,C`.
   - `Choice ts` is a choice between alternatives, i.e., `A|B|C`. *)
type 'prim t =
  | One of 'prim
  | Opt of 'prim t
  | Seq of 'prim t list
  | Choice of 'prim t list

val bind : 'a t -> ('a -> 'b t) -> 'b t
val pp : ('a -> string) -> 'a t -> string

(* Flatten the AST into the list of concrete sequences.
   - `One x` expands to `[[x]]`.
   - `Opt t` expands to the alternatives of `t`, plus the empty sequence.
   - `Choice list` concatenates the expansions of each alternative.
     For example, `Choice [A; B]` becomes `[[A]; [B]]`.
   - `Seq list` computes the ordered cross-product.
     If `A` expands to `[[X; Y]; [Z]]` and `B` to `[[K]; [M; N]]`,
     then `Seq [A; B]` expands to
     `[ [X; Y; K]; [X; Y; M; N]; [Z; K]; [Z; M; N] ]`.
   A concrete example is
   `Seq [Choice [One x; One y]; Opt (Seq [One z; One k])]`,
   which expands to `[[x]; [x; z; k]; [y]; [y; z; k]]`, where:
   - `[x]` comes from `One x` followed by the empty alternative of `Opt`.
   - `[x; z; k]` comes from `One x` followed by `Seq [One z; One k]`.
   - `[y]` comes from `One y` followed by the empty alternative of `Opt`.
   - `[y; z; k]` comes from `One y` followed by `Seq [One z; One k]`.
   *)
val expand : 'a t -> 'a list list
