(* Abstract syntax tree for the relax-input grammar.
   The type parameter `'prim` is the leaf token type produced by the parser.
   The type parameter `'pred` is the predicate type.
   - `One x` is a single primitive token.
   - `Opt t` is an optional sub-expression, i.e., `(t)?`.
   - `Seq ts` is an ordered sequence of sub-expressions, i.e., `A B C`
     or `A,B,C`.
   - `Choice ts` is a choice between alternatives, i.e., `A|B|C`.
   - `Predicate pred t` is an ast `t` decorated by predicate `pred`,
      i.e., `@after(t)` where `after` is the predicate,
   *)
type ('pred,'prim) t =
  | One of 'prim
  | Opt of ('pred,'prim) t
  | Seq of ('pred,'prim) t list
  | Choice of ('pred,'prim) t list
  | Predicate of 'pred * ('pred,'prim) t

val bind : ('pred,'prim) t -> ('prim -> ('pred,'new_prim) t) -> ('pred,'new_prim) t
val map_predicate : ('pred -> 'new_pred) -> ('pred,'prim) t -> ('new_pred,'prim) t
val pp : ('pred -> string) -> ('prim -> string) -> ('pred,'prim) t -> string

(* Flatten the AST into the list of concrete sequences.
   - `One x` expands to `[[x]]`.
   - `Opt t` expands to the alternatives of `t`, plus the empty sequence.
   - `Choice list` concatenates the expansions of each alternative.
     For example, `Choice [A; B]` becomes `[[A]; [B]]`.
   - `Seq list` computes the ordered cross-product.
     If `A` expands to `[[X; Y]; [Z]]` and `B` to `[[K]; [M; N]]`,
     then `Seq [A; B]` expands to
     `[ [X; Y; K]; [X; Y; M; N]; [Z; K]; [Z; M; N] ]`.
   - `Predicate (pred, t)` expands `t` first then calls the passed in
      function to combine `pred` into the expanded `t`.
   A concrete example is
   `Seq [Choice [One x; One y]; Opt (Seq [One z; One k])]`,
   which expands to `[[x]; [x; z; k]; [y]; [y; z; k]]`, where:
   - `[x]` comes from `One x` followed by the empty alternative of `Opt`.
   - `[x; z; k]` comes from `One x` followed by `Seq [One z; One k]`.
   - `[y]` comes from `One y` followed by the empty alternative of `Opt`.
   - `[y; z; k]` comes from `One y` followed by `Seq [One z; One k]`.
   *)
val expand : ('pred -> 'prim -> 'prim) -> ('pred,'prim) t -> 'prim list list
