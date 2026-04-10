type 'prim t =
  | One of 'prim
  | Opt of 'prim t
  | Multi of 'prim t
  | Seq of 'prim t list
  | Choice of 'prim t list

val bind : ('a -> 'b t) -> 'a t -> 'b t
val pp : ('a -> string) -> 'a t -> string
val node : 'a t -> 'a t list

(* Legacy `diy7` parsing interprets a plain top-level sequence as a choice
   for backward compatibility. If the top-level sequence already mixes in an
   explicit `Choice`, keep the sequence structure so that `A|B,C` means
   `(A|B),C` rather than `A|B|C`. *)
val seq_as_choice : 'a t -> 'a t

(* Flatten the AST into the list of concrete sequences.
   - `One` and `Multi` directly upwraps
   - `Opt` contributes the either `'a list` or empty,
   - `Choice` unions alternatives,
      for example Choice [A;B] becomes [[A];[B]]
   - `Seq` computes the cross-product in order.
      for example Seq [A;B] becomes [[A*B]] *)
val to_list : 'a t -> 'a list list
