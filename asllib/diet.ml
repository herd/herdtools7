(*
 * Copyright (C) 2016 David Scott <dave@recoil.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 *)

[@@@warning "-44"]

module type ELT = sig
  type t

  val compare : t -> t -> int
  val zero : t
  val pred : t -> t
  val succ : t -> t
  val sub : t -> t -> t
  val add : t -> t -> t
  val to_string : t -> string
end

module type INTERVAL_SET = sig
  type elt
  type interval

  module Interval : sig
    val make : elt -> elt -> interval
    val x : interval -> elt
    val y : interval -> elt
  end

  type t

  val equal : t -> t -> bool
  val compare : t -> t -> int
  val pp : Format.formatter -> t -> unit
  val empty : t
  val is_empty : t -> bool
  val singleton : elt -> t
  val cardinal : t -> elt
  val mem : elt -> t -> bool
  val fold : (interval -> 'a -> 'a) -> t -> 'a -> 'a
  val fold_individual : (elt -> 'a -> 'a) -> t -> 'a -> 'a
  val iter : (interval -> unit) -> t -> unit
  val add : interval -> t -> t
  val remove : interval -> t -> t
  val min_elt : t -> interval
  val max_elt : t -> interval
  val choose : t -> interval
  val take : t -> elt -> (t * t) option
  val union : t -> t -> t
  val diff : t -> t -> t
  val inter : t -> t -> t
  val find_next_gap : elt -> t -> elt
  val check_invariants : t -> (unit, string) result
  val height : t -> int
end

module Make (Elt : ELT) = struct
  type elt = Elt.t

  module Elt = struct
    include Elt

    let ( - ) = sub
    let ( + ) = add
  end

  type interval = elt * elt

  module Interval = struct
    let make x y =
      if x > y then invalid_arg "Interval.make";
      (x, y)

    let x = fst
    let y = snd
  end

  let ( > ) x y = Elt.compare x y > 0
  let ( >= ) x y = Elt.compare x y >= 0
  let ( < ) x y = Elt.compare x y < 0
  let ( <= ) x y = Elt.compare x y <= 0
  let eq x y = Elt.compare x y = 0
  let succ, pred = (Elt.succ, Elt.pred)

  type t = Empty | Node : node -> t
  and node = { x : elt; y : elt; l : t; r : t; h : int; cardinal : elt }

  let rec cons_enum t enum =
    match t with
    | Empty -> enum
    | Node ({ l; _ } as node) -> cons_enum l (node :: enum)

  let compare_with_invariant { x; y; _ } { x = x'; y = y'; _ } =
    if eq x x' && eq y y' then 0 else if y < x' then -1 else 1

  let rec compare_aux enum enum' =
    match (enum, enum') with
    | [], [] -> 0
    | [], _ -> -1
    | _, [] -> 1
    | node :: enum, node' :: enum' -> (
        match compare_with_invariant node node' with
        | 0 -> compare_aux (cons_enum node.r enum) (cons_enum node'.r enum')
        | c -> c)

  let compare t t' = compare_aux (cons_enum t []) (cons_enum t' [])
  let equal t t' = compare t t' = 0

  let rec pp fmt = function
    | Empty -> Format.fprintf fmt "Empty"
    | Node n -> pp_node fmt n

  and pp_node fmt { x; y; l; r; h; cardinal } =
    Format.pp_open_vbox fmt 0;
    Format.fprintf fmt "x: %s@," (Elt.to_string x);
    Format.fprintf fmt "y: %s@," (Elt.to_string y);
    Format.fprintf fmt "l:@[@\n%a@]@," pp l;
    Format.fprintf fmt "r:@[@\n%a@]@," pp r;
    Format.fprintf fmt "h: %d@," h;
    Format.fprintf fmt "cardinal: %s" (Elt.to_string cardinal);
    Format.pp_close_box fmt ()

  let height = function Empty -> 0 | Node n -> n.h
  let cardinal = function Empty -> Elt.zero | Node n -> n.cardinal

  let create x y l r =
    let h = max (height l) (height r) + 1 in
    let cardinal = Elt.(succ (y - x) + cardinal l + cardinal r) in
    Node { x; y; l; r; h; cardinal }

  let rec node x y l r =
    let hl = height l and hr = height r in
    let open Stdlib in
    if hl > hr + 2 then
      match l with
      | Empty -> assert false
      | Node { x = lx; y = ly; l = ll; r = lr; _ } -> (
          if height ll >= height lr then node lx ly ll (node x y lr r)
          else
            match lr with
            | Empty -> assert false
            | Node { x = lrx; y = lry; l = lrl; r = lrr; _ } ->
                node lrx lry (node lx ly ll lrl) (node x y lrr r))
    else if hr > hl + 2 then
      match r with
      | Empty -> assert false
      | Node { x = rx; y = ry; l = rl; r = rr; _ } -> (
          if height rr >= height rl then node rx ry (node x y l rl) rr
          else
            match rl with
            | Empty -> assert false
            | Node { x = rlx; y = rly; l = rll; r = rlr; _ } ->
                node rlx rly (node x y l rll) (node rx ry rlr rr))
    else create x y l r

  let depth tree =
    let rec depth tree k =
      match tree with
      | Empty -> k 0
      | Node n -> depth n.l (fun dl -> depth n.r (fun dr -> k (1 + max dl dr)))
    in
    depth tree (fun d -> d)

  module Invariant = struct
    let ( >>= ) xr f = match xr with Ok x -> f x | e -> e

    let ensure b msg t =
      if b then Ok () else Error (Format.asprintf "%s: %a" msg pp t)

    let rec on_every_node d f =
      match d with
      | Empty -> Ok ()
      | Node n ->
          f n d >>= fun () ->
          on_every_node n.l f >>= fun () -> on_every_node n.r f

    (* The pairs (x, y) in each interval are ordered such that x <= y *)
    let ordered { x; y; _ } =
      ensure (x <= y) "Pairs within each interval should be ordered"

    (* The intervals don't overlap *)
    let no_overlap { x; y; l; r; _ } n =
      let error = "Intervals should be ordered without overlap" in
      (match l with Empty -> Ok () | Node left -> ensure (left.y < x) error n)
      >>= fun () ->
      match r with Empty -> Ok () | Node right -> ensure (right.x > y) error n

    let no_adjacent { x; y; l; r; _ } n =
      let error = "Intervals should not be adjacent" in
      (match l with
      | Empty -> Ok ()
      | Node left -> ensure (Elt.succ left.y < x) error n)
      >>= fun () ->
      match r with
      | Empty -> Ok ()
      | Node right -> ensure (Elt.pred right.x > y) error n

    let node_height n = n.h
    let node_depth n = depth (Node n)

    (* The height is being stored correctly *)
    let height_equals_depth n =
      ensure
        (node_height n = node_depth n)
        "The height is not being maintained correctly"

    let balanced { l; r; _ } =
      let diff = height l - height r in
      let open Stdlib in
      ensure (-2 <= diff && diff <= 2) "The tree has become imbalanced"

    let check_cardinal { x; y; l; r; cardinal = c; _ } =
      ensure
        Elt.(c - cardinal l - cardinal r - y + x = succ zero)
        "The cardinal value stored in the node is wrong"

    let check t =
      on_every_node t ordered >>= fun () ->
      on_every_node t no_overlap >>= fun () ->
      on_every_node t height_equals_depth >>= fun () ->
      on_every_node t balanced >>= fun () ->
      on_every_node t check_cardinal >>= fun () -> on_every_node t no_adjacent
  end

  let empty = Empty
  let is_empty = function Empty -> true | _ -> false

  let rec mem elt = function
    | Empty -> false
    | Node n ->
        (* consider this interval *)
        (elt >= n.x && elt <= n.y)
        ||
        (* or search left or search right *)
        if elt < n.x then mem elt n.l else mem elt n.r

  let rec min_elt = function
    | Empty -> raise Not_found
    | Node { x; y; l = Empty; _ } -> (x, y)
    | Node { l; _ } -> min_elt l

  let rec max_elt = function
    | Empty -> raise Not_found
    | Node { x; y; r = Empty; _ } -> (x, y)
    | Node { r; _ } -> max_elt r

  let choose = function Empty -> raise Not_found | Node { x; y; _ } -> (x, y)

  (* fold over the maximal contiguous intervals *)
  let rec fold f t acc =
    match t with
    | Empty -> acc
    | Node n ->
        let acc = fold f n.l acc in
        let acc = f (n.x, n.y) acc in
        fold f n.r acc

  (* fold over individual elements *)
  let fold_individual f t acc =
    let range (from, upto) acc =
      let rec loop acc x =
        if eq x (succ upto) then acc else loop (f x acc) (succ x)
      in
      loop acc from
    in
    fold range t acc

  (* iterate over maximal contiguous intervals *)
  let iter f t =
    let f' itl () = f itl in
    fold f' t ()

  (* return (x, y, l) where (x, y) is the maximal interval and [l] is
     the rest of the tree on the left (whose intervals are all smaller). *)
  let rec splitMax = function
    | { x; y; l; r = Empty; _ } -> (x, y, l)
    | { r = Node r; _ } as n ->
        let u, v, r' = splitMax r in
        (u, v, node n.x n.y n.l r')

  (* return (x, y, r) where (x, y) is the minimal interval and [r] is
     the rest of the tree on the right (whose intervals are all larger) *)
  let rec splitMin = function
    | { x; y; l = Empty; r; _ } -> (x, y, r)
    | { l = Node l; _ } as n ->
        let u, v, l' = splitMin l in
        (u, v, node n.x n.y l' n.r)

  let addL = function
    | { l = Empty; _ } as n -> n
    | { l = Node l; _ } as n ->
        (* we might have to merge the new element with the maximal interval from
           the left *)
        let x', y', l' = splitMax l in
        if eq (succ y') n.x then { n with x = x'; l = l' } else n

  let addR = function
    | { r = Empty; _ } as n -> n
    | { r = Node r; _ } as n ->
        (* we might have to merge the new element with the minimal interval on
           the right *)
        let x', y', r' = splitMin r in
        if eq (succ n.y) x' then { n with y = y'; r = r' } else n

  let rec add (x, y) t =
    if y < x then invalid_arg "interval reversed";
    match t with
    | Empty -> node x y Empty Empty
    (* completely to the left *)
    | Node n when y < Elt.pred n.x ->
        let l = add (x, y) n.l in
        node n.x n.y l n.r
    (* completely to the right *)
    | Node n when Elt.succ n.y < x ->
        let r = add (x, y) n.r in
        node n.x n.y n.l r
    (* overlap on the left only *)
    | Node n when x < n.x && y <= n.y ->
        let l = add (x, pred n.x) n.l in
        let n = addL { n with l } in
        node n.x n.y n.l n.r
    (* overlap on the right only *)
    | Node n when y > n.y && x >= n.x ->
        let r = add (succ n.y, y) n.r in
        let n = addR { n with r } in
        node n.x n.y n.l n.r
    (* overlap on both sides *)
    | Node n when x < n.x && y > n.y ->
        let l = add (x, pred n.x) n.l in
        let r = add (succ n.y, y) n.r in
        let n = addL { (addR { n with r }) with l } in
        node n.x n.y n.l n.r
    (* completely within *)
    | Node n -> Node n

  let union a b =
    let a' = cardinal a and b' = cardinal b in
    if a' > b' then fold add b a else fold add a b

  let merge l r =
    match (l, r) with
    | l, Empty -> l
    | Empty, r -> r
    | Node l, r ->
        let x, y, l' = splitMax l in
        node x y l' r

  let rec remove (x, y) t =
    if y < x then invalid_arg "interval reversed";
    match t with
    | Empty -> Empty
    (* completely to the left *)
    | Node n when y < n.x ->
        let l = remove (x, y) n.l in
        node n.x n.y l n.r
    (* completely to the right *)
    | Node n when n.y < x ->
        let r = remove (x, y) n.r in
        node n.x n.y n.l r
    (* overlap on the left only *)
    | Node n when x < n.x && y < n.y ->
        let n' = node (succ y) n.y n.l n.r in
        remove (x, pred n.x) n'
    (* overlap on the right only *)
    | Node n when y > n.y && x > n.x ->
        let n' = node n.x (pred x) n.l n.r in
        remove (succ n.y, y) n'
    (* overlap on both sides *)
    | Node n when x <= n.x && y >= n.y ->
        let l = remove (x, n.x) n.l in
        let r = remove (n.y, y) n.r in
        merge l r
    (* completely within *)
    | Node n when eq y n.y -> node n.x (pred x) n.l n.r
    | Node n when eq x n.x -> node (succ y) n.y n.l n.r
    | Node n ->
        assert (n.x <= pred x);
        assert (succ y <= n.y);
        let r = node (succ y) n.y Empty n.r in
        node n.x (pred x) n.l r

  let diff a b = fold remove b a
  let inter a b = diff a (diff a b)

  let rec find_next_gap from = function
    | Empty -> from
    | Node n ->
        (* consider this interval *)
        if from >= n.x && from <= n.y then succ n.y (* or search left *)
        else if from < n.x then find_next_gap from n.l (* or search right *)
        else find_next_gap from n.r

  let take t n =
    let rec loop acc free n =
      if n = Elt.zero then Some (acc, free)
      else
        match
          try
            let i = choose free in
            let x, y = Interval.(x i, y i) in
            let len = Elt.(succ @@ (y - x)) in
            let will_use = if Stdlib.(Elt.compare n len < 0) then n else len in
            let i' = Interval.make x Elt.(pred @@ (x + will_use)) in
            Some (add i' acc, remove i' free, Elt.(n - will_use))
          with Not_found -> None
        with
        | Some (acc', free', n') -> loop acc' free' n'
        | None -> None
    in
    loop empty t n

  let check_invariants = Invariant.check
  let singleton x = add (Interval.make x x) empty

  let pp_interval fmt i =
    let x, y = Interval.(x i, y i) in
    if eq x y then Format.fprintf fmt "{%s}" (Elt.to_string x)
    else Format.fprintf fmt "[%s, %s]" (Elt.to_string x) (Elt.to_string y)

  let pp fmt =
    let open Format in
    function
    | Empty -> fprintf fmt "∅"
    | t ->
        let m = min_elt t in
        let t = remove m t in
        pp_open_hovbox fmt 0;
        pp_interval fmt m;
        iter
          (fun i ->
            fprintf fmt "@ \u{222a} ";
            pp_interval fmt i)
          t;
        pp_close_box fmt ()
end

module Int_elt = struct
  type t = int

  let compare a b = compare (a : int) b
  let zero = 0
  let pred = pred
  let succ = succ
  let sub = ( - )
  let add = ( + )
  let to_string = string_of_int
end

module Int = Make (Int_elt)
module Int64 = Make (Int64)
module Z = Make (Z)
