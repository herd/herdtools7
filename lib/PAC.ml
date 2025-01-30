
(* A vritual address must have a PAC field represented in the most significant
   bits of the pointer, this PAC field may contani multiple PAC signatures using
   the XOR off all the signatures *)
type signature =
  {
    (* "ia", "da", "ib" or "db" *)
    key : string ;
    (* modifier: a string used as a "salt" added to the hash *)
    modifier : string ;
    (* the offset of the pointer at the time we compute the PAC field *)
    offset : int ;
  }

let pp_signature p s =
  Printf.sprintf "pac%s(%s, %s, %d)" p.key s p.modifier p.offset

let compare_signature p1 p2 =
  match String.compare p1.key p2.key with
  | 0 ->
      begin match String.compare p1.modifier p2.modifier with
      | 0 -> Int.compare p1.offset p2.offset
      | r -> r
      end
  | r -> r

module PacSet = Set.Make (struct
  type t = signature
  let compare = compare_signature
end)

module PacMap = Map.Make (struct
  type t = signature
  let compare = compare_signature
end)

type t = PacSet.t

let canonical = PacSet.empty

let is_canonical pac =
  PacSet.is_empty pac

(* add a PAC signature in a PAC field using an exclusive OR *)
let add key modifier offset pac =
  if PacSet.mem {modifier; key; offset} pac then
    PacSet.remove {modifier; key; offset} pac
  else
    PacSet.add {modifier; key; offset} pac

let compare = PacSet.compare

let equal = PacSet.equal

let pp pac s =
  let rec aux = function
    | x :: xs -> pp_signature x (aux xs)
    | [] -> s
  in
  aux (PacSet.to_list pac)

