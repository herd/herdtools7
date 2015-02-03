type t = Gen | Trad

let tags = ["gen";"trad";]

let parse tag = match String.lowercase tag with
| "gen" -> Some Gen
| "trad" -> Some Trad
| _ -> None

let pp = function
  | Gen -> "gen"
  | Trad -> "trad"
