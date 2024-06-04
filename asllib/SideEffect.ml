open AST

type t =
  | ReadLocal of identifier
  | WriteLocal of identifier
  | ReadGlobal of identifier
  | WriteGlobal of identifier
  | Throwing of identifier
  | StringSpecified of string
  | RecursiveCall of identifier

let pp_print f =
  let open Format in
  function
  | ReadLocal s -> fprintf f "ReadLocal %S" s
  | WriteLocal s -> fprintf f "WriteLocal %S" s
  | ReadGlobal s -> fprintf f "ReadGlobal %S" s
  | WriteGlobal s -> fprintf f "WriteGlobal %S" s
  | Throwing s -> fprintf f "Throwing %S" s
  | StringSpecified s -> fprintf f "StringSpecified %S" s
  | RecursiveCall s -> fprintf f "RecursiveCall %S" s

let equal t1 t2 =
  match (t1, t2) with
  | ReadLocal s1, ReadLocal s2
  | WriteLocal s1, WriteLocal s2
  | ReadGlobal s1, ReadGlobal s2
  | WriteGlobal s1, WriteGlobal s2
  | StringSpecified s1, StringSpecified s2
  | RecursiveCall s1, RecursiveCall s2
  | Throwing s1, Throwing s2 ->
      String.equal s1 s2
  | _ -> false

let compare t1 t2 =
  match (t1, t2) with
  | ReadLocal s1, ReadLocal s2
  | WriteLocal s1, WriteLocal s2
  | ReadGlobal s1, ReadGlobal s2
  | WriteGlobal s1, WriteGlobal s2
  | StringSpecified s1, StringSpecified s2
  | RecursiveCall s1, RecursiveCall s2
  | Throwing s1, Throwing s2 ->
      String.compare s1 s2
  | ( ReadLocal _,
      ( WriteLocal _ | ReadGlobal _ | WriteGlobal _ | StringSpecified _
      | Throwing _ | RecursiveCall _ ) )
  | ( WriteLocal _,
      ( ReadGlobal _ | WriteGlobal _ | StringSpecified _ | Throwing _
      | RecursiveCall _ ) )
  | ( ReadGlobal _,
      (WriteGlobal _ | StringSpecified _ | Throwing _ | RecursiveCall _) )
  | WriteGlobal _, (StringSpecified _ | Throwing _ | RecursiveCall _)
  | StringSpecified _, (Throwing _ | RecursiveCall _)
  | Throwing _, RecursiveCall _ ->
      1
  | ( ( WriteLocal _ | ReadGlobal _ | WriteGlobal _ | StringSpecified _
      | Throwing _ | RecursiveCall _ ),
      ReadLocal _ )
  | ( ( ReadGlobal _ | WriteGlobal _ | StringSpecified _ | Throwing _
      | RecursiveCall _ ),
      WriteLocal _ )
  | ( (WriteGlobal _ | StringSpecified _ | Throwing _ | RecursiveCall _),
      ReadGlobal _ )
  | (StringSpecified _ | Throwing _ | RecursiveCall _), WriteGlobal _
  | (Throwing _ | RecursiveCall _), StringSpecified _
  | RecursiveCall _, Throwing _ ->
      -1

let write_global s = WriteGlobal s
let read_global s = ReadGlobal s
let read_local s = ReadLocal s
let write_local s = WriteLocal s
let throwing s = Throwing s
let recursive_call s = RecursiveCall s
let string_specified s = StringSpecified s
let implicitly_thrown = Throwing "__IMPLICITLY_THROWN__"
