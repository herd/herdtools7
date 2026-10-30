include Yojson.Basic

let string s = `String s
let int i = `Int i
let bool b = `Bool b
let null = `Null
let list xs = `List xs
let assoc l = `Assoc l

let ctor ~label l = `Assoc (("type", `String label) :: l)
