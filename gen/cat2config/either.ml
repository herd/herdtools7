type ('a, 'b) t = Left of 'a | Right of 'b

let find_left = function Left x -> Some x | Right _ -> None
let find_right = function Left _ -> None | Right x -> Some x
