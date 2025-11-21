module type S = sig
  val printv : 'a. int -> ('a, Format.formatter, unit) format -> 'a
  val eprintv : 'a. int -> ('a, Format.formatter, unit) format -> 'a
end
