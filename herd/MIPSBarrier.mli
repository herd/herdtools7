module type S =
  sig
    type a (* Native arch barrier *)
    type b = SYNC (* MIPS Momory barrier *)
    val a_to_b : a -> b
  end
