typedef A @prose_description "A";

relation transform(a: A) -> (b: A)
  @prose_description "transforms {a} to {b}"
  @prose_application "";

relation transform_description_unmatched_b(a: A) -> A
  @prose_description "transforms {a} to {b}"
  @prose_application "";
