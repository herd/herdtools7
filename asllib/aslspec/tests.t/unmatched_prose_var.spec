typedef A { "A" };

relation transform(a: A) -> (b: A)
{
    "transforms {a} to {b}",
    prose_application = "",
};

relation transform_description_unmatched_b(a: A) -> A
{
    "transforms {a} to {b}",
    prose_application = "",
};
