func no_parameterized_base_value{N}(x: bits(N))
begin
    // Illegal since 'N' is a parameterized integer.
    var constrained_bits_base: bits(N);

    // Legal,
    var constrained_bits_init: bits(N) = Zeros{N};
end;
