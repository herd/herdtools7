func parameterized_base_value{N}(x: bits(N))
begin
    // Legal: produces 0[:N].
    var constrained_bits_base: bits(N);

    // Legal.
    var constrained_bits_init: bits(N) = Zeros{N};
end;
