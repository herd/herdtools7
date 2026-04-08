func bitvector_sizes()
begin
    var size = ARBITRARY : integer;
    let immutable_size = size;
    // immutable_size is symbolically evaluable.
    // Illegal: immutable_size is not constrained.
    var bv: bits(immutable_size);
end;
