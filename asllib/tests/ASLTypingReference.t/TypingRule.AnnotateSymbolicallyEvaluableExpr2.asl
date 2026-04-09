func write_chunks(message: bits(64), code: bits(8)) => bits(64)
begin
        var chunk_index : integer{0..8}  = 0;
        var new_message: bits(64);

        // Read until all bytes are read or until a fault is encountered.
        while (TRUE) looplimit 5 do
            // chunk_index is mutable, so not symbolically evaluable.
            // However, it does not affect the length of the written slice
            // (8 * chunk_index - 8 * chunk_index + 7 + 1 = 8)
            // which is the constant 8, which is symbolically evaluable,
            // so we consider the slice to be symbolically evaluable.
            new_message[8 * chunk_index + 7 : 8 * chunk_index] = code;
            chunk_index = chunk_index + 1 as integer{0..8};
        end;
        return new_message;
end;
