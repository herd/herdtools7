func write_chunks(message: bits(64), code: bits(8)) => bits(64)
begin
        var chunk_index : integer{0..8}  = 0;
        var new_message: bits(64);

        while (TRUE) looplimit 5 do
            // The slice expression on the left-hand side of the assignment
            // is not syntactically symbolically evaluable because it contains
            // the mutable variable chunk_index, but it is semantically
            // symbolically evaluable because chunk_index does not affect
            // the length of the slice, which is a constant 8
            // (8 * chunk_index - 8 * chunk_index + 7 + 1 = 8).
            new_message[8 * chunk_index + 7 : 8 * chunk_index] = code;
            chunk_index = chunk_index + 1 as integer{0..8};
        end;
        return new_message;
end;
