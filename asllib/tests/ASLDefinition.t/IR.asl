func outer_product{M,N}(u: bits(M * 8), v: bits(N * 8)) => bits(M * N * 8)
begin
    var result: bits(8 * N * M);
    for i = 0 to M - 1 do
        for j = 0 to N - 1 do
            let offset = (i * N + j) * 8;
            let entry = UInt(u[i *: 8]) * UInt(v[j *: 8]);
            result[offset +:8] = entry[7:0];
        end;
    end;
    return result;
end;
