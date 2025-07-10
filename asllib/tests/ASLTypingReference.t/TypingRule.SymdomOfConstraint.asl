func symdom{N}(bv: bits(N))
begin
    var x: integer{1..6, 5..N, 9, N*2} = 1;
    var y: integer{2..4, 5..N, N+N} = 2;
    x = y;
end;
