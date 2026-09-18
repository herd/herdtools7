type counter of integer;

func foo{N}(bv: bits(N)) => integer
begin
    var x : counter = 5;
    return (-N) + (-x);
end;
