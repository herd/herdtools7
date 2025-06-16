func main() => integer
begin
    var b = ARBITRARY: bits(2);
    // This example is not valid ASL
    case b of
        when '10' => // empty statement list is invalid, does not fall through
        when '11' => X[30] = 0;
    end;
    return 0;
end;
