
var state: bits(8) = '01000110';

func my_procedure(mask: bits(8))
begin
    state = state AND mask;
end;

func flip_bits{N}(v: bits(N)) => bits(N)
begin
    return Ones{N} XOR v;
end;

func main() => integer
begin
    my_procedure(flip_bits{8}('11001010'));
    println(state);
    return 0;
end;
