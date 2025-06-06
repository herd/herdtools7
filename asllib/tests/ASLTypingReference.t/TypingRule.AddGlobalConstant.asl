constant FOUR = 4;
// The static environment binds FOUR to 4.

func foo(v: integer {0..100}) => integer {0..100}
begin
    return v;
end;

constant x = 32;
constant z: integer {0..100} = foo(x);
// The static environment binds z to 32.
constant gbv: bits(32) = Zeros{z};

func main() => integer
begin
    var bv: bits(2^FOUR) = Zeros{FOUR * FOUR};
    return 0;
end;
