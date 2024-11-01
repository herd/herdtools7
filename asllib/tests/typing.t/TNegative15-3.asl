type NamedTypeA of integer {8,16};
type NamedTypeB of integer {8,16};

func negative15(x: integer, w1: NamedTypeA, w2: NamedTypeA)
begin
    var testD     = Zeros(32);
    testD[0 *: x] = Zeros(x); // Same rules apply to bit slices on LHS
end;

