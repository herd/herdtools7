type NamedTypeA of integer {8,16};
type NamedTypeB of integer {8,16};

func negative15(x: integer, w1: NamedTypeA, w2: NamedTypeA)
begin
    let testB     = 0xA55A1234[0 +: x]; // illegal, bit width isn't a constrained integer
end

