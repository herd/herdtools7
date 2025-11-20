type NamedTypeA of integer {8,16};
type NamedTypeB of integer {8,16};

func negative15(x: integer, w1: NamedTypeA, w2: NamedTypeA)
begin
    // The RHS width expression does not result in a constrained integer, so
    // even though the width is guaranteed to be 8, this is illegal.
    let testA     = 0xA55A1234[x+7:x]; 
end;

