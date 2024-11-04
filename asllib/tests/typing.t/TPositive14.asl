type NamedTypeA of integer {8,16};
type NamedTypeB of integer {8,16};

func positive14(w1 : NamedTypeA)
begin
    let tempA : NamedTypeB     = w1 as NamedTypeB; // NOTE: ATC's between different named types will be permitted by ASL-505
    let testB : bits(w1)       = Zeros{tempA};     // legal, RHS and LHS are both of type bits(w1). The fact that w1 and tempA are different
                                                   // named types doesn't matter, its the value they have, not their types that matters.
    // Assignment to/from named type and primitive type are legal without ATC's
    let testC : integer {8,16} = w1;
    let testD : NamedTypeB     = testC;
    let tempE : NamedTypeB     = w1 as integer {8,16}; // Combined version of testC/D. Using ATC to erase named type.
end;
