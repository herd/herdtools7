type NamedTypeA of integer {8,16};
type NamedTypeB of integer {8,16};

func negative14(w1 : NamedTypeA, w2: NamedTypeA)
begin
    let tempA : NamedTypeB = w1;        // illegal, not the same type
    // let testB : bits(w1)   = Zeros(w2); // illegal, just because w1 and w2 are the same type doesn't mean they are the same value, so
                                        // type bits(w1) != bits(w2)
end;
