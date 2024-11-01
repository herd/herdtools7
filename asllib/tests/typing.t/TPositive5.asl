func positive5(size : integer {0..3})
begin
    // The domain of the ATC can be exactly the same, or a subset/superset of the value it applies to
    let testA = size as integer {0..3};
    let testB = size as integer {0..16};
    let testC = size as integer {0..1}; // Legal statically, but may fail at runtime if size is >1

    // The domain of the ATC may be completely disjoint from the value it applies to. This sounds odd but is required by the instruction
    // flow. See ASL-313 for rational
    let testD = size as integer {8,16}; // Must be legal statically, but will fail at runtime if this line is ever reached.
    let testE = 1    as integer {8,16}; // Must be legal statically, but will fail at runtime if this line is ever reached.

    // The domain of the output of the ATC is the domain of the ATC, and this domain is used for type inference
    let temp                   = size as integer {0..1}; // temp has type integer {0..1}
    let testF : integer {0..1} = temp;
end;
