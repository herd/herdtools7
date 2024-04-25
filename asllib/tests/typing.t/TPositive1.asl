////////////////////////////////////////////////////////////////////////////////
// Generating constrained integers
////////////////////////////////////////////////////////////////////////////////
func positive1(myBits : bits(4))
begin
    let testA : integer {42}    = 42;           // Integer literals are constrained integers with the domain containing just the literal value.
                                                // so "42" has type "integer {42}"
    let testB : integer {0..15} = UInt(myBits); // UInt() produces a constrained integer based on the number of bits. In this case 4 bits producing
                                                // an integer in the range 0 to 15
    let testC : integer {-8..7} = SInt(myBits); // Similarly for SInt(). NOTE: LRM is contradictory for SInt(). Rrxyn says it doesn't return an
                                                // constrained integer, but section "9.1 Standard integer functions and procedures" says it does.

    // If the type of a variable isn't defined, its inferred from the RHS, including the domain/constraints, so temp has type integer {-8..7}
    // and the assignment to testD is legal
    let temp                    = testC;
    let testD : integer {-8..7} = temp;
end
