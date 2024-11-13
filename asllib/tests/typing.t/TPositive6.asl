////////////////////////////////////////////////////////////////////////////////
// comparisons
////////////////////////////////////////////////////////////////////////////////
func foo() => integer {8, 16}
begin
  return UNKNOWN : integer {8, 16};
end;

func positive6(size : integer {0..3})
begin
    let testA = size == 8; // legal even though there's no overlap between domains integer {0..3} and integer {8}
    let testB = size >  8;
    let testC = size >= 8;
    let testD = size <  8;
    let testE = size <= 8;
    let testF = size IN {8,16};

    // Same principle applies to the when clauses in a case statement, although the case statement must still be exhaustive or contain an otherwise clause.
    let testG : integer {8,16} = foo();
    case testG of
        when 8 =>
            pass;
            // <some code>
        when 16 =>
            pass;
            // <some code>
        when 32 => // Unreachable but legal code
            pass;
            // <some code>
    end;
end;
