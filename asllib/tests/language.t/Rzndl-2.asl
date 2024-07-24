//R_ZNDL: The IN operator is equivalent to testing its first operand for 
//equality against each value in the (possibly infinite) set denoted by the
//second operand, and taking the logical OR of the result. Values denoted by //a set of patterns comprise the union of the set of values denoted by each
//pattern. Values denoted by a bitmask_lit comprise all bitvectors that
//could match the bit-mask. It is not an error if any or all of the values 
//denoted by the first operand can be statically determined to never compare
//equal with the second operand.

// RUN: interp %s | FileCheck %s

func main() => integer
begin
    assert '111' IN {'1xx'};
    assert '111' IN '1xx';
    assert !('111' IN {'0xx'});
    assert 3 IN {2,3,4};
    assert !(1 IN {2,3,4});
    assert 3 IN {1..10};
    assert 3 IN {<= 10};
    assert 3 IN !{1,2,4};
    assert (1,'10') IN {(1,'1x')};
    assert !((1,'10') IN {(1,'0x'), (2, '1x')});

    return 0;
end
