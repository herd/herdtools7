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