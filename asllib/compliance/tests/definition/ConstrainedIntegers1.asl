type Ity of integer {2,4,8};

func f()
begin
    var A: integer {2,4,8};
    var B: integer {2,4};
    // A and B have anonymous types
    A = B;
    var I: Ity;
    I = A;
    I = B;
    A = I;
end;

var gInt: integer; // unconstrained global integer

func g()
begin
    var myInt: integer = gInt; // Legal
    var myIntA: integer {1..10} = myInt as integer {1..10};
    // Legal: incurs execution-time check that (myInt IN {1..10})
    var myIntB: integer {0..20} = myIntA;
    // Legal: type satisfaction and no execution-time check required
end;
