var gInt: integer; // unconstrained global integer

func g()
begin
    var myInt: integer = gInt;
    var myIntA: integer {1..10} = myInt as integer {1..10};
    var myIntB: integer {0..20} = myIntA;
    myIntA = myIntB; // Illegal even though at this point
                     // the value stored in myIntB is always
                     // in 1..10.
end;
