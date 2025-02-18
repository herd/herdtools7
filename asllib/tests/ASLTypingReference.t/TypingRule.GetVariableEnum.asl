type Key of enumeration {One, Two, Three};
type SubKey subtypes Key;

func main() => integer
begin            // The right-hand-side expression is | Reason:
    var x = 5;   // Not an enumeration variable       | not a variable expression
    var - = x;   // Not an enumeration variable       | x is integer-typed
    var - = One; // An enumeration variable           | the underlying type is Key
    return 0;
end;
