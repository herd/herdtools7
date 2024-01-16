// RUN: interp %s | FileCheck %s


// declaration of a tuple type and then a var identifier using that type
type MyTupleT of (boolean, integer);
var enable_value : MyTupleT;

// declaration let identifier (of type tuple), with initialization
let default_range : (integer, integer) = (0, 31);

// declaration of two var identifiers
// The type information for a and b is taken from the initialization expression
var (a, b) = default_range;

// a function that returns a tuple
func calcEnable() => myTupleT
begin
    return (TRUE, data); // returning a 2-tuple
end

func main() => integer
begin
    // assignment to a tuple
    enable_value = calcEnable();

    // multiple assignment to the list of elements `lo, hi` from
    // a conditional expression that has one tuple literal `(0,63)`
    // and a variable default_range, of type tuple.
    // Note that the expression on the left side of the equals sign is
    // not a tuple but instead a list of elements.
    (lo, hi) = if sf then (0, 63) else default_range;

    return 0;
end
