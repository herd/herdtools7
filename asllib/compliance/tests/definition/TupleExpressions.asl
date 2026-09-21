// declaration of a tuple type
type MyTupleT of (boolean, integer);

var data: integer;

// a function that returns a tuple
func calcEnable() => MyTupleT
begin
    return (TRUE, data); // returning a 2-tuple
end;

func example1() => (integer, integer)
begin
    // declaration let identifier (of type tuple), with initialization
    let default_range: (integer, integer) = (0, 31);
    // declaration of two var identifiers
    // The type information for a and b is taken from the initialization expression
    var (a,b) = default_range;
    // declare a local identifier using a tuple type
    var enable_value: MyTupleT;
    // assignment to a tuple
    enable_value = calcEnable();
    var hi, lo: integer;
    // multiple assignment to the list of elements `lo, hi` from
    // a conditional expression that has one tuple literal `(0,63)`
    // and a variable default_range, of type tuple.
    // Note that the expression on the left side of the equals sign is
    // not a tuple but instead a list of elements.
    (lo, hi) = if ARBITRARY: boolean then (0, 63) else default_range;
    return (lo, enable_value.item1);
end;
