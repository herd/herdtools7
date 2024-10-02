// RUN: interp %s | FileCheck %s
// RCGWR A tuple type must contain at least 2 elements.

// This is expected to be a valid ASL specification. All tuples have at least 2 elements.
// see Rcgwr-2.asl for a negative test with tuples with 0 and 1 elements.

type a_t of (integer, boolean);
type b_t of (integer, boolean, bits(32));


func main() => b_t
begin
    var a : a_t = (42, TRUE);
    var b : b_t = (0, FALSE, Zeros(32));

    return b;
end
