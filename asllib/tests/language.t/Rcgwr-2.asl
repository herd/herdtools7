//R_CGWR: A tuple type must contain at least 2 elements.

// RUN: not interp %s | FileCheck %s

type a of (integer); // this line should fail, a tuple must have at least 2 elements
type b of ();         // this line should fail, a tuple must have at least 2 elements

func main() => integer
begin
    return 0;
end
