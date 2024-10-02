//R_ZDKC: The type of a mutable storage element shall not be an
//under-constrained integer.

//Note: In order to maintain simple rules for a compiler to determine the
//range of values which an under-constrained integer may hold, we forbid
//mutable under-constrained integers since these would require the compiler
//to interpret every assignment to that storage element.


// RUN: not interp %s | FileCheck %s

var x: integer{} = 10;

func main() => integer
begin
    return 0;
end
