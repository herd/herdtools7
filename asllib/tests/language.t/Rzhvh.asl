//R_ZHVH: The number of elements in the tuple on the right-hand side of the
//assignment must be equal to the number of items (name or dash) in the
//parenthesized list on the left-hand side.

// RUN: not interp %s | FileCheck %s

func main() => integer
begin
    var (a, b) = (1, 2, 3);

    return 0;
end
