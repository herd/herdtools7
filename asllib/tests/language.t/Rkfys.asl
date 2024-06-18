//R_KFYS: If both operands of an integer binary primitive operation are
//well-constrained integers, then it shall return a constrained integer
//whose constraint is calculated by applying the operation to all possible
//value pairs.

// RUN: interp %s | FileCheck %s

func test(a: integer{1..4}, b: integer{0..3})
begin
    var c: integer{1..7} = a + b;
end

func main() => integer
begin
    return 0;
end
