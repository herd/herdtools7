//I_TSXL: An expression to access an individual element of a tuple is a 
//tuple expression followed by a dot (.) and then a specific identifier.
//This identifier must be the combination of the exact string item followed
//by an integer. The integer must be the digit 0, or a positive number (with
//no leading zeros).

// RUN: interp %s | FileCheck %s
// CHECK: 10
// CHECK-NEXT: 20

var a = (10, 20);

func main() => integer
begin
    print(a.item0);
    print(a.item1);

    return 0;
end

// XFAIL: *
