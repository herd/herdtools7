//R_DVVQ: If a getter function does not define any arguments but contains an
//empty argument list in the declaration, the getter must be invoked with an
//empty set of square brackets.

// RUN: interp %s | FileCheck %s
// CHECK: 30
// CHECK-NEXT: 10

getter a[value1: integer, value2: integer] => integer
begin
    return value1 + value2;
end

getter b[] => integer
begin
    return 10;
end

func main() => integer
begin
    var c = a[10, 20];
    print(c);

    print(b[]);
    return 0;
end
