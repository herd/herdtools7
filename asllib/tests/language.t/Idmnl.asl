//I_DMNL: Strings play relatively little role in specifications and the only
//operations on strings are equality and inequality tests.

// RUN: interp %s | FileCheck %s
// CHECK: TRUE
// CHECK-NEXT: TRUE

func main() => integer
begin
    var a: string = "Hello";
    print((a == "Hello"));
    print((a != "World"));
    return 0;
end
