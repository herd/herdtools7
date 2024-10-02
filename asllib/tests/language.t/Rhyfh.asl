//R_HYFH: Integers are written either in decimal using one or more of the
//characters 0-9 and underscore, or in hexadecimal using 0x at the start
//followed by the characters 0-9, a-f, A-F and underscore. An integer
//literal cannot start with an underscore.

//Definition of an integer: 
//  <int_lit> ::= digit ('_' | digit)*
//Definition of a hexadecimal: 
//  <hex_lit> ::= '0' 'x' (digit | ["abcdefABCDEF"]) ('_' | digit | ["abcdefABCDEF"])*


// RUN: interp %s | FileCheck %s
// CHECK: 10
// CHECK-NEXT: 10
// CHECK-NEXT: 10
// CHECK-NEXT: 10
// CHECK-NEXT: 10
// CHECK-NEXT: 10

func main() => integer
begin
    print(10);
    print(1_0);
    print(0xa);
    print(0xA);
    print(0xA_);
    print(0x0a);

    return 0;
end
