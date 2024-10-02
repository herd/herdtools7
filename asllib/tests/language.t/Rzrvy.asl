//R_ZRVY: A string value is a string of zero or more characters, where a
//character is a printable ASCII character, tab (ASCII code 0x09) or newline
//(ASCII code 0x0A). String values are created by string literals.
//String literals consist of printable characters surrounded by
//double-quotes. Actual tabs and newlines are not permitted in string
//literals, meaning that string literals cannot span multiple source lines.
//The backslash character, ‘\’, is treated as an escape character.
//Definition of a string:
//<string_lit> ::= '"' ((char - ["\"\\"]) | ('\\' ["nt\"\\"] ) )* '"'

// RUN: interp %s | FileCheck %s
// CHECK: hello
// CHECK-NEXT: wor"ld
// CHECK-NEXT: te\st
// CHECK-NEXT: bre
// CHECK-NEXT: ak

func main() => integer
begin
    print("hello");
    print("wor\"ld");
    print("te\\st");
    print("bre\nak");
    return 0;
end
