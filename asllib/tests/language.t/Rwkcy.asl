//R_WKCY: The base value of the string type is the empty string.

// RUN: interp %s | FileCheck %s
// CHECK:

func main() => integer
begin
    var a: string;
    print(a);
    return 0;
end
