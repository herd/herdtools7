//R_WMFV: The type of a storage element is the same as the type of the
//identifier which denotes the storage element.

// RUN: interp %s | FileCheck %s

func main() => integer
begin
    var a: integer = 10;
    var b: integer = a;
    return 0;
end
