//R_LXQZ: A storage element of type S, where S is any type that does not
//have the structure of the under-constrained integer type, may only be
//assigned or initialized with a value of type T if T type-satisfies S

// RUN: not interp %s | FileCheck %s

func main() => integer
begin
    var a: integer = 0.0;
    return 0;
end
