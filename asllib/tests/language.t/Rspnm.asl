//R_SPNM: When the catch of a try statement is executed, then the thrown
//exception is caught by the first catcher in that catch which it
//type-satisfies or the otherwise_opt in that catch if it exists.

// RUN: interp %s | FileCheck %s
// CHECK: b
// CHECK-NEXT: a
// CHECK-NEXT: other

type a of exception{};
type b of exception{};
type c of exception{};

func main() => integer
begin
    try
        throw b{};
    catch
        when a => print("a");
        when b => print("b");
        otherwise => print("other");
    end

    try
        throw a{};
    catch
        when a => print("a");
        when b => print("b");
        otherwise => print("other");
    end

    try
        throw c{};
    catch
        when a => print("a");
        when b => print("b");
        otherwise => print("other");
    end
    return 0;
end
