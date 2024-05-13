//R_ZTLB: If a thrown exception is not caught in a catch then the exception
//is passed to the catch of the closest enclosing try statement in the
//current execution.

// RUN: interp %s | FileCheck %s
// CHECK: a

type a of exception{};
type b of exception{};
type c of exception{};

func main() => integer
begin
    try
        try
            throw a{};
        catch
            when b => print("b");
        end
    catch
        when a => print("a");
        when b => print("b");
        otherwise => print("other");
    end
    return 0;
end
