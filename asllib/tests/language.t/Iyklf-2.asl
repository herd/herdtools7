//I_YKLF: The exception thrown by an expressionless throw statement has the
//same thrown type of the original exception which the current catcher
//caught and may not be the same as the caught type of the catcher’s
//exception.

// RUN: interp %s | FileCheck %s
// CHECK: a

type a of exception{};
type b subtypes a;

func main() => integer
begin
    try
        try
            throw b{};
        catch
            when b => throw;
        end
    catch
        when a => print("a");
    end
    return 0;
end

// XFAIL: *
