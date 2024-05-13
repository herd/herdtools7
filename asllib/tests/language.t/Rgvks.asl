//R_GVKS: An expressionless throw statement causes the exception which the
//currently executing catcher caught to be thrown.

// RUN: interp %s | FileCheck %s
// CHECK: a

type a of exception{};

func main() => integer
begin
    try
        try
            throw a{};
        catch
            when a => throw;
        end
    catch
        when a => print("a");
    end
    return 0;
end
