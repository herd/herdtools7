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
