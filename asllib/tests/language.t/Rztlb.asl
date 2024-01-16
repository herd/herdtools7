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
