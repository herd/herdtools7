// RUN: interp %s | FileCheck %s
// CHECK: Caught correctly
// CHECK-NEXT: Caught incorrectly

type a of exception{};
type b of exception{};

func main() => integer
begin
    try
        throw a{};
    catch
        when a => print("Caught correctly");
        otherwise => print("Caught incorrectly");
    end


    try
        throw b{};
    catch
        when a => print("Caught correctly");
        otherwise => print("Caught incorrectly");
    end
    return 0;
end
