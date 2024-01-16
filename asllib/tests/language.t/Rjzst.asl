// RUN: interp %s | FileCheck %s
// CHECK: aa
// CHECK-NEXT: bb

type a of exception{};


func main() => integer
begin
    try
        throw a{};
    catch
        when a => print("aa");
    end
    print("bb");
    return 0;
end
