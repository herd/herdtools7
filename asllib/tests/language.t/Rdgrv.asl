//R_DGRV: Execution of a try statement proceeds by executing its stmt_list.
//If execution reaches the end of the stmt_list then execution proceeds to
//the statement following the try statement.


// RUN: interp %s | FileCheck %s
// CHECK: Hello
// CHECK-NEXT: World
func main() => integer
begin
    try
        print("Hello");
    catch
        when integer => pass;
    end
    print("World");
    return 0;
end
