//R_JZST: If execution reaches the end of a stmt_list corresponding to a
//catcher or an otherwise_opt then execution proceeds to the statement
//following the try statement which contains the catcher or the
//otherwise_opt.

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
