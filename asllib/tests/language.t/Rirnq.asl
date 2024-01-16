// RUN: not interp %s | FileCheck %s
// CHECK: runtime_exception

func main() => integer
begin
    assert(FALSE);
    return 0;
end
