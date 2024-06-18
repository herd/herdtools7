// RUN: not interp %s 2>&1 | FileCheck %s
// CHECK: ASSERT FAILED

func main() => integer
begin
    assert(FALSE);
    return 0;
end