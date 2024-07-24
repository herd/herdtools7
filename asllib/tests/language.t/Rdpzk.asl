//R_DPZK: The behaviour of the runtime environment when a dynamic error
//occurs is implementation defined.

// RUN: not interp %s | FileCheck %s

func main() => integer
begin
    assert(FALSE);
    return 0;
end
