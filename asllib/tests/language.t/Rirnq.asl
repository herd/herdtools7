//R_IRNQ: If the runtime environment raises an exception in response to a
//dynamic error the exception must have the supertype runtime_exception.

// RUN: not interp %s | FileCheck %s
// CHECK: runtime_exception

func main() => integer
begin
    assert(FALSE);
    return 0;
end
