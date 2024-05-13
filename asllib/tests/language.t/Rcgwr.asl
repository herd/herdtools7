//R_CGWR: A tuple type must contain at least 2 elements.

// RUN: not interp %s | FileCheck %s

type a of (integer);

func main() => integer
begin
    return 0;
end
