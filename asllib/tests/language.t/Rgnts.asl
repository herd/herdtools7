//R_GNTS: It is illegal for a storage element whose type has the structure
//of the under-constrained integer to be assigned a value whose type has the
//structure of the under-constrained integer.

// RUN: not interp %s | FileCheck %s

func a(N: integer{}, M: integer{})
begin
    var b = N;
    b = M;
end

func main() => integer
begin
    return 0;
end
