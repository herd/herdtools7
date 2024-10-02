//D_QMYP: The type of a subprogram is its signature and consists of its
//return type (if any) and the types of its formal arguments.

// RUN: interp %s | FileCheck %s

// ! Nothing to test

func main() => integer
begin
    return 0;
end
