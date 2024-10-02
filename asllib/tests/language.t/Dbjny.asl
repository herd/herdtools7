//D_BJNY: An assignment statement stores the value of the
//right-hand side expression in one or more storage elements,
//as denoted by the left hand side.

// RUN: interp %s | FileCheck %s

func main() => integer
begin
    var a: integer = 10;
    a = 4;
    return 0;
end
