//R_XHPB: If a locally declared identifier does not have an associated type
//then it must have an associated initialization expression and the
//identifier has the type of that expression.

// RUN: interp %s | FileCheck %s

func main() => integer
begin
    var a = 10;

    return 0;
end
