//R_NYWH: A return statement appearing in a setter or procedure must have no
//return value expression.

// RUN: not interp %s | FileCheck %s

func prod()
begin
    return 10;
end

func main() => integer
begin
    return 0;
end
