//R_DFWZ: It is not an error for execution of a procedure or setter to end
//without a return statement.

// RUN: interp %s | FileCheck %s

var t: integer;

func a()
begin
    let b = 10;
end

setter c[] = value: integer
begin
    t = value;
end

func main() => integer
begin
    return 0;
end
