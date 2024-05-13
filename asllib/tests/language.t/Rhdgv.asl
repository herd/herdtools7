//R_HDGV: It is an error to declare a subprogram formal argument or
//parameter with the same name as a global variable.

// RUN: not interp %s | FileCheck %s

var a: integer;

func a()
begin
    pass;
end

func main() => integer
begin
    return 0;
end
