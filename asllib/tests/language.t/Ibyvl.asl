//I_BYVL: Types describe the allowed values of variables, constants, 
//function arguments, etc.

// RUN: interp %s | FileCheck %s

var x: integer;
constant a: integer = 10;

func test(t: integer)
begin
  pass;
end

func main() => integer
begin
    return 0;
end
