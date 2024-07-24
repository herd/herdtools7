//I_GKLW: Multiple subprograms may be defined with the same name provided
//that each definition has a different number or type of arguments to allow
//disambiguation.

// RUN: interp %s | FileCheck %s

func foo(a: integer, b: boolean) => integer
begin
    return 0;
end

func foo(a: integer, b: integer) => integer
begin
    return 0;
end

func foo() => integer
begin
  return 0;
end
