// RUN: not interp %s | FileCheck %s

var counter: integer = 0;
constant counter_two: integer = add();

func adder() => integer
begin
    counter = counter + 1;
    return counter;
end

func main() => integer
begin
    return 0;
end
