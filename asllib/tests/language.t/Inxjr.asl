// RUN: interp %s | FileCheck %s

var counter: integer = 0;

func nonexecution() => integer
begin
    return 10;
end

func execution() => integer
begin
    counter = counter + 1;
    return counter;
end


func main() => integer
begin
    return 0;
end
