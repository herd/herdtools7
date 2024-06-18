// RUN: interp %s | FileCheck %s

var nonexecutiontime : integer = 10 + 10;
var executiontime : integer = test() + 1;

func test() => integer
begin
    nonexecutiontime = nonexecutiontime + 1;
    return nonexecutiontime;
end

func main() => integer
begin
    return 0;
end
