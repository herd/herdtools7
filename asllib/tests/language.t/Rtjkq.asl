// RUN: not interp %s | FileCheck %s

func test(N: integer)
begin
    let illegal : integer {32, 64} = N;
end

func main() => integer
begin
    return 0;
end
