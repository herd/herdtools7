// RUN: interp %s | FileCheck %s

type a of integer;

var b: integer;
let c = 10;
constant d = 10;
config e : integer = 10;

func f()
begin
    return;
end

getter g[] => integer
begin
    return b;
end

setter h[] = value: integer
begin
    b = value;
    return;
end

func main() => integer
begin
    return 0;
end
