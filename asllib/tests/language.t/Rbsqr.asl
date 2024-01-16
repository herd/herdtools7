// RUN: not interp %s | FileCheck %s

config a : integer = 10;

func main() => integer
begin
    a = 5;
    return 0;
end
