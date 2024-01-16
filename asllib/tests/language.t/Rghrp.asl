// RUN: not interp %s | FileCheck %s

config a: integer = 10;

type b of bits(a);

func main() => integer
begin
    return 0;
end
