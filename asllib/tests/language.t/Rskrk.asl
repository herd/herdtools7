// RUN: interp %s | FileCheck %s
config a: integer{0..10} = 10;
type b of bits(a);

func main() => integer
begin
    return 0;
end
