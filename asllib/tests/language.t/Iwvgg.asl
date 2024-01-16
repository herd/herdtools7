// RUN: interp %s | FileCheck %s

config a = 10;

type b of integer;
type c of bits(a);

func main() => integer
begin
    return 0;
end
