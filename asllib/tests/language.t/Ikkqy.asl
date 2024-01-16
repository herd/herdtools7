// RUN: interp %s | FileCheck %s

config a = 10;

type b of bits(a);
type c of integer;

func main() => integer
begin
    return 0;
end
