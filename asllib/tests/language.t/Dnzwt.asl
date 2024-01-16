// RUN: interp %s | FileCheck %s

type a of integer;
type b of real;
type c of string;
type d of boolean;
type e of bits(10);
type f of bit;
type g of enumeration{A, B};

func main() => integer
begin
    return 0;
end
