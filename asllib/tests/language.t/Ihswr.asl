//I_HSWR: Where both S and T are bitvectors of determined width, since their
//widths must match, we do not need to compare their domains.

// RUN: not interp %s | FileCheck %s

func testing(a: bits(4))
begin
    pass;
end

func main() => integer
begin
    var a: bits(2);
    testing(a);
    return 0;
end