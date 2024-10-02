//R_ZRWH: Each bit within a bitvector has value ‘0’ or ‘1’.
// RUN: interp %s | FileCheck %s

func main() => integer
begin
    var a: bits(16) = '0000 0001 0010 0100';
    return 0;
end
