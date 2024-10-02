//R_GYJZ: A checked type conversion where the target type is a bitvector of
//determined width has the same width as the target type.

// RUN: interp %s | FileCheck %s

func main() => integer
begin
    var a: bits(10);
    var b = a as bits({0..10});

    return 0;
end
