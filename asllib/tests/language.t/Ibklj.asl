//I_BKLJ: By convention a return value of zero indicates success and a
//return value of one indicates failure.

// RUN: interp %s | FileCheck %s

func main() => integer
begin
    return 0;
end
