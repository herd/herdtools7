// RUN: interp %s | FileCheck %s

func main() => integer
begin
    var a: (boolean, bits(32)) = (TRUE, Zeros(32));

    return 0;
end
