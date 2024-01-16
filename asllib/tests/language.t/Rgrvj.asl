// RUN: not interp %s | FileCheck %s

func main() => integer
begin
    var a: enumeration {A, B};

    return 0;
end
