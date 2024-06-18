// RUN: not interp %s | FileCheck %s

type enum of enumeration{A, B};


func main() => integer
begin
    var a: boolean = A <= B;

    return 0;
end
