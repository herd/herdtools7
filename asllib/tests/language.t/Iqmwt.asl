// RUN: interp %s | FileCheck %s

type enum of enumeration{A, B};
type enum2 subtypes enum;

func main() => integer
begin
    return 0;
end
