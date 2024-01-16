// RUN: interp %s | FileCheck %s

type a of enumeration {A, B};
type b of array [a] of integer;

func main() => integer
begin
    return 0;
end
