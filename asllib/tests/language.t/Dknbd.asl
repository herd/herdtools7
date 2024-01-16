// RUN: interp %s | FileCheck %s

type a of (integer, integer);
type b of array[10] of integer;
type c of record {aa: integer, bb: integer};
type d of exception {aa: integer, bb: integer};

func main() => integer
begin
    return 0;
end
