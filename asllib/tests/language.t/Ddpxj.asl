// RUN: interp %s | FileCheck %s

type a of array[10] of integer;

func main() => integer
begin
    return 0;
end
