// RUN: interp %s | FileCheck %s

config size: integer{0..10} = 5;

type a of array[size] of integer;

func main() => integer
begin
    return 0;
end
