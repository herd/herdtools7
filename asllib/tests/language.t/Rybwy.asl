// RUN: not interp %s | FileCheck %s

type base of record {
    one: other
};

type other of base;

func main() => integer
begin
    return 0;
end
