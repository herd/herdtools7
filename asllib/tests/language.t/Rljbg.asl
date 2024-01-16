// RUN: interp %s | FileCheck %s

type a of bits(-:integer{0..10});

func main() => integer
begin
    return 0;
end
