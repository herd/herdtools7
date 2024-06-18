// RUN: not interp %s | FileCheck %s

type enum of enumeration{A, B};

func A()
begin
    pass;
end

func main() => integer
begin
    return 0;
end
