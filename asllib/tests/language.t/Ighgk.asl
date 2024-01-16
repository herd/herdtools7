// RUN: interp %s | FileCheck %s

func under_constrained(a: integer{})
begin
    pass;
end

func main() => integer
begin
    return 0;
end
