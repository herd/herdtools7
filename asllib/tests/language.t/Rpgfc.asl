//R_PGFC: It is illegal to declare a subprogram if it clashes with any other
//declared subprogram.

// RUN: not interp %s | FileCheck %s

func a()
begin
    pass;
end

func a()
begin
    pass;
end

func main() => integer
begin
    return 0;
end
