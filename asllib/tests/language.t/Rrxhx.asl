//R_RXHX: The func keyword declares a subprogram. If a return type is
//included it is a function declaration, otherwise it is a procedure
//declaration.

// RUN: interp %s | FileCheck %s

func function() => integer
begin
    return 10;
end

func procedure()
begin
    pass;
end

func main() => integer
begin
    return 0;
end
