//R_KCMK: It is an error to provide multiple definitions with the same
//argument types for the same function or procedure, even if those
//definitions are identical.

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
