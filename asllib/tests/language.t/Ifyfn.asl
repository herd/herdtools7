//I_FYFN: A side-effect-free subprogram may contain assert statements, calls
//to the Unreachable() subprogram and calls to the print subprogram.

// RUN: interp %s | FileCheck %s

func a()
begin
    assert(FALSE);
end

func b()
begin
    print("Side effect");
end

func main() => integer
begin
    return 0;
end
