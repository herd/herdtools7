//R_SCHV: When determining types within a subprogram declaration, only the
//identifiers mapped by the current type environment shall be used.

// RUN: not interp %s | FileCheck %s

type a of integer;

func b()
begin
    var aa : ty;
end

func main() => integer
begin
    return 0;
end
