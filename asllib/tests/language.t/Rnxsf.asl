//R_NXSF: If a locally declared identifier has an associated type in the
//declaration_stmt, then the identifier has that type.

// RUN: interp %s | FileCheck %s

type a of integer;

func main() => integer
begin
    var b: a;

    return 0;
end
