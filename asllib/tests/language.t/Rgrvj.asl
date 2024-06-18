//R_GRVJ: Anonymous enumeration, record and exception type declarations are
//not permitted except in named type declarations.

// RUN: not interp %s | FileCheck %s

func main() => integer
begin
    var a: enumeration {A, B};

    return 0;
end
