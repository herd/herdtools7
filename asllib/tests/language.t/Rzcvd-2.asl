//R_ZCVD: It is illegal for a storage element whose type has the structure 
//of the under-constrained integer to be initialized with a value whose type //has the structure of the under-constrained integer, unless the type is
//omitted from the declaration (and therefore the type can be unambiguously
//inferred) or the initialization expression is omitted (and therefore the
//type is not omitted from the declaration).

// RUN: interp %s | FileCheck %s

func a(N: integer, M: integer)
begin
    var b = N;
    var c: integer;
end

func main() => integer
begin
    return 0;
end
