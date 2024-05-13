//R_XYLP: Where the initialization expression in a declaration_stmt is a
//bitvector of determined width, if the initialization expression type
//satisfies the declared type, then the declaration creates a storage
//element whose determined width is the determined width of the
//initialization expression.

// RUN: interp %s | FileCheck %s

func main() => integer
begin
    var a: bits({1..10}) = '111';

    return 0;
end
