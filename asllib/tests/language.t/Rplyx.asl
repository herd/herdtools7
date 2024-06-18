//R_PLYX: Where the initialization expression in a variable_declaration is a
//bitvector of determined width, if the initialization expression type
//satisfies the declared type, then the declaration creates a storage
//element whose determined width is the determined width of the
//initialization expression.

// RUN: interp %s | FileCheck %s

config width = 10;
var a : bits(width);

func main() => integer
begin
    return 0;
end
