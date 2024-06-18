//R_JQYF: The expression in an assert statement must have the structure of
//boolean.

// RUN: not interp %s | FileCheck %s

func main() => integer
begin
    assert(10);
    return 0;
end
