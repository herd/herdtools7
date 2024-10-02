//R_BRCJ: An expressionless throw statement may only be used in the
//stmt_list of a catcher.

// RUN: not interp %s | FileCheck %s

func main() => integer
begin
    throw;
    return 0;
end
