//R_JFRD: A local identifier declared with var, let or constant is in scope
//from the point immediately after its declaration until the end of the
//immediately enclosing block.

// RUN: not interp %s | FileCheck %s

func main() => integer
begin
    if TRUE then
        var a = 10;
    end

    var b = a;
    return 0;
end
