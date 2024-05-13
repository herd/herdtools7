//R_JBXQ: where a new stmt_list is encountered, a new type environment is
//created which is initialized with the contents of the current type
//environment. This new type environment becomes the current type
//environment. When the end of the stmt_list is encountered, the current
//type environment is discarded and the type environment which was current
//at the start of the stmt_list becomes the current type environment again.

// RUN: not interp %s | FileCheck %s

config cond = TRUE;

func main() => integer
begin
    var a = 10;

    if cond then
        var b = a;
    end

    var c = a + b;

    return 0;
end
