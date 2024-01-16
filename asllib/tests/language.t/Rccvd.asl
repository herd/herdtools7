// RUN: interp %s | FileCheck %s

var global: integer {4,8};

func foo {parm: integer} (
    arg0: bits(global),
    arg1: bits(parm+global),
    arg2: (integer, bits(parm))
)
begin
    // The type of the second part of the tuple `arg2` is
    // parameter-defining for `parm`, without which the
    // declaration would be illegal.
    // None of the other formals are parameter-defining.
    return;
end

func main() => integer
begin
    return 0;
end
