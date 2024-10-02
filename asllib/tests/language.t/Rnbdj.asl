//R_NBDJ: All condition expressions in conditional statements must have the
//structure of boolean.

// RUN: not interp %s | FileCheck %s

func main() => integer
begin
    if 10 then
        pass;
    end
    return 0;
end
