//R_WGSY: The case guard of a case statement’s when clause must have the
//structure of boolean.

// RUN: not interp %s | FileCheck %s

func main() => integer
begin
    case '1' of
        when '1' where 1 => pass;
    end
    return 0;
end
