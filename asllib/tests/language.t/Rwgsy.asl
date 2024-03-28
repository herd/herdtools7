// RUN: not interp %s | FileCheck %s

func main() => integer
begin
    case '1' of
        when '1' where 1 => pass;
    end
    return 0;
end
