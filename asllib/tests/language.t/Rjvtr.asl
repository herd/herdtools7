// RUN: not interp %s | FileCheck %s

func main() => integer
begin
    case TRUE of
        when FALSE => print("FALSE");
    end
    return 0;
end
