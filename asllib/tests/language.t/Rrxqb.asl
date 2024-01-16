// RUN: interp %s | FileCheck %s

func main() => integer
begin
    case TRUE of
        when FALSE => print("FALSE");
        otherwise => print("Otherwise");
    end
    return 0;
end
