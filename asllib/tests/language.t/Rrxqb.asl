//R_RXQB: Testing the discriminant against a pattern list follows the same
//procedure as pattern matching. It is not an error if it can be statically
//determined that none of the patterns in a case alternative can match the
//discriminant.

// RUN: interp %s | FileCheck %s

func main() => integer
begin
    case TRUE of
        when FALSE => print("FALSE");
        otherwise => print("Otherwise");
    end
    return 0;
end
