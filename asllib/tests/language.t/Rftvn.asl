//R_FTVN: The condition expression in a while or repeat statement must have
//the structure of boolean.

// RUN: not interp %s | FileCheck %s

func main() => integer
begin
    repeat
        pass;
    until 10;

    return 0;
end
