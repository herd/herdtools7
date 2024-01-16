// RUN: not interp %s | FileCheck %s

func main() => integer
begin
    repeat
        pass;
    until 10;

    return 0;
end
