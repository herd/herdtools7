// RUN: interp %s | FileCheck %s

var only: bits(-: integer{1,2}) = Zeros(1);

func main() => integer
begin
    return 0;
end
