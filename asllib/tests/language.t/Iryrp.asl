// RUN: interp %s | FileCheck %s

func main() => integer
begin
    var intWid: integer {32,64} = 32;
    return 0;
end
