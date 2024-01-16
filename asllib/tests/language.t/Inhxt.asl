// RUN: interp %s | FileCheck %s

config configWid: integer {32, 64} = 32;
config gExtra : integer {0, 8} = 0;

var gReg: bits(configWid);
var gRegF: bits(configWid + gExtra);

func main() => integer
begin
    return 0;
end
