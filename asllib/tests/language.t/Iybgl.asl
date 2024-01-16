// RUN: interp %s | FileCheck %s

config wid = 10;

type b of bits(wid);

func main() => integer
begin
    return 0;
end
