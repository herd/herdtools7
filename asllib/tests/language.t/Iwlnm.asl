// RUN: not interp %s | FileCheck %s

var a = '1xx1';

func main() => integer
begin
    return 0;
end
