// RUN: interp %s | FileCheck %s

type named of integer;
var anonymous: integer;

func main() => integer
begin
    return 0;
end
