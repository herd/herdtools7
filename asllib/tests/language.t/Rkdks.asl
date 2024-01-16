// RUN: interp %s | FileCheck %s

type id of integer;

var a: id;


func main() => integer
begin
    return 0;
end
