// RUN : interp %s | FileCheck %s

type a of integer;

func main() => integer
begin
    var aa: a;
    var aaa: a = aa;

    return 0;
end
