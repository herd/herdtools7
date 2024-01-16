// RUN: not interp %s | FileCheck %s

func main() => integer
begin
    let a: String = "©";
    return 0;
end
