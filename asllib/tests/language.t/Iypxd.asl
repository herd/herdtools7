// RUN: interp %s | FileCheck %s

let (a, b) = (10, 10);

func main() => integer
begin
    return 0;
end
