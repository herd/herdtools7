// RUN: interp %s | FileCheck %s

constant a = Ones(10);

func main() => integer
begin
    return 0;
end
