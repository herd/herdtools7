// RUN: interp %s | FileCheck %s

type a of exception {a: integer};

func main() => integer
begin
    return 0;
end
