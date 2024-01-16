// RUN: interp %s | FileCheck %s

func a()
begin
    assert(FALSE);
end

func b()
begin
    print("Side effect");
end

func main() => integer
begin
    return 0;
end
