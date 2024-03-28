// RUN: interp %s | FileCheck %s

var globe: integer = 10;

func a()
begin
    globe = globe + 10;
end

func b()
begin
    print("Side effect");
end

type expc of exception{};

func c()
begin
    throw expc{};
end

func main() => integer
begin
    return 0;
end
