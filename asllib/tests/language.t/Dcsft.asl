// RUN: interp %s | FileCheck %s

var counter: integer = 10;

func storage() => integer
begin
    return counter;
end

func expression_func()
begin
    counter = counter + 1;
end

func function() => integer
begin
    expression_func();
    return storage();
end

func main() => integer
begin
    return 0;
end
