// Function 'Add'
func Add{N}(x: bits(N), y: bits(N)) => bits(N)
begin
    return x + y;
end;

var Counter: integer = 0;

// Procedure 'IncrementCounter'
func IncrementCounter(inc: integer)
begin
    Counter = Counter + inc;
    return;
end;
