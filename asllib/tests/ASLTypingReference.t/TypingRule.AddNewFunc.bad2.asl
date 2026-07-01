type circle of integer;
type square of integer;
var myCircle: circle;
var mySquare: square;

func g(x: circle, y: integer)
begin
    pass;
end;

func g(x: square, y: real)
begin
    pass;
end;

func g_test()
begin
    g(myCircle, 0.1); // illegal
    // because no declaration of g has
    // a first argument which is type-satisfied by a circle
    // and a second argument which is type-satisfied by a real.
end;
