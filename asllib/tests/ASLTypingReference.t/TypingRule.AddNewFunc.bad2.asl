type shape of integer;
type square of integer;
var myShape: shape;
var mySquare: square;

func g(x: shape, y: integer)
begin
    pass;
end;

func g(x: square, y: real)
begin
    pass;
end;

func g_test()
begin
    g(myShape, 0.1); // illegal
    // because no declaration of g has
    // a first argument which is type-satisfied by a shape
    // and a second argument which is type-satisfied by a real.
end;
