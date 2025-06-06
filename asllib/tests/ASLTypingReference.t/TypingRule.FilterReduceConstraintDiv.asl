func foo{A, B}(a: integer{A}, b: integer{B})
begin
    var x: integer{2, 3, 5..A, A..10, A..B, 7..10, 14..17, 16..15} = 2;
    var y = 2;
    var z: integer{} = x DIV y;
    var z_typed : integer{
        1,
        3..(A DIV 2),
        (A DIV 2)..5,
        (A DIV 2)..(B DIV 2),
        4..5,
        7..8} = x DIV y;
end;
