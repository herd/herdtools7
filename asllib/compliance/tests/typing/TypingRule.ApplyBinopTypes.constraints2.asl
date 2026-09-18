func f(x: integer{2, 4}, y: integer{-1..1})
begin
    // typing `x DIV y` involves removing 0 and 1
    // from {-1..1}, leaving only 1.
    // `z` is typed as `integer{2, 4}`.
    let z = x DIV y;
end;
