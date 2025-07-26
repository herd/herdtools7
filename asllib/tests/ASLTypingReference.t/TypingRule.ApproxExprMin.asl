func approx_min{A}(d: integer{A})
begin
    var x: integer{A..10} = ARBITRARY: integer{A..10};
    var y: integer{0, (A DIV 2)..5} = x DIV 2;

    let a: integer{12..15} = ARBITRARY: integer{12..15};
    let b: integer{a..20, 1} = ARBITRARY: integer{a..20};
end;
