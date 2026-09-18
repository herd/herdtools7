type MyInt of integer{0..400};

func foo(x: MyInt) => MyInt
begin
    return if x < 20 then (20 * x) as MyInt else x;
end;
