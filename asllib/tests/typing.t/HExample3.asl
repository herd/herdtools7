type RuntimeType of enumeration {BIGGER_THAN_8, LESS_THAN_8, EQUALS_8};

func Runtime{size}(error: RuntimeType) => bits(8*size)
begin
    var temp = Zeros{64};
    var result = Zeros{8*size};
    case error of
        when BIGGER_THAN_8 =>
            // Runtime Error when size > 8
            result = temp[0+:8*size];
            temp = ZeroExtend{64,8*size}(result);

        when EQUALS_8 =>
            // Cannot concatenate if size != 8
            result = [temp[63:56], Zeros{8*size - 8}];

        when LESS_THAN_8 =>
            // Runtime Error when size < 8
            result = ZeroExtend{8*size,64}(temp);
            // Selecting values from result that do not exist
            result[95:0] = Zeros{96};
            // Passing a value down to another function type-checks
            // but causes a runtime error
            function{size}(ZeroExtend{8*size,64}(temp));

    end;
    return result;
end;

func function{size}(x : bits(8*size))
begin
    pass;
end;
