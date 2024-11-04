// Divisions
func division_example{width}(data_in : bits(width)) => bits(width)
begin
    var iterations = width DIV 8;
    return Zeros{width};
end;

func for_loop_example1{N}(data : bits(N))
begin
    for i = 0 to (N DIV 8) - 1 do
        var byte = data[i*8+7:i*8];
    end;
end;

func for_loop_example2{width}(data_in : bits(width)) => bits(width)
begin
    var data = data_in;

    var iterations = width DIV 8;
    for i=0 to iterations-1 do
        data[63+i*64:64*i] = Ones{64};
    end;
    return data;
end;
