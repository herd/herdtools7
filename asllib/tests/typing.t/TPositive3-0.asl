func positive3(size : integer {0..3}, size2 : integer {8,16,32,64})
begin
    // exact operators propagate the constraints. For example "integer {8} << integer {0..3}" has type "integer {8,16,32,64}"
    // let testA : integer {8,16,32,64} = 8 << size;
    let testB : integer {1..4}       = size + 1;
    let testC : integer {0,2,4,6}    = size * 2;
    let testD : integer {1,2,4,8}    = size2 DIV 8;
    let testE : integer {7,15,31,63} = size2 - 1;
end;

