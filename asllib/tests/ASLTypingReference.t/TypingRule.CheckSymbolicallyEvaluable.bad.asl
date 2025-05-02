type MyException of exception;

func symbolic_throwing{N}(x: integer{N}) => integer{N}
begin
    throw MyException{-};
    return x;
end;

type Data of bits(128) {
    [symbolic_throwing{4}(4)] data
};
