type MyException of exception { a: integer, b: boolean };
type ExceptionWithEmptyFieldList of exception;
type ExceptionWithoutFields of exception;

func main() => integer
begin
    var - = MyException {a = 3, b = TRUE};
    var - = ExceptionWithEmptyFieldList {};
    var - = ExceptionWithoutFields {};
    return 0;
end;
