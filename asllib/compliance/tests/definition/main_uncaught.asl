type MyException of exception{-};

func main() => integer
begin
    throw MyException{-};
    return 0;
end;
