type BAD_OPCODE of exception;
type UNDEFINED_OPCODE of exception {reason: string, opcode: bits(16)};
type ExceptionWithEmptyFieldList of exception {-};

func test()
begin
    throw UNDEFINED_OPCODE{reason="Undefined", opcode='0111011101110111'};
end;

func main() => integer
begin
    - = ExceptionWithEmptyFieldList {-};
    - = BAD_OPCODE {-};
    return 0;
end;
