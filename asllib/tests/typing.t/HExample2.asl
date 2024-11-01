
type ExtractType of enumeration {PLUS, COLON, ELEM, FUNC, UNKNOWN_OP, EXTEND};
func Example(size : integer, op_type : ExtractType) => bits(8*size)
begin
    var register : bits(8*size) = Ones(8*size);
    case op_type of
        when PLUS =>
            return register[0 +: 8*size];

        when COLON =>
            return register[8*size-1 : 0];

        when ELEM =>
            return register[0 *: 8*size];

        when FUNC =>
            return register_read(size);

        when UNKNOWN_OP =>
            return UNKNOWN: bits(8*size);

        when EXTEND =>
            if size == 32 then
                register[31:0] = Ones(32);
            else
                register = ZeroExtend('0', 8*size);
            end;
            return register;
    end;
end;

func register_read(size : integer) => bits(8*size)
begin
    return Ones(8*size);
end;
