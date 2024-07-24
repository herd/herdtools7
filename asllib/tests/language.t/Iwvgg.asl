// I_WVGG: Types in ASL are either execution-time types or non-execution-time
// types. 

config a = 10;

type b of integer;
type c of bits(a);

func main() => integer
begin
    return 0;
end
