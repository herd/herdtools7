
func main(t::integer, s::integer, datasize::integer)
begin
    write_register(t, datasize, read_register(s, datasize));
end

