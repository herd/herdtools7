
func main(t::integer, s::integer, datasize::integer)
    write_register(t, datasize, read_register(s, datasize))
end

