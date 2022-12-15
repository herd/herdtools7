func main(t::integer, n::integer)
  address = read_register(n);
  data = read_memory(address);
  write_register(t, data)
end
