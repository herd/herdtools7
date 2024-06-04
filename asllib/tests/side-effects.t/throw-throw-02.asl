type E of exception {};
type F of exception {};

func ThrowsE() => integer
begin
  throw E {};
end

func ThrowsF() => integer
begin
  throw F {};
end

func main () => integer
begin
  try
    let - = (ThrowsE (), ThrowsF ());
  catch when E => pass; when F => pass; end

  return 0;
end



