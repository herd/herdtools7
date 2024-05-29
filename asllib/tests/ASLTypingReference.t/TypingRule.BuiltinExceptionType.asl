type Not_found of exception;
type SyntaxException of exception { message:string };

func main () => integer
begin
  if UNKNOWN : boolean then
    throw Not_found {};
  else
    throw SyntaxException { message="syntax" };
  end

  return 0;
end
