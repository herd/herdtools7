type Not_found of exception;
type SyntaxException of exception { message:string };

func main () => integer
begin
  if ARBITRARY : boolean then
    throw Not_found {-};
  else
    throw SyntaxException { message="syntax" };
  end;

  return 0;
end;
