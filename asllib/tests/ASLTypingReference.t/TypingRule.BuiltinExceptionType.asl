type Not_found of exception;
type Error of exception { message:string };

func main () => integer
begin
  if UNKNOWN : boolean then
    throw Not_found {};
  else
    throw Error { message="syntax" };
  end

  return 0;
end
