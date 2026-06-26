type WordCount of integer;
type PacketLength of WordCount;

func main() => integer
begin
  var myWordCount: WordCount;
  var myPacketLength: PacketLength;
  // illegal: distinct named types
  myPacketLength = myWordCount;
  return 0;
end;
