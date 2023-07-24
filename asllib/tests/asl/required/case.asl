func inv(i: integer) => integer
begin
  case i of
    when 0 => return 1;
    when 1 => return 0;
  end
end

func main() => integer
begin
  assert 1 == inv(0);
  assert 0 == inv(1);

  return 0;
end

// RUN: archex.sh --eval=':set asl=1.0' --eval=':set -syntax:case_implies' --eval=':load %s' --eval='assert main() == 0;' | FileCheck %s

