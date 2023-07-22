func main () => integer
begin
  assert ('111' IN {'1xx'}) == TRUE;
  assert ('111' IN '1xx') == TRUE;
  assert ('111' IN {'0xx'}) == FALSE;
  assert (3 IN {2,3,4}) == TRUE;
  assert (1 IN {2,3,4}) == FALSE;
  assert (3 IN {1..10}) == TRUE;
  assert (3 IN {<= 10}) == TRUE;
  assert (3 IN !{1,2,4}) == TRUE;
  assert ((1, 2) IN {(1, 2)}) == TRUE;
  assert ((1,'10') IN {(1,'1x')}) == TRUE;
  assert ((1,'10') IN {(1,'0x'), (2, '1x')}) == FALSE;
  // (see note below)

  assert ((1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, '11') IN {(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, '0x')}) == FALSE;

  // The last two expressions are equivalent to:
  let expr_H = (1 IN {1}) && ('10' IN {'1x'});
  let expr_I = ((1 IN {1}) && ('10' IN {'0x'})) ||
  ((1 IN {2}) && ('10' IN {'1x'}));

  return 0;
end

// RUN: archex.sh --eval=':set asl=1.0' --eval=':set +syntax:aslv1_colon_colon' --eval=':load %s' --eval='assert main() == 0;' | FileCheck %s

