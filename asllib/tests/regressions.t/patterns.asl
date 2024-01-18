func example_5_3 ()
begin
  // let expr_A = '111' IN {'1xx'};                    //  TRUE
  // assert expr_A;
  // let expr_B = '111' IN {'0xx'};                    //  FALSE
  // assert !expr_B;
  let expr_C = 3 IN {2,3,4};                        //  TRUE
  assert expr_C;
  let expr_D = 1 IN {2,3,4};                        //  FALSE
  assert !expr_D;
  let expr_E = 3 IN {1..10};                        //  TRUE
  assert expr_E;
  let expr_F = 3 IN {<= 10};                        //  TRUE
  assert expr_F;
  let expr_G = 3 IN !{1,2,4};                       //  TRUE
  assert expr_G;

end

func main () => integer
begin
  example_5_3 ();

  return 0;
end

// RUN: archex.sh --eval=':set asl=1.0' --eval=':set +syntax:aslv1_colon_colon' --eval=':load %s' --eval='assert main() == 0;' | FileCheck %s

