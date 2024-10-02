// RUN: interp %s | FileCheck %s
// CHECK: Hello
// CHECK-NEXT: World
// CHECK-NOT: ERROR
// CHECK-NEXT: DONE


// RDGRV Execution of a try statement proceeds by executing its stmt_list.
//       If execution reaches the end of the stmt_list then execution proceeds
//       to the statement following the try statement.

type ONE of exception;
type TWO of exception;
type THREE of exception;

var num_of_named_catches_caught : integer = 0;


func main() => integer
begin
    try
        print("Hello ");
        throw ONE;
    catch
        when ONE =>
           num_of_named_catches_caught = num_of_named_catches_caught +1;
           pass;
    end

    assert num_of_named_catches_caught == 1;

    try             // wrapper try-catch block


        // now a try/catch block where we expect to see execution to go to stmt following block
        try
            print("World ");
            throw TWO;   // throw something that is not caught in this throw-catch block
        catch
            when THREE  =>   // a catch for something that not expected to be thrown
               num_of_named_catches_caught = num_of_named_catches_caught +1;
               pass;
        end
        print("ERROR: This line should never be printed");
        num_of_named_catches_caught = num_of_named_catches_caught +1;
    catch
        otherwise =>     //
           print("DONE");
    end

    assert num_of_named_catches_caught == 1;

    return 0;
end
