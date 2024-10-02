//R_WVXS: The identifier given in a when clause of a try..catch statement is
//introduced into the new type environment which is created by R_JBXQ
//when the body of the when clause is encountered. The identifier maps to
//the type it is annotated with in the when clause.

//R_JBXQ: where a new stmt_list is encountered, a new type environment is
//created which is initialized with the contents of the current type
//environment. This new type environment becomes the current type
//environment. When the end of the stmt_list is encountered, the current
//type environment is discarded and the type environment which was current
//at the start of the stmt_list becomes the current type environment again.

// RUN: interp %s | FileCheck %s

type a of exception{
    aa: integer
};

func main() => integer
begin
    try
        pass;
    catch
        when aa: a => var b: integer = aa.aa;
    end
    return 0;
end
