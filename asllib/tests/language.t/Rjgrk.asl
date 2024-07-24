//R_JGRK: The reserved_id grammar rule defines identifiers which are
//currently not permitted in ASL.

//Definition of an identifier:
//  <identifier> ::= ( letter | '_' ) ( letter | '_' | digit )*
//Reserved identifiers:
//  <reserved_id> ::= "AND" | "DIV" | "DIVRM" | "EOR" 
//                 | "IN" | "MOD" | "NOT" | "OR"
//                 | "SAMPLE" | "UNKNOWN" | "UNSTABLE" | "XOR"
//                 | "_" | "access" | "advice" | "after"
//                 | "any" | "array" | "as" | "aspect" 
//                 | "assert" | "assume" | "assumes" | "before"
//                 | "begin" | "bit" | "bits" | "boolean" 
//                 | "call" | "case" | "cast" | "catch"
//                 | "class" | "config" | "constant" | "dict" 
//                 | "do" | "downto" | "else" | "elsif"
//                 | "end" | "endcase" | "endcatch" | "endclass" 
//                 | "endevent" | "endfor" | "endfunc" | "endgetter"
//                 | "endif" | "endmodule" | "endnamespace" | "endpackage" 
//                 | "endproperty" | "endrule" | "endsetter" | "endtemplate"
//                 | "endtry" | "endwhile" | "entry" | "enumeration" 
//                 | "event" | "exception" | "export" | "expression"
//                 | "extends" | "extern" | "feature" | "for"
//                 | "func" | "get" | "getter" | "gives"
//                 | "if" | "iff" | "implies" | "import"
//                 | "in" | "integer" | "intersect" | "intrinsic"
//                 | "invariant" | "is" | "let" | "list"
//                 | "map" | "module" | "namespace" | "newevent"
//                 | "newmap" | "of" | "original" | "otherwise" 
//                 | "package" | "parallel" | "pass" | "pattern"
//                 | "pointcut" | "port" | "pragma" | "private" 
//                 | "profile" | "property" | "protected" | "public"
//                 | "real" | "record" | "repeat" | "replace" 
//                 | "requires" | "rethrow" | "return" | "rule"
//                 | "set" | "setter" | "shared" | "signal"
//                 | "statements" | "string" | "subtypes" | "template"
//                 | "then" | "throw" | "to" | "try"
//                 | "type" | "typeof" | "union" | "until"
//                 | "using" | "var" | "watch" | "when" 
//                 | "where" | "while" | "with" | "ztype"

// RUN: not interp %s | FileCheck %s

func main() => integer
begin
    var _: integer;
    return 0;
end
