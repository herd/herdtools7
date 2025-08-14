Examples used to test syntax and AST building rules:
  $ aslref expr1.asl
  $ aslref expr2.asl
  $ aslref ASTRule.Desugar_SCase1.asl
  $ aslref ASTRule.Desugar_SCase2.asl
  $ aslref --no-exec Lexical.Comments.asl
  $ aslref GuideRule.Whitespace1.asl
  $ aslref GuideRule.Whitespace2.asl
  $ aslref GuideRule.ReservedIdentifiers.bad.asl
  ASL Lexical error: "__internal_var" is a reserved keyword.
  [1]
  $ aslref GuideRule.IdentifiersKeywords.bad.asl
  File GuideRule.IdentifiersKeywords.bad.asl, line 3, characters 8 to 12:
      var case = 5;
          ^^^^
  ASL Grammar error: Cannot parse.
  [1]
  $ aslref ConventionRule.IdentifiersDifferingByCase.asl
  $ aslref --no-exec ConventionRule.IdentifierSingleUnderscore.asl
  $ aslref --no-exec ASTRule.DesugarElidedParameter.asl
  $ aslref GuideRule.DiscardingLocalStorageDeclarations.asl
  File GuideRule.DiscardingLocalStorageDeclarations.asl, line 4,
    characters 6 to 7:
    let - = 42;
        ^
  ASL Grammar error: Obsolete syntax: Discarded storage declaration.
  [1]
  $ aslref GuideRule.DiscardingGlobalStorageDeclarations.asl
  File GuideRule.DiscardingGlobalStorageDeclarations.asl, line 1,
    characters 4 to 5:
  let - = 42;
      ^
  ASL Grammar error: Obsolete syntax: Discarded storage declaration.
  [1]
  $ aslref ASTRule.DesugarLHSAccess.asl
  $ aslref ASTRule.DesugarLHSTuple.asl
  $ aslref ASTRule.DesugarLHSFieldsTuple.asl
  $ aslref ASTRule.EBinop.bad1.asl
  File ASTRule.EBinop.bad1.asl, line 6, characters 20 to 25:
          let p_a_s = a + b - c;
                      ^^^^^
  ASL Grammar error: Cannot parse.
  [1]
  $ aslref ASTRule.EBinop.bad2.asl
  File ASTRule.EBinop.bad2.asl, line 6, characters 20 to 25:
          let p_s_a = a - b + c;
                      ^^^^^
  ASL Grammar error: Cannot parse.
  [1]
  $ aslref ASTRule.EBinop.bad3.asl
  File ASTRule.EBinop.bad3.asl, line 6, characters 23 to 30:
          let p_and_or = d AND e OR f;
                         ^^^^^^^
  ASL Grammar error: Cannot parse.
  [1]
  $ aslref ASTRule.EBinop.bad4.asl
  File ASTRule.EBinop.bad4.asl, line 6, characters 22 to 28:
          let p_eq_eq = a == b != g;
                        ^^^^^^
  ASL Grammar error: Cannot parse.
  [1]
  $ aslref ASTRule.EBinop.bad5.asl
  File ASTRule.EBinop.bad5.asl, line 6, characters 24 to 29:
          let p_sub_sub = a - b - c;
                          ^^^^^
  ASL Grammar error: Cannot parse.
  [1]
  $ aslref CaseStatement.bad.asl
  File CaseStatement.bad.asl, line 7, characters 8 to 12:
          when '11' => X[30] = 0;
          ^^^^
  ASL Grammar error: Cannot parse.
  [1]
