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
  ASL Error: Cannot parse.
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
