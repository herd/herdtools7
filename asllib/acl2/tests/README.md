
Running a test with ACL2's ASL interpreter
=======================

Build aslref as described in the "Source build" section of herdtools7/INSTALL.md.

Dump the static environment and AST for an ASL program into a Lisp object file using:
```
   aslref --print-lisp --no-exec myprogram.asl > myprogram.asl.lsp
```

Run ACL2 and submit the form:
```
    (assign :fname "myprogram.asl.lsp")
    (ld "run-interactive.lsp")
```

This process is automated in 'run.sh', including running aslref. It
also cuts out all the ACL2 output except what comes from ASL interpretation.
