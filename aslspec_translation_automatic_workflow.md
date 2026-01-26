# ASLSpec Translation Assistant Automatic Workflow

### 8-Step Translation Workflow

1. **Scan translation_worklist.md**
  - Look for a relation <name> without an implementation and without a `// translation ...` comment

2. **Scan asl.spec**
   - Look for one of `typing relation <name>`, `typing function <name>`,
    `semantics relation <name>`, `semantics function <name>` declaration
   - Note the line number and relation/function name

3. **Find LaTeX source**
   - Search for `\RenderRelation{<name>}` in the `.tex` files under `doc/`
   - Record the file and line number

4. **Locate LaTeX rules**
   - Scan down from `\RenderRelation{<name>}`
   - Find consecutive blocks of `\begin{mathpar}...\end{mathpar}` instances
   - These LaTeX rules define the formal semantics to be translated

5. **Translate LaTeX rules to ASLSpec**
   - Use the translation patterns and best practices documented in aslspec_translation_assistant.md
   - If uncertain about any part, add `// UNCERTAIN: [description]` comments
   - Generate the complete ASLSpec implementation

6. **Modify asl.spec**
   - Locate the relation/function definition in asl.spec
   - Insert `} =` followed by the translated ASLSpec rule before the closing `;`
   - Ensure proper indentation and syntax
   - If you find the need to change a relation signature or type variant, leave a comment for the user to review,
   and maintain the old version in comments
   - If you find that you had to fix a bug in the latex code to translate the rule, leave a comment explaining the bug and your fix

7. **Run typechecker and fix errors**
   - The user set up a watch on asl.spec, which will automatically
     trigger a build. Look in aslspec.err for any errors
   - Fix any syntax errors, type mismatches, or undefined functions reported
   - Iterate until the typechecker runs successfully, but at most 5 minutes and 10 attempts
   - This validates the implementation before proceeding
   - If the translation failed, remove the translation attempt code

8. **Move to next relation**
   - Mark the result of the translation in translation_worklist.md
   - If the translation was successful add `// translation success: pending rendering` after the translation name
   - Otherwise add `// translation failed`
   - Return to step 1
