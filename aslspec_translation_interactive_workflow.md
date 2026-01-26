# ASLSpec Translation Assistant Interactive Workflow

## Workflow for Translating LaTeX Rules to ASLSpec

This is the step-by-step workflow to translate inference rules from LaTeX to ASLSpec implementation code.

### 10-Step Translation Workflow

1. **Scan asl.spec**
   - Look for each `typing relation <name>` or `typing function <name>` declaration
   - Note the line number and relation/function name

2. **Find LaTeX source**
   - Search for `\RenderRelation{<name>}` in the `.tex` files under `doc/`
   - Record the file and line number

3. **Locate LaTeX rules**
   - Scan down from `\RenderRelation{<name>}`
   - Find consecutive blocks of `\begin{mathpar}...\end{mathpar}` instances
   - These LaTeX rules define the formal semantics to be translated

4. **Translate LaTeX rules to ASLSpec**
   - Use the translation patterns and best practices documented in aslspec_translation_assistant.md
   - If uncertain about any part, add `// UNCERTAIN: [description]` comments
   - Generate the complete ASLSpec implementation

5. **Modify asl.spec**
   - Locate the relation/function definition in asl.spec
   - Insert `} =` followed by the translated ASLSpec rule before the closing `;`
   - Ensure proper indentation and syntax

6. **Run typechecker and fix errors**
   - Run `dune exec -- aslspec ./asl.spec --render` from `asllib/doc/`
   - Fix any syntax errors, type mismatches, or undefined functions reported
   - Iterate until the typechecker runs successfully
   - This validates the implementation before proceeding

7. **Add RenderRule directive**
   - In the `.tex` file, add `\RenderRule{<name>}` immediately AFTER the LaTeX rules (after `\end{mathpar}`)
   - This tells the PDF renderer to include the ASLSpec implementation

8. **Render and inspect**
   - Stop and ask user to render: `dune exec -- aslspec ./asl.spec --render` from `asllib/doc/`
   - Wait for user to inspect the PDF output
   - User may edit if needed; continue when approved

9. **Wrap LaTeX rules**
   - After the user confirms the PDF is correct, wrap the original LaTeX rules in the `.tex` file
   - Add `\BackupOriginalRule{` on a new line immediately before the first `\begin{mathpar}`
   - Add `} % END_OF_BACKUP_RULE` on a new line immediately after the last `\end{mathpar}`
   - This preserves the original rules as reference while showing the ASLSpec translation in the PDF

10. **Move to next relation**
   - Return to step 1
   - Continue with the next `typing relation` or `typing function` in asl.spec
