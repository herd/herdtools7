#!/usr/bin/python3

############################################################################
# This file is mostly AI-generated and is not meant to be easily readable. #
############################################################################

from utils import read_file_lines, read_file_str, is_skipped_line
import re


def check_zero_arg_macro_misuse(latex_files: list[str]) -> int:
    r"""
    Scans ASLmacros.tex to find all zero-argument macros, then checks content .tex files
    for incorrect usage of these macros (i.e., using them with arguments like \macro{arg}).
    Returns the total number of errors found.
    """
    # First, find all zero-argument macros in ASLmacros.tex
    aslmacros_path = "ASLmacros.tex"
    lines = read_file_lines(aslmacros_path)
    zero_arg_macros: set[str] = set()
    # Pattern to match \newcommand\macroname[0]{...}
    zero_arg_pattern = re.compile(r"\\newcommand\\([a-zA-Z]+)\[0\]")
    for line in lines:
        if is_skipped_line(line):
            continue
        matches = re.findall(zero_arg_pattern, line)
        for match in matches:
            # Exclude macros that end with "term" as they can be used with {} for styling
            if (
                not match.endswith("term")
                and not match.endswith("Term")
                and not match.startswith("Prose")
                and not match.startswith("terminateas")
            ):
                zero_arg_macros.add(match)
    if not zero_arg_macros:
        return 0

    # Now check content files for incorrect usage
    num_errors = 0
    for filename in latex_files:
        lines = read_file_lines(filename)
        line_number = 0

        for line in lines:
            line_number += 1
            if is_skipped_line(line):
                continue

            # Find all macro usages with arguments and check if they're zero-argument macros
            # Pattern to match \macroname{content} where content doesn't contain unescaped braces
            macro_usage_pattern = r"\\([a-zA-Z]+)\{([^{}]*(?:\{[^{}]*\}[^{}]*)*)\}"
            matches = re.findall(macro_usage_pattern, line)

            for macro_name, match_content in matches:
                # Check if this macro is a zero-argument macro and has non-empty content
                if macro_name in zero_arg_macros and match_content.strip():
                    print(
                        f"{filename}:{line_number}: Zero-argument macro \\{macro_name} used with arguments: {match_content}"
                    )
                    num_errors += 1

    return num_errors


def check_macro_arity(latex_files: list[str]) -> int:
    r"""
    Checks that LaTeX macros are used with the correct number of arguments
    based on their definitions in ASLmacros.tex.

    For example, a macro defined as \newcommand\m[0]{...} can be used as \m or \m{},
    but not as \m{hello}. A macro defined as \newcommand\m[2]{...#1...#2...} must be
    used with exactly 2 arguments.

    This function handles nested braces correctly and multi-line arguments.
    It also handles optional parameters correctly (e.g., \newcommand\m[3][default]{...}).
    Returns the total number of errors found.
    """
    aslmacros_path = "ASLmacros.tex"

    # Extract all macro definitions with their arities and optional argument info
    # Maps macro_name -> (required_arity, has_optional_arg)
    macro_arities: dict[str, tuple[int, bool]] = {}
    lines = read_file_lines(aslmacros_path)

    # Pattern to match \newcommand\macroname[N][default] - captures the optional part
    # We use two separate captures to detect optional arguments
    macro_def_with_optional = re.compile(
        r"\\newcommand\\([a-zA-Z]+)\[(\d+)\]\[[^\]]*\]"
    )
    macro_def_without_optional = re.compile(r"\\newcommand\\([a-zA-Z]+)\[(\d+)\]")

    # Construct the macros arity map
    for line in lines:
        if is_skipped_line(line):
            continue

        # Try to match with optional argument first
        match = re.search(macro_def_with_optional, line)
        if match:
            macro_name = match.group(1)
            total_arity = int(match.group(2))
            # Has optional argument, so first param is optional
            macro_arities[macro_name] = (total_arity - 1, True)
            continue

        # Try to match without optional argument
        match = re.search(macro_def_without_optional, line)
        if match:
            macro_name = match.group(1)
            total_arity = int(match.group(2))
            # No optional argument
            macro_arities[macro_name] = (total_arity, False)
    if not macro_arities:
        return 0

    # Special cases for macros not defined in ASLmacros.tex
    macro_arities['hypertarget'] = (2, False)
    macro_arities['hyperlink'] = (2, False)

    num_errors = 0

    # For each content file, check macro usage
    for filename in latex_files:
        if is_skipped_line(filename):
            continue
        file_str = read_file_str(filename)

        # Find all macro invocations in the entire file (handles multi-line args)
        # We need to parse carefully to handle nested braces
        macro_invocations = _find_all_macro_invocations(file_str)

        for macro_name, args, line_number in macro_invocations:
            if macro_name in macro_arities:
                required_arity, has_optional = macro_arities[macro_name]
                actual_arity = len(args)

                # If the macro is referenced without braced arguments, skip.
                # This avoids false positives for typographic mentions or
                # usages with only optional [..] parameters.
                if actual_arity == 0:
                    continue

                # Allow 0-argument macros to be used with empty braces {}
                # But reject them if used with non-empty content
                if required_arity == 0 and actual_arity == 1:
                    if args[0].strip() == "":
                        # Empty braces are allowed for 0-argument macros
                        continue

                # Check if the actual arity is valid
                if has_optional:
                    # With optional arguments, we accept required_arity or required_arity+1 arguments
                    if actual_arity not in (required_arity, required_arity + 1):
                        # Truncate args for display if they're too long
                        args_display = []
                        for arg in args:
                            if len(arg) > 50:
                                args_display.append(arg[:47] + "...")
                            else:
                                args_display.append(arg)
                        args_str = " ".join([f"{{{arg}}}" for arg in args_display])
                        print(
                            f"{filename}:{line_number}: Macro \\{macro_name} expects {required_arity} or {required_arity + 1} argument(s) "
                            f"but got {actual_arity}: \\{macro_name}{args_str}"
                        )
                        num_errors += 1
                else:
                    # Without optional arguments, all arguments are required
                    if actual_arity != required_arity:
                        # Truncate args for display if they're too long
                        args_display = []
                        for arg in args:
                            if len(arg) > 50:
                                args_display.append(arg[:47] + "...")
                            else:
                                args_display.append(arg)
                        args_str = " ".join([f"{{{arg}}}" for arg in args_display])
                        print(
                            f"{filename}:{line_number}: Macro \\{macro_name} expects {required_arity} argument(s) "
                            f"but got {actual_arity}: \\{macro_name}{args_str}"
                        )
                        num_errors += 1

    return num_errors


def _find_macro_invocations(line: str) -> list[tuple[str, list[str]]]:
    r"""
    Finds all macro invocations in a line and returns a list of (macro_name, arguments) tuples.
    Macro invocations have the form \macroname{arg1}{arg2}... or \macroname with no arguments.

    This function handles nested braces correctly by properly matching brace nesting.

    Special handling for LaTeX primitives like \left, \right, etc. that don't take braced arguments.
    """
    # LaTeX primitives that are followed by delimiter symbols, not braced arguments
    latex_primitives = {
        "left",
        "right",
        "big",
        "Big",
        "bigg",
        "Bigg",  # Delimiter sizing
        "choose",
        "atop",
        "over",  # Infix operators
    }

    invocations = []
    i = 0

    while i < len(line):
        # Look for a backslash followed by macro name
        if line[i] == "\\" and i + 1 < len(line) and line[i + 1].isalpha():
            # Extract macro name
            j = i + 1
            while j < len(line) and line[j].isalpha():
                j += 1
            macro_name = line[i + 1 : j]

            # Skip LaTeX primitives that don't take braced arguments
            if macro_name in latex_primitives:
                i = j
                continue

            # Now look for arguments (any sequence of {...} groups)
            args = []
            k = j
            while k < len(line) and line[k] == "{":
                # Extract the content of this brace group, handling nesting
                arg, end_pos = _extract_brace_content(line, k)
                if arg is not None:
                    args.append(arg)
                    k = end_pos
                else:
                    break

            # Record the macro invocation (always, even if no args)
            invocations.append((macro_name, args))

            i = k if k > j else j
        else:
            i += 1

    return invocations


def _find_all_macro_invocations(file_str: str) -> list[tuple[str, list[str], int]]:
    r"""
    Finds all macro invocations in the entire file and returns a list of
    (macro_name, arguments, line_number) tuples.

    This function handles nested braces correctly even across multiple lines.
    Line numbers are calculated based on newline positions in the file.
    """
    # LaTeX primitives that are followed by delimiter symbols, not braced arguments
    latex_primitives = {
        "left",
        "right",
        "big",
        "Big",
        "bigg",
        "Bigg",  # Delimiter sizing
    }

    invocations = []
    i = 0
    line_number = 1

    while i < len(file_str):
        # Track line numbers
        if file_str[i] == "\n":
            line_number += 1
            i += 1
            continue

        # Look for a backslash followed by macro name
        if file_str[i] == "\\" and i + 1 < len(file_str) and file_str[i + 1].isalpha():
            # Record the line number where this macro starts
            macro_line_number = line_number

            # Extract macro name
            j = i + 1
            while j < len(file_str) and file_str[j].isalpha():
                j += 1
            macro_name = file_str[i + 1 : j]

            # Skip LaTeX primitives that don't take braced arguments
            if macro_name in latex_primitives:
                i = j
                continue

            # Now look for arguments (any sequence of {...} groups)
            args = []
            k = j
            while k < len(file_str):
                # Skip whitespace (spaces, tabs, newlines) between arguments
                while k < len(file_str) and file_str[k] in " \t\n\r":
                    if file_str[k] == "\n":
                        line_number += 1
                    k += 1

                # Check if we have another argument
                if k >= len(file_str) or file_str[k] != "{":
                    break

                # Extract the content of this brace group, handling nesting
                arg, end_pos = _extract_brace_content_with_linecount(
                    file_str, k, line_number
                )
                if arg is not None:
                    arg_content, new_line_count = arg
                    args.append(arg_content)
                    k = end_pos
                    line_number += new_line_count
                else:
                    break

            # Record the macro invocation (always, even if no args)
            invocations.append((macro_name, args, macro_line_number))

            i = k
        else:
            i += 1

    return invocations


def _extract_brace_content(line: str, start_pos: int) -> tuple[str | None, int]:
    r"""
    Extracts the content between matching braces starting at start_pos.
    Assumes line[start_pos] is '{'.
    Returns (content, end_position_after_closing_brace) or (None, start_pos) if no matching brace.

    Handles nested braces correctly by tracking brace depth.
    """
    if start_pos >= len(line) or line[start_pos] != "{":
        return None, start_pos

    depth = 0
    i = start_pos

    while i < len(line):
        if line[i] == "\\" and i + 1 < len(line):
            # Skip escaped characters
            i += 2
            continue
        elif line[i] == "{":
            depth += 1
            i += 1
        elif line[i] == "}":
            depth -= 1
            if depth == 0:
                # Found matching closing brace
                content = line[start_pos + 1 : i]
                return content, i + 1
            i += 1
        else:
            i += 1

    # No matching closing brace found
    return None, start_pos


def _extract_brace_content_with_linecount(
    file_str: str, start_pos: int, current_line: int
) -> tuple[tuple[str, int] | None, int]:
    r"""
    Extracts the content between matching braces starting at start_pos, and counts newlines.
    Assumes file_str[start_pos] is '{'.
    Returns ((content, newline_count), end_position_after_closing_brace) or (None, start_pos) if no matching brace.

    Handles nested braces correctly by tracking brace depth.
    """
    if start_pos >= len(file_str) or file_str[start_pos] != "{":
        return None, start_pos

    depth = 0
    i = start_pos
    newline_count = 0

    while i < len(file_str):
        if file_str[i] == "\\" and i + 1 < len(file_str):
            # Skip escaped characters
            i += 2
            continue
        elif file_str[i] == "{":
            depth += 1
            i += 1
        elif file_str[i] == "}":
            depth -= 1
            if depth == 0:
                # Found matching closing brace
                content = file_str[start_pos + 1 : i]
                return (content, newline_count), i + 1
            i += 1
        elif file_str[i] == "\n":
            newline_count += 1
            i += 1
        else:
            i += 1

    # No matching closing brace found
    return None, start_pos
