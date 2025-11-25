#!/usr/bin/python3

import sys
import os
import re
import argparse
from dataclasses import dataclass
from typing import List, Optional, Dict
from doclint import RuleBlock, match_rules, read_file_lines, get_latex_sources

# Parse command line arguments
parser = argparse.ArgumentParser(description="Translate inference rules from LaTeX files to aslspec format")
parser.add_argument(
    "--details",
    action="store_true",
    help="Print detailed output and summary of all rules",
)
args = parser.parse_args()

# Global macro dictionary, loaded at startup
MACRO_DICT = {}


def load_macro_dictionary(
    variable_macros_path: str, asl_macros_path: str, generated_macros_path: str
) -> Dict[str, str]:
    """
    Load macro definitions from variable_name_macros.tex, ASLmacros.tex, and generated_macros.tex files.
    Returns a dictionary mapping macro names to variable/function names.
    """
    macro_dict = {}

    # Load variable_name_macros.tex: \newcommand\macroname[0]{\texttt{variable_name}}
    try:
        with open(variable_macros_path, "r", encoding="utf-8") as f:
            for line in f:
                line = line.strip()
                match = re.match(
                    r"\\newcommand\\([a-zA-Z]+)\[0\]\{\\texttt\{([^}]+)\}\}", line
                )
                if match:
                    macro_name = "\\" + match.group(1)
                    variable_name = match.group(2).replace(
                        "\\_", "_"
                    )  # Unescape underscores
                    macro_dict[macro_name] = variable_name

    except FileNotFoundError:
        print(f"Warning: Could not find variable macro file {variable_macros_path}")
    except Exception as e:
        print(f"Warning: Error reading variable macro file {variable_macros_path}: {e}")

    # Load ASLmacros.tex: \newcommand\macro[0]{\hyperlink{...}{\textfunc{underscore_escaped_name}}}
    try:
        with open(asl_macros_path, "r", encoding="utf-8") as f:
            for line in f:
                line = line.strip()
                # Match pattern: \newcommand\macroname[0]{\hyperlink{...}{\textfunc{function_name}}}
                match = re.match(
                    r"\\newcommand\\([a-zA-Z]+)\[0\]\{\\hyperlink\{[^}]+\}\{\\textfunc\{([^}]+)\}\}\}",
                    line,
                )
                if not match:
                    # Also try \textsf pattern: \newcommand\macroname[0]{\hyperlink{...}{\textsf{name}}}
                    match = re.match(
                        r"\\newcommand\\([a-zA-Z]+)\[0\]\{\\hyperlink\{[^}]+\}\{\\textsf\{([^}]+)\}\}\}",
                        line,
                    )
                if not match:
                    # Also try \text pattern: \newcommand\macroname[0]{\text{name}}
                    match = re.match(
                        r"\\newcommand\\([a-zA-Z]+)\[0\]\{\\text\{([^}]+)\}\}", line
                    )
                if not match:
                    # Also try \texttt pattern: \newcommand\macroname[0]{\hyperlink{...}{\texttt{name}}}
                    match = re.match(
                        r"\\newcommand\\([a-zA-Z]+)\[0\]\{\\hyperlink\{[^}]+\}\{\\texttt\{([^}]+)\}\}\}",
                        line,
                    )
                if match:
                    macro_name = "\\" + match.group(1)
                    function_name = match.group(2).replace(
                        "\\_", "_"
                    )  # Unescape underscores
                    # Only add if function_name contains only letters, digits, underscore, and apostrophe
                    if re.match(r"^[a-zA-Z0-9_\']+$", function_name):
                        macro_dict[macro_name] = function_name

    except FileNotFoundError:
        print(f"Warning: Could not find ASL macro file {asl_macros_path}")
    except Exception as e:
        print(f"Warning: Error reading ASL macro file {asl_macros_path}: {e}")

    # Load generated_macros.tex: \newcommand\macro[0]{\hyperlink{...}{\textsf{underscore_escaped_name}}}
    try:
        with open(generated_macros_path, "r", encoding="utf-8") as f:
            for line in f:
                line = line.strip()
                # Match pattern: \newcommand\macroname[0]{\hyperlink{...}{\textsf{name}}}
                match = re.match(
                    r"\\newcommand\\([a-zA-Z]+)\[0\]\{\s*\\hyperlink\{[^}]+\}\{\\textsf\{([^}]+)\}\}\s*\}",
                    line,
                )
                if not match:
                    # Also try \textsc pattern
                    match = re.match(
                        r"\\newcommand\\([a-zA-Z]+)\[0\]\{\s*\\hyperlink\{[^}]+\}\{\\textsc\{([^}]+)\}\}\s*\}",
                        line,
                    )
                if not match:
                    # Also try \textit pattern (for relation names)
                    match = re.match(
                        r"\\newcommand\\([a-zA-Z]+)\[0\]\{\s*\\hyperlink\{[^}]+\}\{\\textit\{([^}]+)\}\}\s*\}",
                        line,
                    )
                if match:
                    macro_name = "\\" + match.group(1)
                    type_name = match.group(2).replace(
                        "\\_", "_"
                    )  # Unescape underscores
                    # Only add if type_name contains only letters, digits, underscore, and apostrophe
                    if re.match(r"^[a-zA-Z0-9_\']+$", type_name):
                        macro_dict[macro_name] = type_name

    except FileNotFoundError:
        print(f"Warning: Could not find generated macro file {generated_macros_path}")
    except Exception as e:
        print(
            f"Warning: Error reading generated macro file {generated_macros_path}: {e}"
        )

    return macro_dict


def get_custom_macro_replacements() -> Dict[str, str]:
    """
    Return a dictionary of custom macro replacements for special cases.
    """
    return {
        "\\OrTypeError": "", # will be inserted automatically by aslspec rendering
        "\\typearrow": "->",
        "\\eqdef": ":=",
        "\\eqname": "=:",
        "\\checktransarrow": "->",
        "\\Ignore": "_",
        "\\ ": "",
        "\\left(": "(",
        "\\right)": ")",
        "\\left[": "[",
        "\\right]": "]",
        "\\right\\}": "}",
        "\\in": "IN",
        "\\neg": "NOT",
        "\\emptylist": "empty_list",
        "\\emptyset": "empty_set",
        "\\lor": "OR",
        "\\land": "AND",
        "\\commonprefixline": "",
        "\\commonsuffixline": "",
        "\\some": "some",
        "\\cup": "UNION",
    }


def get_custom_string_replacements() -> Dict[str, str]:
    """
    Return a dictionary of custom string replacements for non-macro patterns.
    These are applied after macro replacements.
    """
    return {"G^ tenv": "tenv.G", "L^ tenv": "tenv.L"}


def replace_custom_strings(text: str, replacements: Dict[str, str]) -> str:
    """
    Replace custom string patterns in the text.
    """
    if not replacements:
        return text

    result = text
    for old_pattern, new_pattern in replacements.items():
        result = result.replace(old_pattern, new_pattern)

    return result


def replace_macros_in_judgment(judgment: str, macro_dict: Dict[str, str]) -> str:
    """
    Replace LaTeX macros in a judgment with their corresponding variable names.
    Tokenizes the judgment to find macro instances (\\letters) and replaces them if found in the dictionary.
    """
    if not macro_dict:
        return judgment

    result = []
    pos = 0

    while pos < len(judgment):
        if judgment[pos] == "\\":
            # Check for multi-character patterns first (longer patterns first)
            found_pattern = False

            # Check for \left\{ and \right\}
            if pos + 7 < len(judgment) and judgment[pos : pos + 7] == "\\left\\{":
                if "\\left\\{" in macro_dict:
                    result.append(macro_dict["\\left\\{"])
                    pos += 7
                    found_pattern = True
            elif pos + 8 < len(judgment) and judgment[pos : pos + 8] == "\\right\\}":
                if "\\right\\}" in macro_dict:
                    result.append(macro_dict["\\right\\}"])
                    pos += 8
                    found_pattern = True
            # Check for other \left and \right patterns
            elif pos + 6 < len(judgment) and judgment[pos : pos + 6] == "\\left(":
                if "\\left(" in macro_dict:
                    result.append(macro_dict["\\left("])
                    pos += 6
                    found_pattern = True
            elif pos + 6 < len(judgment) and judgment[pos : pos + 6] == "\\left[":
                if "\\left[" in macro_dict:
                    result.append(macro_dict["\\left["])
                    pos += 6
                    found_pattern = True
            elif pos + 6 < len(judgment) and judgment[pos : pos + 6] == "\\left.":
                if "\\left." in macro_dict:
                    result.append(macro_dict["\\left."])
                    pos += 6
                    found_pattern = True
            elif pos + 7 < len(judgment) and judgment[pos : pos + 7] == "\\right)":
                if "\\right)" in macro_dict:
                    result.append(macro_dict["\\right)"])
                    pos += 7
                    found_pattern = True
            elif pos + 7 < len(judgment) and judgment[pos : pos + 7] == "\\right]":
                if "\\right]" in macro_dict:
                    result.append(macro_dict["\\right]"])
                    pos += 7
                    found_pattern = True
            elif pos + 7 < len(judgment) and judgment[pos : pos + 7] == "\\right.":
                if "\\right." in macro_dict:
                    result.append(macro_dict["\\right."])
                    pos += 7
                    found_pattern = True
            # Check for special case of "\ " (backslash followed by space)
            elif pos + 1 < len(judgment) and judgment[pos + 1] == " ":
                macro_name = "\\ "
                if macro_name in macro_dict:
                    result.append(macro_dict[macro_name])
                    pos += 2  # Skip both backslash and space
                    found_pattern = True

            if not found_pattern:
                if pos + 1 < len(judgment) and judgment[pos + 1].isalpha():
                    # Found potential alphabetic macro, extract the full macro name
                    macro_start = pos
                    pos += 1  # Skip the backslash

                    # Collect all alphabetic characters
                    while pos < len(judgment) and judgment[pos].isalpha():
                        pos += 1

                    macro_name = judgment[macro_start:pos]

                    # Check if this macro is in our dictionary
                    if macro_name in macro_dict:
                        # Add spaces around replacement to prevent tokens sticking together
                        # e.g., \vs\in\vses would become s\inses without spacing, but s \in ses with spacing
                        result.append(" " + macro_dict[macro_name] + " ")
                    else:
                        result.append(macro_name)
                else:
                    result.append(judgment[pos])
                    pos += 1
        else:
            result.append(judgment[pos])
            pos += 1

    return "".join(result)


@dataclass
class Case:
    """
    A class representing an individual inference rule.
    """

    name: Optional[str]  # Optional case name from \\inferrule[name]
    premises: List[str]  # List of premise strings
    conclusion: str  # Conclusion string
    relation_name: str  # Relation name extracted from conclusion
    error: Optional[str] = None  # Error message if parsing failed


@dataclass
class Rule:
    """
    A class representing a parsed typing rule with its name and mathpar blocks.
    """

    name: str
    mathpar_blocks: List[str]
    cases: List[Case]
    filename: str
    rule_type: str
    relation_name: str  # Relation name for all cases in this rule


def extract_mathpar_blocks(
    file_lines: List[str], start_line: int, end_line: int
) -> List[str]:
    """
    Extract all mathpar blocks from the given range of lines.
    Returns a list of strings, each containing the content of one mathpar block.
    Splits blocks containing multiple cases separated by \and.
    """
    mathpar_blocks = []
    current_block = []
    inside_mathpar = False

    for line_idx in range(start_line, min(end_line + 1, len(file_lines))):
        line = file_lines[line_idx].strip()

        # Skip comment lines
        if line.startswith("%"):
            continue

        if r"\begin{mathpar}" in line:
            inside_mathpar = True
            current_block = []  # Don't include \\begin{mathpar}
            continue

        if r"\end{mathpar}" in line:
            if inside_mathpar:
                # Don't include \\end{mathpar}, process the block content
                block_content = "\n".join(current_block)
                # Split by \\and to handle multiple cases in one mathpar block
                cases = split_mathpar_cases(block_content)
                mathpar_blocks.extend(cases)
                current_block = []
                inside_mathpar = False
            continue

        if inside_mathpar:
            current_block.append(line)

    return mathpar_blocks


def split_mathpar_cases(block_content: str) -> List[str]:
    """
    Split a mathpar block content by \and to separate multiple cases.
    Returns a list of individual case strings.
    """
    if not block_content.strip():
        return []

    # Look for \\and separators, but be careful about nested braces
    cases = []
    current_case = []
    lines = block_content.split("\n")
    brace_count = 0

    for line in lines:
        # Count braces to track nesting level
        for char in line:
            if char == "{":
                brace_count += 1
            elif char == "}":
                brace_count -= 1

        # Check if this line contains \\and at the top level (brace_count == 0)
        if brace_count == 0 and r"\and" in line:
            # Split the line at \\and
            parts = line.split(r"\and")
            if current_case or parts[0].strip():
                current_case.append(parts[0])
                cases.append("\n".join(current_case).strip())

            # Start new cases for remaining parts
            for i in range(1, len(parts)):
                if parts[i].strip():
                    cases.append(parts[i].strip())
                current_case = []
        else:
            current_case.append(line)

    # Add the last case if it exists
    if current_case:
        case_content = "\n".join(current_case).strip()
        if case_content:
            cases.append(case_content)

    # If no \\and was found, return the original content as a single case
    if not cases:
        cases = [block_content.strip()]

    return cases


def split_premises(premises_text: str) -> List[str]:
    """
    Split premises by \\\\ while respecting nested braces and LaTeX constructs.
    Handles both regular braces and LaTeX \\left{ \\right} and \\right. constructs.
    """
    if not premises_text.strip():
        return []

    premises = []
    current_premise = []
    pos = 0
    brace_count = 0

    while pos < len(premises_text):
        # Check for LaTeX constructs first
        if pos <= len(premises_text) - 6 and premises_text[pos : pos + 6] == r"\left{":
            brace_count += 1
            current_premise.append(premises_text[pos : pos + 6])
            pos += 6
        elif pos <= len(premises_text) - 7 and (
            premises_text[pos : pos + 7] == r"\right}"
            or premises_text[pos : pos + 7] == r"\right."
        ):
            brace_count -= 1
            current_premise.append(premises_text[pos : pos + 7])
            pos += 7
        # Check for regular braces
        elif premises_text[pos] == "{":
            brace_count += 1
            current_premise.append(premises_text[pos])
            pos += 1
        elif premises_text[pos] == "}":
            brace_count -= 1
            current_premise.append(premises_text[pos])
            pos += 1
        # Check for premise separator \\\\ at top level (brace_count == 0)
        elif (
            brace_count == 0
            and pos <= len(premises_text) - 2
            and premises_text[pos : pos + 2] == r"\\"
        ):
            # Found premise separator at top level
            premise_content = "".join(current_premise).strip()
            if premise_content:
                premises.append(premise_content)
            current_premise = []
            pos += 2
            # Skip whitespace after \\\\
            while pos < len(premises_text) and premises_text[pos].isspace():
                pos += 1
        else:
            current_premise.append(premises_text[pos])
            pos += 1

    # Add the last premise if any
    premise_content = "".join(current_premise).strip()
    if premise_content:
        premises.append(premise_content)

    return premises


def simplify_arrays(text: str) -> str:
    """
    Simplify single-column LaTeX array environments.
    Converts \\begin{array}{r} ... \\end{array} to just the content with line breaks.
    Handles nested braces and nested array environments.
    """
    result = []
    pos = 0

    while pos < len(text):
        # Look for \begin{array}
        begin_match = text.find(r"\begin{array}", pos)
        if begin_match == -1:
            # No more arrays, append the rest
            result.append(text[pos:])
            break

        # Append text before the array
        result.append(text[pos:begin_match])

        # Find the column specification
        col_start = begin_match + len(r"\begin{array}")
        if col_start < len(text) and text[col_start] == "{":
            # Find the matching closing brace for column specification
            brace_count = 1
            col_spec_start = col_start + 1
            col_end = col_start + 1

            while col_end < len(text) and brace_count > 0:
                if text[col_end] == "{":
                    brace_count += 1
                elif text[col_end] == "}":
                    brace_count -= 1
                col_end += 1

            if brace_count != 0:
                # Malformed array, skip this one
                result.append(text[begin_match:col_end])
                pos = col_end
                continue

            col_spec = text[col_spec_start : col_end - 1].strip()

            # Check if it's a single column (only contains 'r', 'l', or 'c')
            is_single_column = len(col_spec) == 1 and col_spec in "rlc"

            if not is_single_column:
                # Multi-column array, don't simplify
                # Find the matching \end{array} and append as-is
                array_content_start = col_end
                array_end = find_matching_array_end(text, array_content_start)
                if array_end != -1:
                    result.append(text[begin_match:array_end])
                    pos = array_end
                else:
                    # No matching end found, append rest as-is
                    result.append(text[begin_match:])
                    break
                continue

            # Single column array, extract and simplify content
            array_content_start = col_end
            array_end = find_matching_array_end(text, array_content_start)

            if array_end == -1:
                # No matching \end{array} found, treat as malformed
                result.append(text[begin_match:])
                break

            # Extract content between \begin{array}{X} and \end{array}
            array_content = text[
                array_content_start : array_end - len(r"\end{array}")
            ].strip()

            # Recursively simplify nested arrays in the content
            simplified_content = simplify_arrays(array_content)

            # Split by \\\\ and clean up each line
            lines = []
            current_line = []
            i = 0
            brace_count = 0

            while i < len(simplified_content):
                # Track braces including LaTeX constructs
                if (
                    i <= len(simplified_content) - 6
                    and simplified_content[i : i + 6] == r"\left{"
                ):
                    brace_count += 1
                    current_line.append(simplified_content[i : i + 6])
                    i += 6
                elif i <= len(simplified_content) - 7 and (
                    simplified_content[i : i + 7] == r"\right}"
                    or simplified_content[i : i + 7] == r"\right."
                ):
                    brace_count -= 1
                    current_line.append(simplified_content[i : i + 7])
                    i += 7
                elif simplified_content[i] == "{":
                    brace_count += 1
                    current_line.append(simplified_content[i])
                    i += 1
                elif simplified_content[i] == "}":
                    brace_count -= 1
                    current_line.append(simplified_content[i])
                    i += 1
                # Check for \\\\ at top level (brace_count == 0)
                elif (
                    brace_count == 0
                    and i <= len(simplified_content) - 2
                    and simplified_content[i : i + 2] == r"\\"
                ):
                    # Found line separator at top level
                    line_content = "".join(current_line).strip()
                    if line_content:
                        lines.append(line_content)
                    current_line = []
                    i += 2
                    # Skip whitespace after \\\\
                    while (
                        i < len(simplified_content) and simplified_content[i].isspace()
                    ):
                        i += 1
                else:
                    current_line.append(simplified_content[i])
                    i += 1

            # Add the last line if any
            line_content = "".join(current_line).strip()
            if line_content:
                lines.append(line_content)

            # Join lines with newlines
            result.append("\n".join(lines))
            pos = array_end

        else:
            # Malformed array without column spec, skip
            result.append(text[begin_match:col_start])
            pos = col_start

    return "".join(result)


def find_matching_array_end(text: str, start_pos: int) -> int:
    """
    Find the position after the matching \\end{array} for an array starting at start_pos.
    Handles nested arrays properly.
    """
    pos = start_pos
    array_depth = 1  # We're already inside one array

    while pos < len(text) and array_depth > 0:
        # Look for \begin{array} (increases depth)
        begin_pos = text.find(r"\begin{array}", pos)
        # Look for \end{array} (decreases depth)
        end_pos = text.find(r"\end{array}", pos)

        if begin_pos != -1 and (end_pos == -1 or begin_pos < end_pos):
            # Found nested \begin{array}
            array_depth += 1
            pos = begin_pos + len(r"\begin{array}")
        elif end_pos != -1:
            # Found \end{array}
            array_depth -= 1
            pos = end_pos + len(r"\end{array}")
            if array_depth == 0:
                return pos
        else:
            # No more begin or end found
            break

    return -1  # No matching end found


def remove_outer_braces(text: str) -> str:
    """
    Remove outer braces from a judgment if it starts with { and ends with }.
    """
    text = text.strip()
    if text.startswith("{") and text.endswith("}"):
        return text[1:-1].strip()
    return text


def transform_judgment(judgment: str) -> str:
    """
    Transform a judgment (premise or conclusion) from LaTeX format to aslspec format.
    Currently applies array simplification, outer brace removal, removes % characters,
    replaces LaTeX macros with variable names, and applies custom string replacements.
    This will be expanded to perform additional transformations later.
    """
    # Simplify single-column array environments
    simplified = simplify_arrays(judgment)
    # Remove outer braces if they wrap the entire content
    no_outer_braces = remove_outer_braces(simplified)
    # Remove all % characters
    no_percent = no_outer_braces.replace("%", "")
    # Replace LaTeX macros with variable names
    macro_replaced = replace_macros_in_judgment(no_percent, MACRO_DICT)
    # Apply custom string replacements
    custom_string_replacements = get_custom_string_replacements()
    string_replaced = replace_custom_strings(macro_replaced, custom_string_replacements)
    return string_replaced


def parse_case(case_content: str) -> Case:
    """
    Parse a single case content into a Case object.
    Extracts the case name (if any), premises, and conclusion.
    The input should already have \\begin{mathpar} and \\end{mathpar} removed.
    """
    content = case_content.strip()

    # Pattern to match \\inferrule[optional_name]{premises}{conclusion}
    # We need to handle nested braces properly
    inferrule_match = re.match(r"\\inferrule", content)
    if not inferrule_match:
        # No \\inferrule found, this is an error
        error_msg = f"No \\inferrule found in case content: {content[:100]}..."
        return Case(name=None, premises=[], conclusion="", relation_name="", error=error_msg)

    # Start after \\inferrule
    pos = inferrule_match.end()

    # Check for optional [name] part
    case_name = None
    if pos < len(content) and content[pos] == "[":
        # Find matching closing bracket
        bracket_count = 1
        start_pos = pos + 1
        pos += 1
        while pos < len(content) and bracket_count > 0:
            if content[pos] == "[":
                bracket_count += 1
            elif content[pos] == "]":
                bracket_count -= 1
            pos += 1
        if bracket_count == 0:
            case_name = content[start_pos : pos - 1].strip()
            # Unescape underscores in case names
            case_name = case_name.replace("\\_", "_")

    # Skip whitespace and newlines to find the first brace for premises
    while pos < len(content) and content[pos].isspace():
        pos += 1

    # Now we should be at the first brace for premises
    if pos >= len(content) or content[pos] != "{":
        # Malformed, return what we have
        error_msg = f"Expected '{{' for premises but found: {content[pos:pos+20]}..."
        return Case(
            name=case_name,
            premises=[],
            conclusion=content[pos:].strip(),
            relation_name="",
            error=error_msg,
        )

    # Find matching closing brace for premises
    # Handle both regular braces and LaTeX \\left{ \\right} and \\right. constructs
    brace_count = 1
    premises_start = pos + 1
    pos += 1
    while pos < len(content) and brace_count > 0:
        if content[pos : pos + 6] == r"\left{":
            brace_count += 1
            pos += 6
        elif (
            content[pos : pos + 7] == r"\right}" or content[pos : pos + 7] == r"\right."
        ):
            brace_count -= 1
            pos += 7
        elif content[pos] == "{":
            brace_count += 1
            pos += 1
        elif content[pos] == "}":
            brace_count -= 1
            pos += 1
        else:
            pos += 1

    if brace_count != 0:
        # Malformed, return what we have
        error_msg = f"Unmatched braces in premises section, brace_count: {brace_count}"
        return Case(name=case_name, premises=[], conclusion="", relation_name="", error=error_msg)

    premises_text = content[premises_start : pos - 1].strip()
    # Parse premises by splitting on \\\\ while respecting nested braces
    premises_list = split_premises(premises_text)

    # Skip whitespace and newlines to find the second brace for conclusion
    while pos < len(content) and content[pos].isspace():
        pos += 1

    # Now we should be at the second brace for conclusion
    if pos >= len(content) or content[pos] != "{":
        # Malformed, return what we have
        error_msg = f"Expected '{{' for conclusion but found: {content[pos:pos+20]}..."
        return Case(
            name=case_name,
            premises=premises_list,
            conclusion="",
            relation_name="",
            error=error_msg,
        )

    # Find matching closing brace for conclusion
    # Handle both regular braces and LaTeX \\left{ \\right} and \\right. constructs
    brace_count = 1
    conclusion_start = pos + 1
    pos += 1
    while pos < len(content) and brace_count > 0:
        if content[pos : pos + 6] == r"\left{":
            brace_count += 1
            pos += 6
        elif (
            content[pos : pos + 7] == r"\right}" or content[pos : pos + 7] == r"\right."
        ):
            brace_count -= 1
            pos += 7
        elif content[pos] == "{":
            brace_count += 1
            pos += 1
        elif content[pos] == "}":
            brace_count -= 1
            pos += 1
        else:
            pos += 1

    if brace_count != 0:
        # Malformed, return what we have
        error_msg = (
            f"Unmatched braces in conclusion section, brace_count: {brace_count}"
        )
        return Case(
            name=case_name,
            premises=premises_list,
            conclusion="",
            relation_name="",
            error=error_msg,
        )

    conclusion_text = content[conclusion_start : pos - 1].strip()

    return Case(
        name=case_name, premises=premises_list, conclusion=conclusion_text, relation_name=""
    )


def parse_conclusion(case: Case) -> tuple[Optional[str], List[str], str]:
    """
    Parse the conclusion to extract relation name, input arguments, and output expression.
    Expects conclusion in the form 'name(arguments) -> output'.
    Returns (relation_name, arguments_list, output_expression).
    If parsing fails, returns (None, [], conclusion).
    """
    if case.error:
        return (None, [], case.conclusion)

    conclusion = case.conclusion.strip()

    # Look for pattern: name(arguments) -> output
    # Find the first '(' to identify the relation name
    paren_pos = conclusion.find('(')
    if paren_pos == -1:
        # No parentheses found, cannot extract relation name
        return (None, [], conclusion)

    # Extract relation name (everything before the first '(')
    relation_name = conclusion[:paren_pos].strip()

    # Find the matching ')' for the arguments, handling nested structures including LaTeX constructs
    pos = paren_pos + 1
    paren_count = 1
    brace_count = 0
    bracket_count = 0

    while pos < len(conclusion) and paren_count > 0:
        # Check for LaTeX constructs first
        if pos <= len(conclusion) - 6 and conclusion[pos:pos + 6] == r"\left{":
            brace_count += 1
            pos += 6
            continue
        elif pos <= len(conclusion) - 7 and (
            conclusion[pos:pos + 7] == r"\right}" or conclusion[pos:pos + 7] == r"\right."
        ):
            brace_count -= 1
            pos += 7
            continue
        elif pos <= len(conclusion) - 6 and conclusion[pos:pos + 6] == r"\left(":
            paren_count += 1
            pos += 6
            continue
        elif pos <= len(conclusion) - 6 and conclusion[pos:pos + 6] == r"\left[":
            bracket_count += 1
            pos += 6
            continue
        elif pos <= len(conclusion) - 7 and conclusion[pos:pos + 7] == r"\right)":
            paren_count -= 1
            pos += 7
            continue
        elif pos <= len(conclusion) - 7 and conclusion[pos:pos + 7] == r"\right]":
            bracket_count -= 1
            pos += 7
            continue

        # Handle regular characters
        char = conclusion[pos]

        if char == '(':
            paren_count += 1
        elif char == ')':
            paren_count -= 1
        elif char == '{':
            brace_count += 1
        elif char == '}':
            brace_count -= 1
        elif char == '[':
            bracket_count += 1
        elif char == ']':
            bracket_count -= 1

        pos += 1

    if paren_count != 0:
        # Unmatched parentheses, cannot parse
        return (None, [], conclusion)

    # Extract arguments text (between the parentheses)
    arguments_text = conclusion[paren_pos + 1:pos - 1]

    # Parse the arguments (comma-separated, respecting nested structures including LaTeX constructs)
    arguments = []
    if arguments_text.strip():
        current_arg = []
        arg_pos = 0
        paren_count = 0
        brace_count = 0
        bracket_count = 0

        while arg_pos < len(arguments_text):
            # Check for LaTeX constructs first
            if arg_pos <= len(arguments_text) - 6 and arguments_text[arg_pos:arg_pos + 6] == r"\left{":
                brace_count += 1
                current_arg.append(arguments_text[arg_pos:arg_pos + 6])
                arg_pos += 6
                continue
            elif arg_pos <= len(arguments_text) - 7 and (
                arguments_text[arg_pos:arg_pos + 7] == r"\right}" or arguments_text[arg_pos:arg_pos + 7] == r"\right."
            ):
                brace_count -= 1
                current_arg.append(arguments_text[arg_pos:arg_pos + 7])
                arg_pos += 7
                continue
            elif arg_pos <= len(arguments_text) - 6 and arguments_text[arg_pos:arg_pos + 6] == r"\left(":
                paren_count += 1
                current_arg.append(arguments_text[arg_pos:arg_pos + 6])
                arg_pos += 6
                continue
            elif arg_pos <= len(arguments_text) - 6 and arguments_text[arg_pos:arg_pos + 6] == r"\left[":
                bracket_count += 1
                current_arg.append(arguments_text[arg_pos:arg_pos + 6])
                arg_pos += 6
                continue
            elif arg_pos <= len(arguments_text) - 7 and arguments_text[arg_pos:arg_pos + 7] == r"\right)":
                paren_count -= 1
                current_arg.append(arguments_text[arg_pos:arg_pos + 7])
                arg_pos += 7
                continue
            elif arg_pos <= len(arguments_text) - 7 and arguments_text[arg_pos:arg_pos + 7] == r"\right]":
                bracket_count -= 1
                current_arg.append(arguments_text[arg_pos:arg_pos + 7])
                arg_pos += 7
                continue

            # Handle regular characters
            char = arguments_text[arg_pos]

            if char == '(':
                paren_count += 1
            elif char == ')':
                paren_count -= 1
            elif char == '{':
                brace_count += 1
            elif char == '}':
                brace_count -= 1
            elif char == '[':
                bracket_count += 1
            elif char == ']':
                bracket_count -= 1
            elif char == ',' and paren_count == 0 and brace_count == 0 and bracket_count == 0:
                # Found argument separator at top level
                arg_content = ''.join(current_arg).strip()
                if arg_content:
                    arguments.append(arg_content)
                current_arg = []
                arg_pos += 1
                continue

            current_arg.append(char)
            arg_pos += 1

        # Add the last argument if any
        arg_content = ''.join(current_arg).strip()
        if arg_content:
            arguments.append(arg_content)

    # Extract output expression (everything after the closing parenthesis)
    # Look for ' -> ' pattern and exclude the arrow itself
    remaining_text = conclusion[pos:].strip()
    arrow_pos = remaining_text.find(' -> ')
    if arrow_pos != -1:
        output_expression = remaining_text[arrow_pos + 4:].strip()
    else:
        # No arrow found, use remaining text as output (but strip any leading -> if present)
        output_expression = remaining_text
        if output_expression.startswith('->'):
            output_expression = output_expression[2:].strip()
        elif output_expression.startswith(' ->'):
            output_expression = output_expression[3:].strip()

    return (relation_name, arguments, output_expression)


def remove_overname_wrappers(text: str) -> str:
    """
    Remove \\overname{<expression>}{name} wrappers from text, keeping only <expression>.
    """
    result = text

    # Keep processing until no more \\overname patterns are found
    while "\\overname{" in result:
        # Find the start of an overname pattern
        start_pos = result.find("\\overname{")
        if start_pos == -1:
            break

        pos = start_pos + len("\\overname{")
        brace_count = 1
        expression_start = pos

        # Find the end of the expression part by counting braces
        while pos < len(result) and brace_count > 0:
            if result[pos] == '{':
                brace_count += 1
            elif result[pos] == '}':
                brace_count -= 1
            pos += 1

        if brace_count != 0:
            # Unmatched braces, skip this pattern
            break

        expression = result[expression_start:pos-1]

        # Now skip the name part {name}
        if pos < len(result) and result[pos] == '{':
            pos += 1
            brace_count = 1

            # Find the end of the name part
            while pos < len(result) and brace_count > 0:
                if result[pos] == '{':
                    brace_count += 1
                elif result[pos] == '}':
                    brace_count -= 1
                pos += 1

            if brace_count != 0:
                # Unmatched braces, skip this pattern
                break

            # Replace the entire \\overname{expression}{name} with just expression
            result = result[:start_pos] + expression + result[pos:]
        else:
            # No name part found, skip this pattern
            break

    return result


def transform_case(case: Case) -> Case:
    """
    Transform a case from LaTeX format to aslspec format.
    Takes a Case with raw premises and conclusion and returns a new Case with transformed values.
    If the case has an error, returns it unchanged.
    """
    if case.error:
        return case

    # Transform premises and conclusion from LaTeX to aslspec format
    transformed_premises = [transform_judgment(premise) for premise in case.premises]
    transformed_conclusion = transform_judgment(case.conclusion)

    # Create intermediate case with transformed conclusion
    intermediate_case = Case(
        name=case.name,
        premises=transformed_premises,
        conclusion=transformed_conclusion,
        relation_name=case.relation_name,
        error=case.error
    )

    # Parse the conclusion to extract relation name, arguments, and output
    relation_name, arguments, output_expression = parse_conclusion(intermediate_case)

    # Check if parse_conclusion failed - relation name is required
    if relation_name is None:
        return Case(
            name=case.name,
            premises=transformed_premises,
            conclusion=transformed_conclusion,
            relation_name="",
            error=f"Failed to extract relation name from conclusion: {transformed_conclusion}"
        )

    # Extract premises from the conclusion arguments
    extracted_premises, premise_warnings = extract_premises_from_inputs(arguments)

    # Add extracted premises to the beginning of the premise list
    final_premises = extracted_premises + transformed_premises

    # Remove \overname wrappers from all premises
    final_premises = [remove_overname_wrappers(premise) for premise in final_premises]

    # Transform the conclusion by processing the output expression
    # 1. Remove \overname{<expression>}{name} wrappers, keeping only <expression>
    transformed_output = remove_overname_wrappers(output_expression)

    # 2. Replace the conclusion with the transformed output expression
    final_conclusion = transformed_output

    # Collect any warnings from premise extraction
    warning_text = ""
    if premise_warnings:
        warning_text = "; ".join(f"WARNING: {warning}" for warning in premise_warnings)

    return Case(
        name=case.name,
        premises=final_premises,
        conclusion=final_conclusion,
        relation_name=relation_name,
        error=case.error if not warning_text else (case.error + "; " + warning_text if case.error else warning_text)
    )


def extract_premises_from_inputs(arguments: List[str]) -> tuple[List[str], List[str]]:
    """
    Extract premises from the list of arguments obtained from parse_conclusion.

    Rules:
    - If an argument is a single identifier, drop it.
    - If an argument has the form \\overname{expression}{name}, produce a premise 'name = expression'.
    - Otherwise, generate a warning and continue processing other arguments.

    Returns (premises_list, warnings_list).
    """
    premises = []
    warnings = []

    for arg in arguments:
        arg = arg.strip()

        # Check if it's a single identifier (letters, digits, underscore, apostrophe)
        if re.match(r"^[a-zA-Z_][a-zA-Z0-9_']*$", arg):
            # Single identifier, drop it
            continue

        # Check for \\overname{expression}{name} pattern with proper brace handling
        if arg.startswith("\\overname{"):
            # Parse \\overname{expression}{name} with nested brace support
            prefix_len = len("\\overname{")
            pos = prefix_len
            brace_count = 1
            expression_start = pos

            # Find the end of the expression part by counting braces
            while pos < len(arg) and brace_count > 0:
                if arg[pos] == '{':
                    brace_count += 1
                elif arg[pos] == '}':
                    brace_count -= 1
                pos += 1

            if brace_count != 0:
                warnings.append(f"Unmatched braces in overname expression: {arg}")
                continue

            expression = arg[expression_start:pos-1].strip()

            # Now parse the name part
            if pos < len(arg) and arg[pos] == '{':
                name_start = pos + 1
                pos += 1
                brace_count = 1

                # Find the end of the name part
                while pos < len(arg) and brace_count > 0:
                    if arg[pos] == '{':
                        brace_count += 1
                    elif arg[pos] == '}':
                        brace_count -= 1
                    pos += 1

                if brace_count != 0:
                    warnings.append(f"Unmatched braces in overname name: {arg}")
                    continue

                name = arg[name_start:pos-1].strip()
                premise = f"{name} = {expression}"
                premises.append(premise)
                continue
            else:
                warnings.append(f"Missing name part in overname: {arg}")
                continue

        # If we reach here, we couldn't parse the argument
        warnings.append(f"Unable to parse argument: {arg}")

    return (premises, warnings)


def parse_cases(rules: List[Rule]) -> tuple[int, int]:
    """
    Parse mathpar_blocks into Case objects for each rule.
    Returns (successful_count, error_count).
    """
    successful_count = 0
    error_count = 0

    for rule in rules:
        cases = []
        rule_has_error = False
        for case_content in rule.mathpar_blocks:
            case = parse_case(case_content)
            cases.append(case)
            if case.error:
                rule_has_error = True
        rule.cases = cases

        if rule_has_error:
            error_count += 1
        else:
            successful_count += 1

    return successful_count, error_count


def transform_cases(rules: List[Rule]) -> int:
    """
    Transform all cases in the given rules from LaTeX format to aslspec format.
    Modifies the rules in place by replacing each case with its transformed version.
    Validates that all cases in a rule have the same relation name.
    Returns the number of cases with warnings.
    """
    warning_count = 0

    for rule in rules:
        transformed_cases = []
        rule_relation_names = set()

        for case in rule.cases:
            transformed_case = transform_case(case)
            if transformed_case.error and "WARNING:" in transformed_case.error:
                warning_count += 1
            transformed_cases.append(transformed_case)

            # Collect relation names from non-error cases
            if not transformed_case.error and transformed_case.relation_name:
                rule_relation_names.add(transformed_case.relation_name)

        rule.cases = transformed_cases

        # Validate that all cases have the same relation name
        if len(rule_relation_names) == 0:
            # No relation names found (all cases have errors or no relation names)
            rule.relation_name = ""
        elif len(rule_relation_names) == 1:
            # All cases have the same relation name
            rule.relation_name = list(rule_relation_names)[0]
        else:
            # Multiple different relation names - this is an error
            relation_names_str = ", ".join(sorted(rule_relation_names))
            error_msg = f"Rule has cases with different relation names: {relation_names_str}"

            # Mark all cases in this rule as having an error
            for case in rule.cases:
                if not case.error:
                    case.error = error_msg
                else:
                    case.error += f"; {error_msg}"

            rule.relation_name = ""

    return warning_count


def build_relation_name_index(rules: List[Rule]) -> Dict[str, List[Rule]]:
    """
    Build a dictionary mapping relation names to lists of rules that have that relation name.
    Only includes rules with non-empty relation names.

    Args:
        rules: List of Rule objects with relation_name field populated

    Returns:
        Dictionary where keys are relation names and values are lists of rules
    """
    relation_index = {}

    for rule in rules:
        if rule.relation_name:  # Only include rules with non-empty relation names
            if rule.relation_name not in relation_index:
                relation_index[rule.relation_name] = []
            relation_index[rule.relation_name].append(rule)

    return relation_index


def format_case_name(case_name: Optional[str], rule_name: str, case_index: int) -> str:
    """
    Format a case name for aslspec output.
    Uses case name if available, otherwise uses rule name (if multiple rules) or case index.
    """
    if case_name:
        return case_name
    return f"case_{case_index + 1}"


def print_relation_as_aslspec(relation_name: str, rules: List[Rule], signatures: Dict[str, str]) -> str:
    """
    Convert a relation and its rules to aslspec format.

    Args:
        relation_name: Name of the relation
        rules: List of Rule objects for this relation
        signatures: Dictionary mapping relation names to their signatures from asl.spec

    Returns:
        String representation in aslspec format
    """
    lines = []

    # Use the signature from asl.spec if available, otherwise fall back to simple relation name
    if relation_name in signatures:
        signature = signatures[relation_name]
        lines.append(f"{signature} =")
    else:
        lines.append(f"relation {relation_name} =")

    # If there's only one rule, use case names for individual cases
    # If there are multiple rules, use rule names as case names
    use_rule_names_as_cases = len(rules) > 1

    # Count total valid cases across all rules
    total_valid_cases = 0
    for rule in rules:
        valid_cases = [case for case in rule.cases if not case.error and case.relation_name == relation_name]
        total_valid_cases += len(valid_cases)

    # Special case: single rule with single case - drop the case wrapper
    if len(rules) == 1 and total_valid_cases == 1:
        rule = rules[0]
        valid_cases = [case for case in rule.cases if not case.error and case.relation_name == relation_name]
        if valid_cases:
            case = valid_cases[0]
            # Add premises directly without case wrapper
            for premise in case.premises:
                lines.append(f"  {premise};")
            # Add separator
            lines.append("  --")
            # Add conclusion
            lines.append(f"  {case.conclusion};")
    else:
        # Multiple rules or multiple cases: use case wrappers
        for rule_idx, rule in enumerate(rules):
            # Only process rules that have cases without errors and match this relation
            valid_cases = [case for case in rule.cases if not case.error and case.relation_name == relation_name]

            if not valid_cases:
                continue

            if use_rule_names_as_cases:
                # Multiple rules: use rule name as case name
                if len(valid_cases) == 1:
                    # Single case in this rule
                    lines.append(f"  case {rule.name} {{")
                    case = valid_cases[0]
                    # Add premises
                    for premise in case.premises:
                        lines.append(f"    {premise};")
                    # Add separator
                    lines.append("    --")
                    # Add conclusion
                    lines.append(f"    {case.conclusion};")
                    lines.append("  }")
                else:
                    # Multiple cases in this rule - create nested structure
                    lines.append(f"  case {rule.name} {{")
                    for case_idx, case in enumerate(valid_cases):
                        case_name = format_case_name(case.name, rule.name, case_idx)
                        lines.append(f"    case {case_name} {{")

                        # Add premises
                        for premise in case.premises:
                            lines.append(f"      {premise};")
                        # Add separator
                        lines.append("      --")
                        # Add conclusion
                        lines.append(f"      {case.conclusion};")
                        lines.append("    }")

                        # Add blank line between nested cases (except after the last one)
                        if case_idx < len(valid_cases) - 1:
                            lines.append("")

                    lines.append("  }")
            else:
                # Single rule with multiple cases: group under rule name if multiple cases
                if len(valid_cases) == 1:
                    # Single case - treat as top-level case
                    case = valid_cases[0]
                    case_name = format_case_name(case.name, rule.name, 0)
                    lines.append(f"  case {case_name} {{")

                    # Add premises
                    for premise in case.premises:
                        lines.append(f"    {premise};")
                    # Add separator
                    lines.append("    --")
                    # Add conclusion
                    lines.append(f"    {case.conclusion};")

                    lines.append("  }")
                else:
                    # Multiple cases - group under rule name with nested structure
                    lines.append(f"  case {rule.name} {{")
                    for case_idx, case in enumerate(valid_cases):
                        case_name = format_case_name(case.name, rule.name, case_idx)
                        lines.append(f"    case {case_name} {{")

                        # Add premises
                        for premise in case.premises:
                            lines.append(f"      {premise};")
                        # Add separator
                        lines.append("      --")
                        # Add conclusion
                        lines.append(f"      {case.conclusion};")
                        lines.append("    }")

                        # Add blank line between nested cases (except after the last one)
                        if case_idx < len(valid_cases) - 1:
                            lines.append("")

                    lines.append("  }")

    lines.append(";")
    lines.append("")  # Empty line after each relation

    return "\n".join(lines)


def parse_asl_spec_signatures(spec_file: str) -> Dict[str, str]:
    """
    Parse asl.spec file and extract relation/function signatures.

    Args:
        spec_file: Path to the asl.spec file

    Returns:
        Dictionary mapping relation/function names to their full signatures
    """
    signatures = {}

    try:
        with open(spec_file, "r", encoding="utf-8") as f:
            content = f.read()

        # Look for patterns like:
        # [typing|semantics|] [relation|function] name(...) -> ... {
        # The signature is everything up to the opening brace

        # Use regex to find all relation/function declarations
        pattern = r'(?:^|\n)((?:typing\s+|semantics\s+)?(?:relation|function)\s+\w+\([^{]*?\)\s*(?:->\s*[^{]*?)?)\s*\{'

        matches = re.finditer(pattern, content, re.MULTILINE | re.DOTALL)

        for match in matches:
            signature = match.group(1).strip()

            # Extract the relation/function name from the signature
            # Look for pattern: [typing|semantics] [relation|function] name(
            name_match = re.search(r'(?:typing\s+|semantics\s+)?(?:relation|function)\s+(\w+)\s*\(', signature)
            if name_match:
                name = name_match.group(1)
                signatures[name] = signature

    except Exception as e:
        print(f"Warning: Error parsing asl.spec signatures: {e}")

    return signatures


def write_aslspec_output(relation_index: Dict[str, List[Rule]], output_file: str, signatures: Dict[str, str]):
    """
    Write all relations in aslspec format to the specified output file.

    Args:
        relation_index: Dictionary mapping relation names to lists of rules
        output_file: Path to output file
        signatures: Dictionary mapping relation names to their signatures from asl.spec
    """
    with open(output_file, "w", encoding="utf-8") as f:
        f.write("// ASL Specification Rules\n")
        f.write("// Generated by import_rules.py from LaTeX sources\n")
        f.write(f"// Total relations: {len(relation_index)}\n")
        f.write(f"// Total rules: {sum(len(rule_list) for rule_list in relation_index.values())}\n")
        f.write("\n")

        # Sort relations by name for consistent output
        for relation_name in sorted(relation_index.keys()):
            rules = relation_index[relation_name]
            aslspec_output = print_relation_as_aslspec(relation_name, rules, signatures)
            f.write(aslspec_output)
            f.write("\n")


def parse_rules(latex_files: List[str], show_details: bool = False) -> List[Rule]:
    """
    Parse all typing rules from the given LaTeX files.
    Returns a list of Rule objects containing rule names and their mathpar blocks.
    """
    all_rules = []

    for filename in latex_files:
        if show_details:
            print(f"Processing file: {filename}")

        # Get all rule blocks from this file
        rule_blocks = match_rules(filename)

        # Filter for typing rules only
        typing_rules = [rb for rb in rule_blocks if rb.type == RuleBlock.TYPING_RULE]

        if typing_rules and show_details:
            print(f"  Found {len(typing_rules)} typing rules")

        for rule_block in typing_rules:
            # Read the file lines
            file_lines = read_file_lines(filename)

            # Extract mathpar blocks from this rule
            mathpar_blocks = extract_mathpar_blocks(
                file_lines, rule_block.begin, rule_block.end
            )

            # Create Rule object
            rule = Rule(
                name=rule_block.name,
                mathpar_blocks=mathpar_blocks,
                cases=[],  # Will be populated by parse_cases
                filename=filename,
                rule_type=rule_block.type,
                relation_name="",  # Will be set by transform_cases
            )

            all_rules.append(rule)
            if show_details:
                print(f"    Rule '{rule.name}': {len(mathpar_blocks)} case contents")

    return all_rules


def write_parsed_rules(rules: List[Rule], output_file: str):
    """
    Write all parsed rules to the specified output file.
    """
    with open(output_file, "w", encoding="utf-8") as f:
        # Calculate error statistics
        total_cases = sum(len(rule.cases) for rule in rules)
        error_cases = sum(1 for rule in rules for case in rule.cases if case.error)

        f.write("# Parsed Typing Rules\n")
        f.write("# Generated by import_rules.py\n")
        f.write(f"# Total rules found: {len(rules)}\n")
        f.write(f"# Total cases: {total_cases}\n")
        f.write(f"# Cases with parsing errors: {error_cases}\n")
        f.write(f"# Successfully parsed cases: {total_cases - error_cases}\n\n")

        for i, rule in enumerate(rules, 1):
            # Count errors in this rule
            rule_errors = sum(1 for case in rule.cases if case.error)

            f.write(f"## Rule {i}: {rule.name}\n")
            f.write(f"File: {rule.filename}\n")
            f.write(f"Type: {rule.rule_type}\n")
            f.write(f"Relation: {rule.relation_name}\n")
            f.write(f"Case contents: {len(rule.mathpar_blocks)}\n")
            f.write(f"Cases: {len(rule.cases)}")
            if rule_errors > 0:
                f.write(f" ({rule_errors} with parsing errors)")
            f.write("\n\n")

            # Print each case, showing original case content only if there was an error
            for j, (case_content, case) in enumerate(
                zip(rule.mathpar_blocks, rule.cases), 1
            ):
                if case.error:
                    # Show original case content for error cases
                    f.write(f"### Case Content {j} (ERROR)\n")
                    f.write("```latex\n")
                    f.write(case_content)
                    f.write("\n```\n\n")

                f.write(f"### Case {j}")
                if case.name:
                    f.write(f": {case.name}")
                f.write("\n")

                if case.error:
                    f.write(f"**ERROR:** {case.error}\n\n")
                else:
                    if case.relation_name:
                        f.write(f"**Relation:** `{case.relation_name}`\n\n")

                    if case.premises:
                        f.write("**Premises:**\n")
                        for k, premise in enumerate(case.premises, 1):
                            f.write(f"{k}. `{premise}`\n")
                        f.write("\n")
                    else:
                        f.write("**Premises:** None\n\n")

                    f.write(f"**Conclusion:** `{case.conclusion}`\n\n")

            f.write("-" * 80 + "\n\n")


def load_all_macros() -> Dict[str, str]:
    """
    Load all macro definitions from the various macro files and custom replacements.
    Returns a complete dictionary of all macro mappings.
    """
    # Load macro dictionaries from variable_name_macros.tex, ASLmacros.tex, and generated_macros.tex
    doc_dir = os.path.dirname(__file__)
    variable_macros_path = os.path.join(doc_dir, "variable_name_macros.tex")
    asl_macros_path = os.path.join(doc_dir, "ASLmacros.tex")
    generated_macros_path = os.path.join(doc_dir, "generated_macros.tex")
    macro_dict = load_macro_dictionary(
        variable_macros_path, asl_macros_path, generated_macros_path
    )

    # Add custom macro replacements
    custom_macros = get_custom_macro_replacements()
    macro_dict.update(custom_macros)

    if args.details:
        print(
            f"Loaded {len(macro_dict)} macro definitions (including {len(custom_macros)} custom replacements)"
        )
    return macro_dict


def main():
    global MACRO_DICT
    MACRO_DICT = load_all_macros()

    if args.details:
        print("Starting typing rule extraction...")

    # Get all LaTeX source files (content files only, not generated)
    try:
        latex_files = get_latex_sources(True)  # True for content files only
        if args.details:
            print(f"Found {len(latex_files)} LaTeX files to process")
    except Exception as e:
        print(f"Error getting LaTeX sources: {e}")
        sys.exit(1)

    # Parse all typing rules
    rules = parse_rules(latex_files, args.details)

    # Parse individual inference rules (cases) from mathpar blocks
    if args.details:
        print("\nParsing cases from mathpar blocks...")
    successful_count, error_count = parse_cases(rules)

    # Transform all cases from LaTeX to aslspec format
    if args.details:
        print("Transforming cases from LaTeX to aslspec format...")
    warning_count = transform_cases(rules)

    # Build relation name index
    if args.details:
        print("Building relation name index...")
    relation_index = build_relation_name_index(rules)

    # Calculate final error statistics (after transformation)
    total_cases = sum(len(rule.cases) for rule in rules)
    error_cases = sum(1 for rule in rules for case in rule.cases if case.error and not ("WARNING:" in case.error and not any(keyword in case.error for keyword in ["ERROR:", "Failed to parse", "Unable to parse case"])))
    successful_cases = total_cases - error_cases

    print(f"Total typing rules found: {len(rules)}")
    print(f"Total cases parsed: {total_cases}")
    print(f"Cases with parsing errors: {error_cases}")
    print(f"Successfully parsed cases: {successful_cases}")
    print(f"Cases with warnings: {warning_count}")
    print(f"Unique relation names: {len(relation_index)}")
    print(f"Rules with relation names: {sum(len(rule_list) for rule_list in relation_index.values())}")
    if not rules:
        print("No typing rules found!")
        return

    # Write results to output file
    if args.details:
        output_file = "import_rules_log.md"
        write_parsed_rules(rules, output_file)

    # Parse asl.spec signatures
    if args.details:
        print("Parsing asl.spec signatures...")
    asl_spec_path = os.path.join(os.path.dirname(__file__), "asl.spec")
    signatures = parse_asl_spec_signatures(asl_spec_path)
    if args.details:
        print(f"Found {len(signatures)} signatures in asl.spec")

    # Write aslspec format output
    rules_spec_file = "rules.spec"
    if args.details:
        print("Writing aslspec format output...")
    write_aslspec_output(relation_index, rules_spec_file, signatures)
    print(f"spec rules written to: {rules_spec_file}")

    if args.details:
        print(f"Parsed rules written to: {output_file}")
        print(f"ASLspec format also written to: {rules_spec_file}")
        print("\nRelation name distribution:")
        for relation_name, rule_list in sorted(relation_index.items()):
            print(f"  {relation_name}: {len(rule_list)} rules")
        print("\nSummary:")
        for rule in rules:
            print(
                f"  {rule.name}: {len(rule.mathpar_blocks)} case contents, {len(rule.cases)} cases ({rule.filename})"
            )


if __name__ == "__main__":
    main()
