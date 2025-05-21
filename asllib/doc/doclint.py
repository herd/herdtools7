#!/usr/bin/python3

import os, sys, fnmatch
from extended_macros import apply_all_macros, get_latex_sources
import re
from dataclasses import dataclass
from typing import List, Set
import argparse
import pathlib
import pathlib

cli_parser = argparse.ArgumentParser(prog="ASL Reference Linter")
cli_parser.add_argument(
    "-m",
    "--macros",
    help="Rewrites *.tex files with extended macros",
    action="store_true",
)
cli_parser.add_argument(
    "-d",
    "--dictionary",
    help="Specifies reference dictionary file for spellchecking",
    metavar="<path-to-dictionary-file>",
    type=pathlib.Path,
)

INTERNAL_DICTIONARY_FILENAME = "dictionary.txt"
DO_NOT_LINT_STR = "DO NOT LINT"


def read_file_lines(filename: str) -> List[str]:
    with open(filename, "r", encoding="utf-8") as file:
        return file.readlines()


def read_file_str(filename: str) -> List[str]:
    with open(filename, "r", encoding="utf-8") as file:
        return file.read()


def extract_labels_from_line(line: str, left_delim: str, labels: set[str]):
    r"""
    Adds all labels found in `line` into `labels`. A label starts with the
    sub-string given by `left_delim` and ends with the substring `}`.
    """
    right_delim = "}"
    label_pos: int = 0
    while True:
        if DO_NOT_LINT_STR in line:
            return
        label_pos: int = line.find(left_delim, label_pos)
        if label_pos == -1:
            return
        right_brace_pos: int = line.find(right_delim, label_pos)
        if right_brace_pos == -1:
            return
        label = line[label_pos + len(left_delim) : right_brace_pos]
        labels.add(label)
        label_pos = right_brace_pos + 1

def check_unused_latex_macros(latex_files: list[str]):
    r"""
    Checks whether there are LaTeX macros that are defined but never used.
    """
    defined_macros: set[str] = set()
    used_macros: set[str] = set()
    macro_def_pattern = re.compile(r"\\newcommand(\\[a-zA-Z]*)\[")
    macro_use_pattern = re.compile(r"(?<!\\newcommand)\\[a-zA-Z]+")
    num_errors = 0
    for latex_source in latex_files:
        source_str = read_file_str(latex_source)
        for def_match in re.findall(macro_def_pattern, source_str):
            # print(f"found macro definition {def_match}")
            defined_macros.add(def_match)
        for use_match in re.findall(macro_use_pattern, source_str):
            # print(f"found macro usage {use_match}")
            used_macros.add(use_match)
    unused_macros = defined_macros.difference(used_macros)
    for unused in unused_macros:
        print(f"LaTeX macro {unused} is defined but never used!")
        num_errors += 1
    return num_errors


def check_hyperlinks_and_hypertargets(latex_files: list[str]):
    r"""
    Checks whether all labels defined in `\hyperlink` definitions match
    labels defined in `\hypertarget` definitions, print the mismatches
    to the console.
    """
    hyperlink_labels: set[str] = set()
    hypertarget_labels: set[str] = set()
    for latex_source in latex_files:
        for line in read_file_lines(latex_source):
            extract_labels_from_line(line, "\\hyperlink{", hyperlink_labels)
            extract_labels_from_line(line, "\\hypertarget{", hypertarget_labels)
    num_errors = 0
    missing_hypertargets = hyperlink_labels.difference(hypertarget_labels)
    if missing_hypertargets:
        num_missing_hypertargets = len(missing_hypertargets)
        num_errors += num_missing_hypertargets
        print(
            f"ERROR: found {num_missing_hypertargets} hyperlinks without matching hypertargets: ",
            file=sys.stderr,
        )
        for label in missing_hypertargets:
            print(label, file=sys.stderr)

    missing_hyperlinks = hypertarget_labels.difference(hyperlink_labels)
    if not missing_hyperlinks == set():
        num_missing_hyperlinks = len(missing_hyperlinks)
        num_errors += num_missing_hyperlinks
        print(
            f"ERROR: found {num_missing_hyperlinks} hypertargets without\
               matching hyperlinks: ",
            file=sys.stderr,
        )
        for label in missing_hyperlinks:
            print(label, file=sys.stderr)

    return num_errors


def check_undefined_references_and_multiply_defined_labels():
    r"""
    Checks whether the LaTeX compiler found any undefined labels
    or multiply-defined labels.
    """
    num_errors = 0
    log_str = read_file_str("./ASLReference.log")
    if "LaTeX Warning: There were undefined references." in log_str:
        print(
            f"ERROR: There are undefined references (see ./ASLReference.log)",
            file=sys.stderr,
        )
        num_errors += 1
    if "LaTeX Warning: There were multiply-defined labels." in log_str:
        print(
            f"ERROR: There are multiply-defined labels (see ./ASLReference.log)",
            file=sys.stderr,
        )
        num_errors += 1
    if "destination with the same identifier" in log_str:
        print(
            f"ERROR: There are multiply-defined \\hypertarget labels \
                (see 'destination with the same identifier' in \
                ./ASLReference.log)",
            file=sys.stderr,
        )
        num_errors += 1
    return num_errors


def check_tododefines(latex_files: list[str]):
    r"""
    Checks that there are no more than the expected number of \tododefine
    instances.
    """
    MAX_TODODEFINE_INSTANCES = 6
    num_todo_define = 0
    for latex_source in latex_files:
        lines = read_file_lines(latex_source)
        for line in lines:
            if DO_NOT_LINT_STR in line:
                continue
            num_todo_define += line.count("\\tododefine")
    if num_todo_define > MAX_TODODEFINE_INSTANCES:
        # Disallow adding new \tododefines
        print(
            f"ERROR: There are {num_todo_define} occurrences of \\tododefine,\
               expected at most {MAX_TODODEFINE_INSTANCES}"
        )
        return num_todo_define
    else:
        print(f"WARNING: There are {num_todo_define} occurrences of \\tododefine")
        return 0

def check_repeated_lines(filename: str) -> int:
    r"""
    Checks whether `file` contains the same line appearing twice in a row.
    The exception is inside `CONSOLE_BEGIN...CONSOLE_END` blocks.
    Errors are reported for the file name 'filename' and the total
    number of found errors is returned.
    """
    num_errors = 0
    line_number = 0
    last_line = ""
    inside_console_outout = False
    for line in read_file_lines(filename):
        line_number += 1
        if r"CONSOLE_BEGIN" in line:
            inside_console_outout = True
        if r"CONSOLE_END" in line:
            inside_console_outout = False
        if not inside_console_outout and line and line == last_line and line.strip() and line.strip() != "}":
            print(f"./{filename} line {line_number}: repeated twice")
        last_line = line
    return num_errors

def check_repeated_words(filename: str) -> int:
    r"""
    Checks if 'file' contains occurrences of the same word
    repeated twice, independent of case. For example, "the the".
    Errors are reported for the file name 'filename' and the total
    number of found errors is returned.
    """
    num_errors = 0
    line_number = 0
    last_token = ""
    for line in read_file_lines(filename):
        line_number += 1
        line = line.strip()
        tokens = re.split(" |{|}", line)
        for current_token in tokens:
            current_token_lower = current_token.lower()
            last_token_lower = last_token.lower()
            if (
                current_token_lower.isalpha()
                and last_token_lower == current_token_lower
            ):
                num_errors += 1
                print(
                    f"./{filename} line {line_number}: \
                        word repetition ({last_token} {current_token}) in '{line}'"
                )
            last_token = current_token
    return num_errors


def detect_incorrect_latex_macros_spacing(filename: str) -> int:
    r"""
    Detects erroneous occurrences of LaTeX macros rendered without
    separation from the next word in 'file'.
    Errors are reported for the file name 'filename' and the total
    number of found errors is returned.
    """
    num_errors = 0
    file_str = read_file_str(filename)
    double_backslash_matches = re.findall(r"\\\\[a-zA-Z]+", file_str)
    for match in double_backslash_matches:
        print(f"./{filename}: double \\ in macro {match}")
        num_errors += 1
    for match in re.findall(r"\\overname\{\}\{.+?\}", file_str):
        print(f"./{filename}: empty \\overname: {match}")
        num_errors += 1
    for match in re.findall(r"\\overname\{.+?\}\{\}", file_str):
        print(f"./{filename}: empty \\overname label: {match}")
        num_errors += 1

    patterns_to_remove = [
        # Patterns for known math environments:
        r"\$.*?\$",  # $...$
        r"\\\[.*?\\\]",  # \[...\]
        r"\\begin\{mathpar\}.*?\\end\{mathpar\}",  # \begin{mathpar}...\end{mathpar}
        r"\\begin\{equation\}.*?\\end\{equation\}",  # \begin{equation}...\end{equation}
        r"\\begin\{flalign\*\}.*?\\end\{flalign\*\}",  # \begin{flalign*}...\end{flalign*}
        # Macros intended to be preceded by a space character in text mode:
        r"\\item",  # \item occurrences
        r"\\noindent",  # \noindent occurrences
        r"\\tt",  # \tt occurrences
    ]
    for pattern in patterns_to_remove:
        file_str = re.sub(pattern, "", file_str, flags=re.DOTALL)

    # Look for things like \macro word
    macro_followed_by_space = r"\\[a-zA-Z]+(?= )"
    matches = re.findall(macro_followed_by_space, file_str)
    for match in matches:
        print(f'{filename}: LaTeX macro followed by space "{match }"')
        num_errors += 1
    # Look for things like \macro{}word
    macro_followed_by_space = r"\\[a-zA-Z]+{}[a-zA-Z]"
    matches = re.findall(macro_followed_by_space, file_str)
    for match in matches:
        print(f'{filename}: LaTeX macro running into next word "{match}"')
        num_errors += 1
    return num_errors


def check_teletype_in_rule_math_mode(filename) -> int:
    r"""
    Checks that file does not contain \texttt{...} inside math mode
    in rule definitions.
    Otherwise, the errors are reported for 'filename' and the total
    number of errors is returned.
    """
    if filename == "LexicalStructure.tex":
        return 0
    file_str: str = read_file_str(filename)
    num_errors = 0
    rule_math_mode_patterns = [
        r"\\\[.*?\\\]",  # \[...\]
        r"\\begin\{mathpar\}.*?\\end\{mathpar\}",
        r"\\begin\{equation\}.*?\\end\{equation\}",
    ]
    math_modes_exprs = "|".join(rule_math_mode_patterns)
    math_mode_pattern = re.compile(math_modes_exprs, re.DOTALL)
    teletype_pattern = re.compile(r"\\texttt{.*}")
    matches = math_mode_pattern.findall(file_str)
    for match in matches:
        if not "SUPPRESS_TEXTTT_LINTER" in match:
            teletype_vars = teletype_pattern.findall(match)
            for tt_var in teletype_vars:
                print(
                    f"ERROR! {filename}: teletype font not allowed in rules, substitute {tt_var} with a proper macro"
                )
                num_errors += 1
    return num_errors


class RuleBlock:
    r"""
    A class for capturing the lines that a rule consists of,
    the rule type, and the rule name.
    """

    AST_RULE = "AST"
    TYPING_RULE = "Typing"
    SEMANTICS_RULE = "Semantics"
    GUIDE_RULE = "Guide"
    CONVENTION_RULE = "Convention"
    GUIDE_RULE = "Guide"
    CONVENTION_RULE = "Convention"

    rule_begin_pattern = re.compile(
        r"\\(TypingRuleDef|SemanticsRuleDef|ASTRuleDef|ConventionDef|RequirementDef){(.*?)}"
    )
    end_patterns = [
        r"\\section{.*}",
        r"\\subsection{.*}",
        r"\\TypingRuleDef{.*}",
        r"\\SemanticsRuleDef{.*}",
        r"\\ASTRuleDef{.*}",
        r"\\ConventionDef{.*}",
        r"\\RequirementDef{.*}",
    ]
    rule_end_pattern = re.compile("|".join(end_patterns))

    def __init__(self, filename, file_lines: list[str], begin: int, end: int):
        self.filename: str = filename
        self.file_lines: list[str] = file_lines
        self.begin: int = begin
        self.end: int = end

        begin_line = file_lines[begin]
        name_match = re.match(RuleBlock.rule_begin_pattern, begin_line)
        if not name_match:
            self.name = None
            raise ValueError(begin_line)
        else:
            self.name = name_match.group(2)

        if re.search(r"\\ASTRuleDef", begin_line):
            self.type = RuleBlock.AST_RULE
        elif re.search(r"\\TypingRuleDef", begin_line):
            self.type = RuleBlock.TYPING_RULE
        elif re.search(r"\\SemanticsRuleDef", begin_line):
            self.type = RuleBlock.SEMANTICS_RULE
        elif re.search(r"\\RequirementDef", begin_line):
            self.type = RuleBlock.GUIDE_RULE
        elif re.search(r"\\ConventionDef", begin_line):
            self.type = RuleBlock.CONVENTION_RULE
        else:
            self.type = None

    def str(self):
        return f"lines {self.begin+1}-{self.end+1} {self.type}.{self.name}"


def match_rules(filename: str) -> List[RuleBlock]:
    r"""
    Parses 'filename' and returns all AST/Typing/Semantics rules.
    """
    rule_blocks = []
    lines = read_file_lines(filename)
    # The beginning line of a rule block, or -1 meaning outside of a rule.
    current_rule_begin = -1
    for line_number, line in enumerate(lines):
        if RuleBlock.rule_end_pattern.search(line) or line_number == len(lines) - 1:
            if current_rule_begin != -1:
                rule_block = RuleBlock(filename, lines, current_rule_begin, line_number)
                rule_blocks.append(rule_block)
                current_rule_begin = -1
        if RuleBlock.rule_begin_pattern.search(line):
            current_rule_begin = line_number
    return rule_blocks


def check_rule_prose_formally_structure(rule_block: RuleBlock) -> List[str]:
    r"""
    Checks that a rule block contains a single Prose paragraphs followed
    by a single Formally paragraph, returning a list of error messages
    for all errors found.
    """
    if not rule_block.type in [RuleBlock.TYPING_RULE, RuleBlock.SEMANTICS_RULE]:
        return []
    num_prose_paragraphs = 0
    num_formally_paragraphs = 0
    block_errors: List[str] = []
    for line_number in range(rule_block.begin, rule_block.end + 1):
        line = rule_block.file_lines[line_number].strip()
        if line.startswith("%"):
            continue
        if re.search(r"\\ProseParagraph", line):
            num_prose_paragraphs += 1
            if num_formally_paragraphs > 0:
                block_errors.append(
                    "encountered a Formally paragraph before Prose paragraph"
                )
        if re.search(r"\\FormallyParagraph", line):
            num_formally_paragraphs += 1
            if num_formally_paragraphs == 0:
                block_errors.append(
                    "encountered a Prose paragraph before Formally paragraph"
                )
    if num_prose_paragraphs == 0:
        block_errors.append("missing a Prose paragraph")
    if num_prose_paragraphs > 1:
        block_errors.append("encountered more than one Prose paragraph")
    if num_formally_paragraphs == 0:
        block_errors.append("missing a Formally paragraph")
    if num_formally_paragraphs > 1:
        block_errors.append("encountered more than one Formally paragraph")
    return block_errors


def check_rule_case_consistency(rule_block: RuleBlock) -> List[str]:
    r"""
    Checks that the rule cases appearing in the Prose paragraph and Formally
    paragraph are equal and each paragraph does not contain duplicate cases.
    """
    if not rule_block.type in [RuleBlock.TYPING_RULE, RuleBlock.SEMANTICS_RULE]:
        return []

    prose_cases: Set[str] = set()
    formally_cases: Set[str] = set()
    prose_cases_pattern = re.compile(r".*\\AllApplyCase{(.*?)}")
    formally_cases_pattern = re.compile(r".*\\inferrule\[(.*?)\]")
    error_messages: List[str] = []
    for line_number in range(rule_block.begin, rule_block.end + 1):
        line = rule_block.file_lines[line_number].strip()
        if line.startswith("%"):
            continue
        formally_matches = re.match(formally_cases_pattern, line)
        if formally_matches:
            case_names = formally_matches.group(1).split(",")
            for case_name in case_names:
                case_name = case_name.strip()
                if case_name in formally_cases:
                    error_messages.append(
                        f'Case "{case_name}" duplicate in Formally paragraph'
                    )
                formally_cases.add(case_name)
        prose_matches = re.match(prose_cases_pattern, line)
        if prose_matches:
            matched_case_names = prose_matches.group(1)
            case_names = matched_case_names.split(",")
            for case_name in case_names:
                case_name = case_name.strip()
                if case_name in prose_cases:
                    error_messages.append(
                        f'Case "{case_name}" duplicate in Prose paragraph'
                    )
                prose_cases.add(case_name)
    cases_only_in_prose = prose_cases.difference(formally_cases)
    cases_only_in_formally = formally_cases.difference(prose_cases)
    if cases_only_in_prose:
        error_messages.append(f"cases only in Prose paragraph: {cases_only_in_prose}")
    if cases_only_in_formally:
        error_messages.append(
            f"cases only in Formally paragraph: {cases_only_in_formally}"
        )

    return error_messages


def check_rule_has_example(rule_block: RuleBlock) -> List[str]:
    r"""
    Every typing/semantics/convention/guide rule should provide
    or reference at least one example.
    The return value is a list of error message --- one per rule
    that does not have an example.
    """
    if not rule_block.type in [
        RuleBlock.TYPING_RULE,
        RuleBlock.SEMANTICS_RULE,
        RuleBlock.GUIDE_RULE,
        RuleBlock.CONVENTION_RULE,
    ]:
        return []
    example_found = False
    for line_number in range(rule_block.begin, rule_block.end + 1):
        line = rule_block.file_lines[line_number].strip()
        if (
            line.startswith("\\ExampleDef")
            or "\\ExampleRef" in line
            or "\\subsubsection{Example}" in line
            or "\\listingref" in line
            or "% INLINED_EXAMPLE" in line
            or "% NO_EXAMPLE" in line
        ):
            example_found = True
            break
    error_messages = []
    if not example_found:
        error_messages.append("missing an example")
    return error_messages


def check_rules(filename: str) -> int:
    r"""
    Checks the AST/Typing/Semantics/Guide/Convention rules in 'filename'
    and returns the total number of errors.
    """
    # Treat existing issues as warnings and new issues as errors.
    file_to_num_expected_errors = {
        "RelationsOnTypes.tex" : 15,
        "SubprogramCalls.tex" : 1,
        "SymbolicSubsumptionTesting.tex" : 15,
        "SemanticsUtilities.tex" : 19,
    }
    total_expected = 0
    for num_expected in file_to_num_expected_errors.values():
        total_expected += num_expected
    if False and total_expected > 0:
        print(f"#expected errors is {total_expected}")

    max_error_for_filename = file_to_num_expected_errors.get(filename, 0)

    checks = [
        check_rule_prose_formally_structure,
        check_rule_case_consistency,
        check_rule_has_example,
    ]
    num_errors = 0
    rule_blocks: List[RuleBlock] = match_rules(filename)
    for rule_block in rule_blocks:
        if not rule_block:
            print(f"{filename} {rule_block.str()}: unable to determine rule type")
            num_errors += 1
            continue
        error_messages: List[str] = []
        for check in checks:
            error_messages.extend(check(rule_block))
        if error_messages:
            error_messages_str = ", ".join(error_messages)
            num_errors += len(error_messages)
            if num_errors > max_error_for_filename:
                print(f"ERROR! {rule_block.filename} {rule_block.str()}: {error_messages_str}")
            else:
                print(f"WARNING! {rule_block.filename} {rule_block.str()}: {error_messages_str}")

    if num_errors > max_error_for_filename:
        return num_errors
    else:
        if num_errors < max_error_for_filename:
            print(f"WARNING! {rule_block.filename}: update number of expected errors to {num_errors}")
        return 0


def spellcheck(reference_dictionary_path: str, latex_files: list[str]) -> int:
    r"""
    Attempts to find spelling error in the files listed in 'latex_files'
    by consulting the internal dictionary file INTERNAL_DICTIONARY_FILENAME
    and an optional reference dictionary in 'reference_dictionary_path'.
    """
    dict_word_list = read_file_str(INTERNAL_DICTIONARY_FILENAME).splitlines()
    dict_words = set(dict_word_list)
    reference_words: set[str] = set()
    if reference_dictionary_path:
        reference_word_list = read_file_str(reference_dictionary_path).splitlines()
        reference_words = set(reference_word_list)

    patterns_to_remove = [
        # Patterns for environments inside which spellchecking is not needed:
        r"\$.*?\$",
        r"\\\[.*?\\\]",
        r"\\begin\{mathpar\}.*?\\end\{mathpar\}",
        r"\\begin\{equation\}.*?\\end\{equation\}",
        r"\\begin\{flalign\*\}.*?\\end\{flalign\*\}",
        r"\\begin{table}.*?\\end{table}",
        r"\\begin{lstlisting}.*?\\end{lstlisting}",
        r"\\begin\{Verbatim\}.*?\\end\{Verbatim\}",
        r"\\begin\{verbatim\}.*?\\end\{verbatim\}",
        r"\\begin{tabular}.*?\\end{tabular}",
        r"subsubsection",
        r"\\verb\|.*?\|",
        r"\\lrmcomment{.*?}",
        r"\\stdlibfunc{.*?}",
        r"\\defref{.*?}",
        r"\\LexicalRuleDef{.*?}",
        r"\\LexicalRuleRef{.*?}",
        r"\\ASTRuleRef{.*?}",
        r"\\ASTRuleDef{.*?}",
        r"\\TypingRuleRef{.*?}",
        r"\\TypingRuleDef{.*?}",
        r"\\SemanticsRuleRef{.*?}",
        r"\\SemanticsRuleDef{.*?}",
        r"\\RequirementDef{.*?}",
        r"\\RequirementRef{.*?}",
        r"\\ConventionDef{.*?}",
        r"\\AllApplyCase{.*?}",
        r"\% CONSOLE_BEGIN.*\% CONSOLE_END",
        r"\\hypertarget{.*?}",
        r"\\href{.*?}",
        r"\\begin{.*?}",
        r"\\end{.*?}",
        r"\\secref{.*?}",
        r"\\chapref{.*?}",
        r"\\taref{.*?}",
        r"\\cite{.*?}",
        r"\\cite\[.*?\]{.*?}",
        r"\\identi{.*?}",
        r"\\identd{.*?}",
        r"\\identg{.*?}",
        r"\\identr{.*?}",
        r"\\textit{.*?}",
        r"\\texttt{.*?}",
        r"\\listingref{.*?}",
        r"\\appendixref{.*?}",
    ]
    asl_listing_pattern = r"\\ASLListing\{(.*?)\}\{.*?\}\{.*?\}"

    num_errors = 0
    for filename in latex_files:
        file_str: str = read_file_str(filename)
        file_lines = file_str.splitlines()
        # Hash lines to line numbers in an attempt to later match them.
        line_to_line_number = dict()
        for line_number, line in enumerate(file_lines, start=1):
            line_to_line_number[line.strip()] = line_number
        # Remove text blocks where spelling is not needed.
        for pattern in patterns_to_remove:
            file_str = re.sub(pattern, "", file_str, flags=re.DOTALL)
        # Replace instances of \ASLListing with just the caption.
        file_str = re.sub(asl_listing_pattern, r"\1", file_str)
        file_lines = file_str.splitlines()
        for line in file_lines:
            tokens = re.split(" |{|}", line)
            tokens = [token.lower() for token in tokens if token.isalpha()]
            for token in tokens:
                token_in_dict = token in dict_words
                token_in_reference = token in reference_words
                if not token_in_dict:
                    if token_in_reference:
                        dict_words.add(token)
                        print(f"adding new word to internal dictionary: {token}")
                    else:
                        num_errors += 1
                        if line.strip() in line_to_line_number:
                            line_number = line_to_line_number[line.strip()]
                            print(
                                f'{filename} line {line_number}: "{token}" may be misspelled in "{line}"'
                            )
                        else:
                            print(
                                f'{filename}: "{token}" may be misspelled in "{line}"'
                            )

    # Update the internal dictionary filename, if it has changed.
    sorted_words = sorted(list([word.lower() for word in dict_words]))
    if sorted_words != dict_word_list:
        with open(INTERNAL_DICTIONARY_FILENAME, "w", encoding="utf-8") as file:
            file_str = "\n".join(sorted_words) + "\n"
            file_str = "\n".join(sorted_words) + "\n"
            print(f"Updating dictionary file {INTERNAL_DICTIONARY_FILENAME}")
            file.write(file_str)

    return num_errors


def check_per_file(latex_files: list[str], checks):
    r"""
    Applies the list of functions in 'checks' to each file in 'latex files',
    accumulating the number of errors and returning the total number of errors
    across all files.
    """
    num_errors = 0
    for filename in latex_files:
        for check in checks:
            num_errors += check(filename)
    return num_errors


def main():
    args = cli_parser.parse_args()
    if args.macros:
        apply_all_macros()
    print("Linting files...")
    all_latex_sources = get_latex_sources(False)
    content_latex_sources = get_latex_sources(True)
    num_errors = 0
    num_spelling_errors = spellcheck(args.dictionary, content_latex_sources)
    if num_spelling_errors > 0:
        print(
            "There were possible spelling errors. "
            "Please either fix them or add new words to "
            f"{INTERNAL_DICTIONARY_FILENAME}."
        )
    num_errors += num_spelling_errors
    num_errors += check_hyperlinks_and_hypertargets(all_latex_sources)
    num_errors += check_undefined_references_and_multiply_defined_labels()
    num_errors += check_tododefines(content_latex_sources)
    num_errors += check_unused_latex_macros(all_latex_sources)
    num_errors += check_per_file(
        content_latex_sources,
        [
            check_repeated_words,
            check_repeated_lines,
            detect_incorrect_latex_macros_spacing,
            check_teletype_in_rule_math_mode,
            check_rules,
        ],
    )

    if num_errors > 0:
        print(f"There were {num_errors} errors!", file=sys.stderr)
        sys.exit(1)


if __name__ == "__main__":
    main()
