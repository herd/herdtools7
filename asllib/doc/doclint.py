#!/usr/bin/python3

import sys
from extended_macros import (
    apply_console_macros,
    get_latex_sources,
    read_file_lines,
    read_file_str,
)
import re
from dataclasses import dataclass
from typing import List, Set
import argparse
import pathlib
import os, fnmatch

cli_parser = argparse.ArgumentParser(prog="ASL Reference Linter")
cli_parser.add_argument(
    "-cm",
    "--console_macros",
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
cli_parser.add_argument(
    "--aslref",
    help="Specifies path to the aslref executable",
    metavar="<path-to-aslref>",
    type=str,
)

INTERNAL_DICTIONARY_FILENAME = "dictionary.txt"
DO_NOT_LINT_STR = "DO NOT LINT"
GENERATED_ELEMENTS_FILENAME = "generated_elements.tex"


def is_skipped_line(line: str):
    return DO_NOT_LINT_STR in line or line.strip().startswith("%")


def is_content_line(line: str):
    return not is_skipped_line(line)


def extract_labels_from_line(line: str, left_delim: str, labels: set[str]):
    r"""
    Adds all labels found in `line` into `labels`. A label starts with the
    sub-string given by `left_delim` and ends with the substring `}`.
    """
    right_delim = "}"
    label_pos: int = 0
    while True:
        if is_skipped_line(line):
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
        lines = read_file_lines(latex_source)
        lines = [line for line in lines if "DO NOT LINT" not in line]
        source_str = "\n".join(lines)
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
    # Labels to exclude from the check
    excluded_labels: set[str] = {"constant-one"}

    hyperlink_labels: set[str] = set()
    hypertarget_labels: set[str] = set()
    for latex_source in latex_files:
        for line in read_file_lines(latex_source):
            extract_labels_from_line(line, "\\hyperlink{", hyperlink_labels)
            extract_labels_from_line(line, "\\hypertarget{", hypertarget_labels)
            extract_labels_from_line(line, "\\mathhypertarget{", hypertarget_labels)
            extract_labels_from_line(line, "\\texthypertarget{", hypertarget_labels)

    # Remove excluded labels from both sets
    hyperlink_labels -= excluded_labels
    hypertarget_labels -= excluded_labels

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
            f"ERROR: found {num_missing_hyperlinks} hypertargets without matching hyperlinks: ",
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
    log_filepath = "./ASLReference.log"
    try:
        log_str = read_file_str(log_filepath)
        if "LaTeX Warning: There were undefined references." in log_str:
            print(
                f"ERROR: There are undefined references (see {log_filepath})",
                file=sys.stderr,
            )
            num_errors += 1
        if "LaTeX Warning: There were multiply-defined labels." in log_str:
            print(
                f"ERROR: There are multiply-defined labels (see {log_filepath})",
                file=sys.stderr,
            )
            num_errors += 1
        if "has been referenced but does not exist" in log_str:
            print(
                f"ERROR: A reference is made to a non-existent label (see {log_filepath})",
                file=sys.stderr,
            )
            num_errors += 1
        if "destination with the same identifier" in log_str:
            print(
                f"ERROR: There are multiply-defined \\hypertarget labels \
                    (see 'destination with the same identifier' in \
                    {log_filepath})",
                file=sys.stderr,
            )
            num_errors += 1
        # There are 3 known instances of "Warning", which are considered benign.
        # Any others, that have not been detected earlier, thereby increasing
        # `num_errors` are caught here.
        if (
            num_errors == 0
            and len(re.findall(r"warning", log_str, flags=re.IGNORECASE)) > 3
        ):
            print(
                f"ERROR: There are unrecognized instances of 'warning' in {log_filepath})",
                file=sys.stderr,
            )
            num_errors += 1
        return num_errors
    except FileNotFoundError:
        print(f"Unable to open {log_filepath}", file=sys.stderr)
        return 1


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
        if (
            not inside_console_outout
            and line
            and line == last_line
            and line.strip()
            and line.strip() != "}"
        ):
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


class RuleBlock:
    r"""
    A class for capturing the lines that a rule consists of,
    the rule type, and the rule name.
    """

    AST_RULE = "AST"
    TYPING_RULE = "Typing"
    SEMANTICS_RULE = "Semantics"
    GUIDE_RULE = "Guide"
    SYNTACTIC_SUGAR_RULE = "SyntacticSugar"
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
        elif re.search(r"\\SyntacticSugarDef", begin_line):
            self.type = RuleBlock.SYNTACTIC_SUGAR_RULE
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
    rule_must_have_prose_formally_paragraphs = rule_block.type in [
        RuleBlock.TYPING_RULE,
        RuleBlock.SEMANTICS_RULE,
    ]
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
    if rule_must_have_prose_formally_paragraphs and num_prose_paragraphs == 0:
        block_errors.append("missing a Prose paragraph")
    if num_prose_paragraphs > 1:
        block_errors.append("encountered more than one Prose paragraph")
    if rule_must_have_prose_formally_paragraphs and num_formally_paragraphs == 0:
        block_errors.append("missing a Formally paragraph")
    if num_formally_paragraphs > 1:
        block_errors.append("encountered more than one Formally paragraph")
    return block_errors


def check_rule_case_consistency(rule_block: RuleBlock) -> List[str]:
    r"""
    Checks that the rule cases appearing in the Prose paragraph and Formally
    paragraph are equal and each paragraph does not contain duplicate cases.
    """
    # Skip check if rule contains \RenderRule, as cases appear in generated_macros.tex
    # rather than in the prose text
    for line_number in range(rule_block.begin, rule_block.end + 1):
        line = rule_block.file_lines[line_number].strip()
        if r"\RenderRule" in line:
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
    # AST rules can have inference rules but no prose paragraph.
    must_have_prose_paragraph = rule_block.type in [
        RuleBlock.TYPING_RULE,
        RuleBlock.SEMANTICS_RULE,
    ]
    if must_have_prose_paragraph and cases_only_in_formally:
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
        RuleBlock.SYNTACTIC_SUGAR_RULE,
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
            print(
                f"ERROR! {rule_block.filename} {rule_block.str()}: {error_messages_str}"
            )

    return num_errors


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
        r"\\ASTRuleCaseRef{.*?}{.*?}",
        r"\\ASTRuleDef{.*?}",
        r"\\TypingRuleRef{.*?}",
        r"\\TypingRuleDef{.*?}",
        r"\\SemanticsRuleRef{.*?}",
        r"\\SemanticsRuleDef{.*?}",
        r"\\RequirementDef{.*?}",
        r"\\RequirementRef{.*?}",
        r"\\SyntacticSugarDef{.*?}",
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
        r"\\RenderConstant{.*?}",
        r"\\RenderConstant\[.*?\]{.*?}",
        r"\\RenderType{.*?}",
        r"\\RenderType\[.*?\]{.*?}",
        r"\\RenderRelation{.*?}",
        r"\\RenderRelation\[.*?\]{.*?}",
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


def check_mathpar_macro_usage(filename: str) -> int:
    r"""
    Scans a .tex file and checks that within \begin{mathpar} ... \end{mathpar} blocks,
    there are no macros ending with 'term', 'Term', or starting with 'Prose'.
    Returns the number of errors found.
    """
    num_errors = 0

    # List of macros that are excluded from this check
    excluded_macros = {
        r'\polynomialdividebyterm',
    }

    # Pattern to match macros that end with 'term' or 'Term', or start with 'Prose'
    prohibited_macro_pattern = re.compile(r'\\([a-zA-Z]+(?:term|Term)|Prose[a-zA-Z]+)')

    file_str = read_file_str(filename)

    # Find all mathpar blocks
    mathpar_pattern = re.compile(r'\\begin\{mathpar\}(.*?)\\end\{mathpar\}', re.DOTALL)

    for match in mathpar_pattern.finditer(file_str):
        mathpar_content = match.group(1)
        start_pos = match.start()

        # Calculate approximate line number for error reporting
        lines_before = file_str[:start_pos].count('\n')
        line_number = lines_before + 1

        # Find prohibited macros in this mathpar block
        for macro_match in prohibited_macro_pattern.finditer(mathpar_content):
            macro_name = macro_match.group(0)

            # Skip if this macro is in the excluded list
            if macro_name in excluded_macros:
                continue

            # Calculate line within the mathpar block
            mathpar_lines_before = mathpar_content[:macro_match.start()].count('\n')
            error_line = line_number + mathpar_lines_before

            print(
                f"{filename}:{error_line}: Prohibited macro {macro_name} found in mathpar block"
            )
            num_errors += 1

    return num_errors


def check_balanced_parentheses_in_math(filename: str) -> int:
    r"""
    Checks that all mathematical environments ($...$, \[...\], \begin{mathpar}...\end{mathpar})
    have balanced parentheses of the following types:
    - ( and )
    - [ and ]
    - { and }
    - \left{ and \right}
    - \left{ and \right.
    Returns the number of errors found.
    """
    num_errors = 0
    file_str = read_file_str(filename)

    # Patterns to extract mathematical environments
    math_patterns = [
        (r'\$([^\$]+)\$', 'inline math $...$'),
        (r'\\\[(.*?)\\\]', 'display math \\[...\\]'),
        (r'\\begin\{mathpar\}(.*?)\\end\{mathpar\}', 'mathpar environment'),
    ]

    def check_balance(content: str, env_type: str, start_pos: int) -> int:
        """Check if parentheses are balanced and properly nested in the given content."""
        errors = 0

        # Calculate line number for error reporting
        lines_before = file_str[:start_pos].count('\n')
        line_number = lines_before + 1

        # Single stack to track all parentheses in nesting order
        # Each entry is (opening_symbol, position)
        stack = []

        i = 0
        while i < len(content):
            # Check for \left variants (any \left<char> can match any \right<char>)
            if i < len(content) - 5 and content[i:i+5] == r'\left':
                # Check if followed by a non-letter delimiter
                if i + 5 < len(content) and not content[i+5].isalpha():
                    delimiter = content[i+5]
                    stack.append((r'\left', i))
                    i += 6  # Skip \left and the delimiter
                    continue
            # Check for \right variants (can match any \left)
            # Must ensure it's not part of \rightarrow or similar commands
            elif i < len(content) - 6 and content[i:i+6] == r'\right':
                # Check if followed by a non-letter delimiter (not part of \rightarrow, etc.)
                if i + 6 < len(content) and not content[i+6].isalpha():
                    delimiter = content[i+6]
                    # Any \right matches any \left
                    if not stack or not stack[-1][0].startswith(r'\left'):
                        if not stack:
                            print(f"{filename}:{line_number}: Unmatched \\right in {env_type}")
                        else:
                            print(f"{filename}:{line_number}: Improperly nested \\right (expected to close '{stack[-1][0]}') in {env_type}")
                        errors += 1
                    else:
                        stack.pop()
                    i += 7  # Skip \right and the delimiter
                    continue

            # Check regular parentheses
            if content[i] == '(':
                stack.append(('(', i))
            elif content[i] == ')':
                if not stack or stack[-1][0] != '(':
                    if not stack:
                        print(f"{filename}:{line_number}: Unmatched ')' in {env_type}")
                    else:
                        print(f"{filename}:{line_number}: Improperly nested ')' (expected to close '{stack[-1][0]}') in {env_type}")
                    errors += 1
                else:
                    stack.pop()
            elif content[i] == '[':
                stack.append(('[', i))
            elif content[i] == ']':
                if not stack or stack[-1][0] != '[':
                    if not stack:
                        print(f"{filename}:{line_number}: Unmatched ']' in {env_type}")
                    else:
                        print(f"{filename}:{line_number}: Improperly nested ']' (expected to close '{stack[-1][0]}') in {env_type}")
                    errors += 1
                else:
                    stack.pop()
            elif content[i] == '{':
                # Only track { if it's not preceded by a backslash (not part of a LaTeX command)
                if i == 0 or content[i-1] != '\\':
                    stack.append(('{', i))
            elif content[i] == '}':
                # Only track } if it's not preceded by a backslash (not part of a LaTeX command)
                if i == 0 or content[i-1] != '\\':
                    if not stack or stack[-1][0] != '{':
                        if not stack:
                            print(f"{filename}:{line_number}: Unmatched '}}' in {env_type}")
                        else:
                            print(f"{filename}:{line_number}: Improperly nested '}}' (expected to close '{stack[-1][0]}') in {env_type}")
                        errors += 1
                    else:
                        stack.pop()

            i += 1        # Check for unclosed parentheses
        if stack:
            for opening_symbol, pos in stack:
                print(f"{filename}:{line_number}: Unclosed '{opening_symbol}' in {env_type}")
                errors += len(stack)
                break  # Only report once per environment

        return errors

    # Check each type of mathematical environment
    for pattern, env_type in math_patterns:
        for match in re.finditer(pattern, file_str, re.DOTALL):
            content = match.group(1)
            start_pos = match.start()
            num_errors += check_balance(content, env_type, start_pos)

    return num_errors


def check_math_content_validity(filename: str) -> int:
    r"""
    Checks that content in mathematical environments ($...$, \[...\], \begin{mathpar}...\end{mathpar})
    consists only of LaTeX macros (commands starting with \) or known LaTeX symbols.
    Returns the number of errors found.
    """
    num_errors = 0
    original_file_str = read_file_str(filename)
    file_str = original_file_str

    # Remove verbatim and code environments before checking math
    # These environments should not be validated
    verbatim_patterns = [
        r'\\begin\{Verbatim\}.*?\\end\{Verbatim\}',
        r'\\begin\{verbatim\}.*?\\end\{verbatim\}',
        r'\\begin\{lstlisting\}.*?\\end\{lstlisting\}',
        r'\\verb\|.*?\|',
        r'\\verb\+.*?\+',
        r'\\verb\!.*?\!',
    ]

    for pattern in verbatim_patterns:
        file_str = re.sub(pattern, '', file_str, flags=re.DOTALL)

    # Known LaTeX mathematical symbols and operators that don't require backslash
    known_symbols = set([
        '+', '-', '*', '/', '=', '<', '>', '!', '?', '|', ':', ';', ',', '.',
        '(', ')', '[', ']', '{', '}', '_', '^', '~', "'", '`', '"',
        '0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
        ' ', '\n', '\t', '&',  # Whitespace and alignment
    ])

    # Patterns to extract mathematical environments
    math_patterns = [
        (r'\$([^\$]+)\$', 'inline math $...$'),
        (r'\\\[(.*?)\\\]', 'display math \\[...\\]'),
        (r'\\begin\{mathpar\}(.*?)\\end\{mathpar\}', 'mathpar environment'),
    ]

    def check_content_validity(content: str, env_type: str, start_pos: int) -> int:
        """Check if content consists only of valid LaTeX macros and symbols."""
        errors = 0

        # Calculate line number for error reporting using original file
        lines_before = original_file_str[:start_pos].count('\n')
        line_number = lines_before + 1

        i = 0
        while i < len(content):
            char = content[i]

            # Check for LaTeX comments (% to end of line)
            if char == '%':
                # Skip until newline
                while i < len(content) and content[i] != '\n':
                    i += 1
                if i < len(content):
                    i += 1  # Skip the newline itself
                continue

            # Check for LaTeX commands (start with backslash)
            if char == '\\':
                # Find the end of the command
                j = i + 1
                # Command name consists of letters, or is a single special character
                if j < len(content) and content[j].isalpha():
                    cmd_start = j
                    while j < len(content) and content[j].isalpha():
                        j += 1
                    cmd_name = content[cmd_start:j]
                else:
                    # Single character command like \\ or \{ or \}
                    cmd_name = content[j] if j < len(content) else ''
                    j += 1

                # Skip optional asterisk (e.g., \inferrule*)
                if j < len(content) and content[j] == '*':
                    j += 1

                # Skip any whitespace and optional arguments in square brackets
                # For most commands, we don't skip mandatory arguments in braces - we want to check their content
                # Exception: \begin and \end commands have environment names in braces that should be skipped
                # Exception: \text, \texttt, \textXY commands contain regular text, not math, so skip their content
                while True:
                    # Skip whitespace
                    while j < len(content) and content[j] in ' \t\n':
                        j += 1

                    # Check for optional argument [...]
                    # These we DO skip because they're not typically math content (they're options/names)
                    if j < len(content) and content[j] == '[':
                        bracket_count = 1
                        j += 1
                        while j < len(content) and bracket_count > 0:
                            if content[j] == '\\' and j + 1 < len(content):
                                j += 2  # Skip escaped characters
                                continue
                            elif content[j] == '[':
                                bracket_count += 1
                            elif content[j] == ']':
                                bracket_count -= 1
                            j += 1
                        continue

                    # For \begin and \end, skip the environment name argument
                    # For \text, \texttt, \textXY (any text command), skip the text content
                    # For \hypertarget, skip the target name
                    # For \emph, skip the emphasized text
                    if j < len(content) and content[j] == '{':
                        should_skip = False

                        # Skip for \begin and \end
                        if cmd_name in ('begin', 'end'):
                            should_skip = True
                        # Skip for \text and any \textXY variant
                        elif cmd_name == 'text' or (cmd_name.startswith('text') and len(cmd_name) > 4 and cmd_name[4:].isalpha()):
                            should_skip = True
                        # Skip for \hypertarget
                        elif cmd_name == 'hypertarget':
                            should_skip = True
                        # Skip for \emph
                        elif cmd_name == 'emph':
                            should_skip = True

                        if should_skip:
                            brace_count = 1
                            j += 1
                            while j < len(content) and brace_count > 0:
                                if content[j] == '\\' and j + 1 < len(content):
                                    j += 2  # Skip escaped characters
                                    continue
                                elif content[j] == '{':
                                    brace_count += 1
                                elif content[j] == '}':
                                    brace_count -= 1
                                j += 1
                            continue

                    # No more arguments to skip
                    break

                i = j
                continue

            # Check if it's a known symbol
            if char in known_symbols:
                i += 1
                continue

            # Check if it's a letter (variables are allowed in math mode)
            # But only single letters - multi-letter identifiers should use LaTeX commands
            if char.isalpha():
                # Check if this starts a sequence of letters
                j = i + 1
                while j < len(content) and content[j].isalpha():
                    j += 1
                # If we have 2 or more consecutive letters, it's an error
                if j - i >= 2:
                    word = content[i:j]
                    context_start = max(0, i - 10)
                    context_end = min(len(content), j + 10)
                    context = content[context_start:context_end]
                    print(f"{filename}:{line_number}: Multi-letter identifier '{word}' in {env_type} should use LaTeX command, context: ...{context}...")
                    errors += 1
                    i = j
                    continue
                i += 1
                continue

            # If we get here, it's an unknown/invalid character
            # Get context for error message
            context_start = max(0, i - 10)
            context_end = min(len(content), i + 10)
            context = content[context_start:context_end]
            print(f"{filename}:{line_number}: Invalid character '{char}' (ord={ord(char)}) in {env_type}, context: ...{context}...")
            errors += 1
            i += 1

        return errors

    # Build a list of excluded ranges (verbatim blocks) for efficient checking
    excluded_ranges = []
    for verb_pattern in verbatim_patterns:
        for verb_match in re.finditer(verb_pattern, original_file_str, re.DOTALL):
            excluded_ranges.append((verb_match.start(), verb_match.end()))

    # Sort ranges for efficient binary search-like checking
    excluded_ranges.sort()

    def is_in_excluded_range(start: int, end: int) -> bool:
        """Check if a range overlaps with any excluded range."""
        for exc_start, exc_end in excluded_ranges:
            # If excluded range starts after our range ends, no more matches possible
            if exc_start >= end:
                break
            # Check for overlap
            if not (end <= exc_start or start >= exc_end):
                return True
        return False

    # Check each type of mathematical environment
    for pattern, env_type in math_patterns:
        for match in re.finditer(pattern, original_file_str, re.DOTALL):
            content = match.group(1)
            start_pos = match.start()
            end_pos = match.end()

            # Skip if this math environment is inside a verbatim block
            if is_in_excluded_range(start_pos, end_pos):
                continue

            num_errors += check_content_validity(content, env_type, start_pos)

    return num_errors


def check_relation_references(latex_files: list[str]) -> int:
    r"""
    Checks that for each 'relation <name>' and 'function <name>' in any .spec file,
    there exists a '\RenderRelation{<name>}' in some file
    in `latex_files`.
    Returns the number of errors found.
    """
    import pathlib
    import glob

    spec_files = fnmatch.filter(os.listdir("."), "*.spec")

    # Extract all relation and function names from .spec files
    defined_relations = set()
    relation_pattern = re.compile(r"^(?:\w+\s+)*(relation|function)\s+([a-zA-Z_][a-zA-Z0-9_]*)\(")

    for spec_file in spec_files:
        try:
            with open(spec_file, "r", encoding="utf-8") as f:
                for line in f:
                    match = relation_pattern.match(line.strip())
                    if match:
                        relation_name = match.group(2)
                        defined_relations.add(relation_name)
        except Exception as e:
            print(f"ERROR: Could not read spec file {spec_file}: {e}", file=sys.stderr)
            return 1

    # Find all \RenderRelation{<name>} references in .tex files
    referenced_relations = set()
    render_pattern = re.compile(r"\\RenderRelation\{([^}]+)\}")

    for latex_file in latex_files:
        try:
            file_content = read_file_str(latex_file)
            # Process line by line to exclude comments
            for line in file_content.splitlines():
                # Skip lines that are comments (start with %)
                stripped = line.strip()
                if stripped.startswith("%"):
                    continue
                # Check if line contains a comment and handle it
                comment_pos = line.find("%")
                if comment_pos != -1:
                    # Only look at the part before the comment
                    line_to_check = line[:comment_pos]
                else:
                    line_to_check = line

                for match in render_pattern.finditer(line_to_check):
                    relation_name = match.group(1)
                    referenced_relations.add(relation_name)
        except Exception as e:
            print(
                f"ERROR: Could not read LaTeX file {latex_file}: {e}", file=sys.stderr
            )
            return 1

    missing_references = defined_relations - referenced_relations
    unused_references = referenced_relations - defined_relations
    num_errors = len(missing_references) + len(unused_references)
    if missing_references:
        print(
            f"ERROR: Found {len(missing_references)} relations defined in .spec files but not referenced in .tex files:"
        )
        for relation in sorted(missing_references):
            print(f"  Missing \\RenderRelation{{{relation}}}")
    if unused_references:
        print(
            f"ERROR: Found {len(unused_references)} \\RenderRelation references to undefined relations:"
        )
        for relation in sorted(unused_references):
            print(f"  Undefined relation: {relation}")

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
    if args.console_macros:
        aslref_path = args.aslref if args.aslref else "aslref"
        apply_console_macros(aslref_path)
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
    num_errors += check_unused_latex_macros(all_latex_sources)
    num_errors += check_zero_arg_macro_misuse(content_latex_sources)
    num_errors += check_relation_references(content_latex_sources)
    num_errors += check_per_file(
        content_latex_sources,
        [
            check_repeated_words,
            check_repeated_lines,
            detect_incorrect_latex_macros_spacing,
            check_rules,
            check_mathpar_macro_usage,
            check_balanced_parentheses_in_math,
            check_math_content_validity,
        ],
    )

    if num_errors > 0:
        print(f"There were {num_errors} errors!", file=sys.stderr)
        sys.exit(1)


if __name__ == "__main__":
    main()
