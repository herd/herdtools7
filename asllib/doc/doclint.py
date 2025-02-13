#!/usr/bin/python3

import os, sys, fnmatch
from extended_macros import apply_all_macros, get_latex_sources
import re

import argparse

cli_parser = argparse.ArgumentParser(prog="ASL Reference Linter")
cli_parser.add_argument(
    "-t",
    "--transform",
    help="Rewrites *.tex files with extended macros",
    action="store_true",
)


def extract_labels_from_line(line: str, left_delim: str, labels: set[str]):
    r"""
    Adds all labels found in `line` into `labels`. A label starts with the
    sub-string given by `left_delim` and ends with the substring `}`.
    """
    right_delim = "}"
    label_pos: int = 0
    while True:
        label_pos: int = line.find(left_delim, label_pos)
        if label_pos == -1:
            return
        right_brace_pos: int = line.find(right_delim, label_pos)
        if right_brace_pos == -1:
            return
        label = line[label_pos + len(left_delim) : right_brace_pos]
        labels.add(label)
        label_pos = right_brace_pos + 1


def check_hyperlinks_and_hypertargets(latex_files: list[str]):
    r"""
    Checks whether all labels defined in `\hyperlink` definitions match
    labels defined in `\hypertarget` definitions, print the mismatches
    to the console.
    """
    hyperlink_labels: set[str] = set()
    hypertarget_labels: set[str] = set()
    for latex_source in latex_files:
        with open(latex_source) as file:
            for line in file.readlines():
                extract_labels_from_line(line, "\\hyperlink{", hyperlink_labels)
                extract_labels_from_line(line, "\\hypertarget{", hypertarget_labels)
    num_errors = 0
    missing_hypertargets = hyperlink_labels.difference(hypertarget_labels)
    if missing_hypertargets:
        num_missing_hypertargets = len(missing_hypertargets)
        num_errors += num_missing_hypertargets
        print(
            f"ERROR: found {num_missing_hypertargets} hyperlinks without \
              matching hypertargets: ",
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
    with open("./ASLReference.log") as file:
        log_str = file.read()
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
    MAX_TODODEFINE_INSTANCES = 7
    num_todo_define = 0
    for latex_source in latex_files:
        with open(latex_source) as file:
            file_str = file.read()
            num_todo_define += file_str.count("\\tododefine")
    num_todo_define -= 1  # Ignore the definition of the \tododefine macro itself.
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


def check_repeated_words(file, latex_source: str) -> int:
    r"""
    Checks if 'file' contains occurrences of the same word
    repeated twice, independent of case. For example, "the the".
    Errors are reported for the file name 'filename' and the total
    number of found errors is returned.
    """
    num_errors = 0
    line_number = 0
    last_word = ""
    for line in file.readlines():
        line_number += 1
        line = line.strip()
        parts = line.split()
        if len(parts) < 2:
            continue
        for current_word in parts:
            if current_word.isalpha() and last_word.lower() == current_word.lower():
                num_errors += 1
                print(
                    f"./{latex_source} line {line_number}: \
                        word repetition ({last_word} {current_word}) in '{line}'"
                )
            last_word = current_word
    return num_errors


def detect_incorrect_latex_macros_spacing(file, filename: str) -> int:
    r"""
    Detects erroneous occurrences of LaTeX macros rendered without
    separation from the next word in 'file'.
    Errors are reported for the file name 'filename' and the total
    number of found errors is returned.
    """
    num_errors = 0
    file_str = file.read()
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


def check_consistent_prose_formally_paragraphs(file, filename) -> int:
    r"""
    Checks that in 'file' the list of \ProseParagraph and \FormallyParagraph
    are such that each \ProseParagraph is followed by a \FormallyParagraph
    and each \FormallyParagraph is preceded by a \ProseParagraph.
    Errors are reported for 'filename' and the returned value is either
    0 for no errors and 1, otherwise.
    """
    num_errors = 0
    line_number = 0
    num_prose_paragraphs = 0
    num_formally_paragraphs = 0
    for line in file.readlines():
        line_number += 1
        if "\\ProseParagraph" in line:
            num_prose_paragraphs += 1
        if "\\FormallyParagraph" in line:
            num_formally_paragraphs += 1
        if num_formally_paragraphs > num_prose_paragraphs:
            print(
                f"{filename} line {line_number} (or before): encountered Formally paragraph missing a Prose paragraph"
            )
            num_errors += 1
        if num_prose_paragraphs > num_formally_paragraphs + 1:
            print(
                f"{filename} line {line_number} (or before): encountered Prose paragraph missing Formally paragraph"
            )
            num_formally_paragraphs += 1
            num_errors += 1
        if num_errors > 0:
            break
    return num_errors


def check_per_file(latex_files: list[str], checks):
    r"""
    Applies the list of functions in 'checks' to each file in 'latex files',
    accumulating the number of errors and returning the total number of errors
    across all files.
    """
    num_errors = 0
    for filename in latex_files:
        with open(filename) as file:
            for check in checks:
                num_errors += check(file, filename)
    return num_errors


def main():
    args = cli_parser.parse_args()
    if args.transform:
        apply_all_macros()
    print("Linting files...")
    all_latex_sources = get_latex_sources(False)
    content_latex_sources = get_latex_sources(True)
    num_errors = 0
    num_errors += check_hyperlinks_and_hypertargets(all_latex_sources)
    num_errors += check_undefined_references_and_multiply_defined_labels()
    num_errors += check_tododefines(content_latex_sources)
    num_errors += check_per_file(
        all_latex_sources,
        [
            check_repeated_words,
            detect_incorrect_latex_macros_spacing,
            check_consistent_prose_formally_paragraphs,
        ],
    )

    if num_errors > 0:
        print(f"There were {num_errors} errors!", file=sys.stderr)
        sys.exit(1)


if __name__ == "__main__":
    main()
