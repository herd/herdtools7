#!/usr/bin/python3

import os, fnmatch, sys

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

def check_hyperlinks_and_hypertargets():
    r"""
    Checks whether all labels defined in `\hyperlink` definitions match
    labels defined in `\hypertarget` definitions, print the mismatches to the console.
    """
    latex_files = fnmatch.filter(os.listdir('.'), '*.tex')
    hyperlink_labels : set[str] = set()
    hypertarget_labels : set[str] = set()
    for latex_souce in latex_files:
        with open(latex_souce) as file:
            for line in file.readlines():
                extract_labels_from_line(line, "\\hyperlink{", hyperlink_labels)
                extract_labels_from_line(line, "\\hypertarget{", hypertarget_labels)
    num_errors, num_warnings = 0, 0
    missing_hypertargets = hyperlink_labels.difference(hypertarget_labels)
    if not missing_hypertargets == set():
        num_missing_hypertargets = len(missing_hypertargets)
        num_errors += num_missing_hypertargets
        print(f"ERROR: found {num_missing_hypertargets} hyperlinks without matching hypertargets: ", file=sys.stderr)
        for label in missing_hypertargets:
            print(label, file=sys.stderr)

    missing_hyperlinks = hypertarget_labels.difference(hyperlink_labels)
    if not missing_hyperlinks == set():
        num_missing_hyperlinks = len(missing_hyperlinks)
        num_warnings += num_missing_hyperlinks
        print(f"WARNING: found {num_missing_hyperlinks} hypertargets without matching hyperlinks: ", file=sys.stderr)
        for label in missing_hyperlinks:
            print(label, file=sys.stderr)

    return num_errors, num_warnings

def main():
    num_errors, num_warnings = check_hyperlinks_and_hypertargets()
    print(f"There were {num_errors} errors and {num_warnings} warnings!", file=sys.stderr)
    if num_errors > 0:
        sys.exit(1)

if __name__ == "__main__":
    main()
