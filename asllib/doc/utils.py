#!/usr/bin/python3

from typing import List


def read_file_lines(filename: str) -> List[str]:
    with open(filename, "r", encoding="utf-8") as file:
        return file.readlines()


def read_file_str(filename: str) -> List[str]:
    with open(filename, "r", encoding="utf-8") as file:
        return file.read()


DO_NOT_LINT_STR = "DO NOT LINT"


def is_skipped_line(line: str):
    return DO_NOT_LINT_STR in line or line.strip().startswith("%")


def is_content_line(line: str):
    return not is_skipped_line(line)
