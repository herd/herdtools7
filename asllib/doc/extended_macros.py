#!/usr/bin/python3

import os, fnmatch, subprocess, shlex
from abc import ABC, abstractmethod
from dataclasses import dataclass
import re
from typing import List

ASLREF_EXE: str = "aslref"

debug = False


def read_file_lines(filename: str) -> List[str]:
    with open(filename, "r", encoding="utf-8") as file:
        return file.readlines()


def read_file_str(filename: str) -> List[str]:
    with open(filename, "r", encoding="utf-8") as file:
        return file.read()


def yellow_error_message(msg: str) -> str:
    YELLOW = "\033[43m"
    COLOR_RESET = "\033[m"
    return YELLOW + msg + COLOR_RESET


def get_latex_sources(exclude) -> list[str]:
    r"""
    Returns the list of .tex files in the current directory.
    If 'exclude' is True, common files that are not required
    for transformation and linting are excluded.
    """
    latex_files = fnmatch.filter(os.listdir("."), "*.tex")
    if exclude:
        excluded_files = [
            "ASLReference.tex",
            "ASLmacros.tex",
            "ASLRefALP2.1ChangeLog.tex",
            "ASLRefALP2ChangeLog.tex",
            "generated_macros.tex",
            "rendering_macros.tex",
            "variable_name_macros.tex",
        ]
        for excluded_file in excluded_files:
            latex_files.remove(excluded_file)
    return latex_files


def execute_and_capture_output(command: str, error_expected: bool) -> str:
    r"""
    Executes `command` and returns the output in a string.
    """
    args = shlex.split(command)
    if not args:
        raise ValueError(f"Missing command after CONSOLE_BEGIN!")
    if "aslref" not in args[0]:
        raise ValueError(f"Command {command} does not refer to aslref!")
    try:
        if error_expected:
            subprocess_result = subprocess.run(
                args, stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True
            )
            output = subprocess_result.stdout + subprocess_result.stderr
        else:
            output = subprocess.check_output(args, text=True)
        return output
    except Exception as e:
        print(yellow_error_message(f"Error: failed executing {command}! Aborting run"))
        raise e


@dataclass
class Block:
    r"""
    Represents the range of lines in a source file
    given by the (0-indexed) slice `[begin : end + 1]`.
    """

    begin: int
    end: int


class BlockMacro(ABC):
    r"""
    Represents a transformation to blocks (line ranges).
    """

    @abstractmethod
    def find_blocks(cls, lines: list[str]) -> list[Block]:
        r"""
        Returns the blocks within `lines` that this macro should transform.
        """
        pass

    @abstractmethod
    def transform(cls, block_lines: list[str]) -> list[str]:
        r"""
        Transforms `block_lines` into another list of lines.
        """
        pass

    @classmethod
    def extend_to_all_blocks(
        cls, blocks_to_transform: list[Block], num_lines
    ) -> list[tuple[Block, bool]]:
        r"""
        Adds the blocks that should not be transformed between
        `blocks_to_transform`. The output is a list of pairs, each consisting
        of a block and a Boolean value signifying whether the block should be
        transformed or not.
        The blocks in the output list should partition the list of lines in a
        given file.
        """
        assert blocks_to_transform
        all_blocks: list[Block] = []
        last_end_line = 0
        for block in blocks_to_transform:
            if block.begin > last_end_line:
                all_blocks.append((Block(last_end_line, block.begin - 1), False))
            all_blocks.append((block, True))
            last_end_line = block.end + 1
        # Append any lines after the last block to transform.
        if last_end_line < num_lines:
            all_blocks.append((Block(last_end_line, num_lines - 1), False))
        return all_blocks

    @classmethod
    def validate_blocks(cls, blocks: list[tuple[Block, bool]], num_lines):
        r"""
        Ensures that the list of blocks form non-overlapping intervals
        that partition the range (0, num_lines).
        """
        last_line_number = -1
        for block, _ in blocks:
            assert block.end >= block.begin
            assert block.begin == last_line_number + 1
            last_line_number = block.end
        assert last_line_number == num_lines - 1

    @classmethod
    def apply_to_file(cls, source: str):
        lines: list[str] = []
        with open(source) as file:
            lines = file.readlines()
        blocks_to_transform = cls.find_blocks(lines)
        if not blocks_to_transform:
            return
        if debug:
            print(f"{source}: found {len(blocks_to_transform)} macro instance(s)")
        all_blocks: list[Block] = cls.extend_to_all_blocks(
            blocks_to_transform, len(lines)
        )
        cls.validate_blocks(all_blocks, len(lines))
        output_lines: list[str] = []
        number_of_changes = 0
        for block, should_transform in all_blocks:
            original_lines = lines[block.begin : block.end + 1]
            block_output_lines: list[str] = []
            if should_transform:
                block_output_lines = cls.transform(original_lines)
                # Ensure lines end with a newline character.
                block_output_lines = [
                    line + "\n" if not line.endswith("\n") else line
                    for line in block_output_lines
                ]
                if block_output_lines != original_lines:
                    first_changed_line_number = len(output_lines) + 1
                    last_changed_line_number = first_changed_line_number + len(
                        block_output_lines
                    )
                    print(
                        f"{source} lines {first_changed_line_number}-{last_changed_line_number} are new."
                    )
                    number_of_changes += 1
            else:
                block_output_lines = original_lines
            output_lines.extend(block_output_lines)
        if number_of_changes > 0:  # Avoid changing file timestamps
            with open(source, "w") as file:
                file.writelines(output_lines)

    @classmethod
    def apply_to_files(cls, sources):
        for source in sources:
            try:
                cls.apply_to_file(source)
            except ValueError as e:
                print(f"Error in ./{source}: ", e)


class ConsoleMacro(BlockMacro):
    r"""
    A console macro starts with `% CONSOLE_BEGIN <cmd>`
    and ends with `CONSOLE_END`.
    The macro executes `<cmd>` and typesets its output.
    """

    CONSOLE_BEGIN = "CONSOLE_BEGIN"
    CONSOLE_END = "CONSOLE_END"
    CONSOLE_STDERR = "CONSOLE_STDERR"
    CONSOLE_CMD = "CONSOLE_CMD"

    @classmethod
    def find_blocks(cls, lines):
        begin_lines = []
        end_lines = []
        for line_number, line in enumerate(lines):
            if cls.CONSOLE_BEGIN in line:
                begin_lines.append(line_number)
            if cls.CONSOLE_END in line:
                end_lines.append(line_number)
        if len(begin_lines) != len(end_lines):
            error_message = (
                f"there are {len(begin_lines)} occurrences of {cls.CONSOLE_BEGIN}"
                f" and {len(end_lines)} occurrences of {cls.CONSOLE_END}! Aborting."
            )
            raise ValueError(error_message)
        blocks: list[Block] = []
        for begin_line_number, end_line_number in zip(begin_lines, end_lines):
            blocks.append(Block(begin_line_number, end_line_number))
        return blocks

    @classmethod
    def transform(cls, block_lines: list[str]) -> list[str]:
        assert block_lines
        begin_line: str = block_lines[0]
        error_expected = cls.CONSOLE_STDERR in begin_line
        include_cmd = cls.CONSOLE_CMD in begin_line
        command = begin_line.replace("aslref", ASLREF_EXE)

        command = (
            begin_line.replace("%", "")
            .replace(cls.CONSOLE_BEGIN, "")
            .replace(cls.CONSOLE_STDERR, "")
            .replace(cls.CONSOLE_CMD, "")
            .replace("\\definitiontests", "../tests/ASLDefinition.t")
            .replace("\\syntaxtests", "../tests/ASLSyntaxReference.t")
            .replace("\\typingtests", "../tests/ASLTypingReference.t")
            .replace("\\semanticstests", "../tests/ASLSemanticsReference.t")
            .replace("\n", "")
            .strip()
        )
        # `command`` is potentially included in the LaTeX output (if `CONSOLE_CMD``
        # appears in the macro).
        # Since we don't want to include any user-specific paths `command` is used
        # for inclusion whereas the command with the actual path to aslref is
        # used in `executable_command`.
        executable_command = command.replace("aslref", ASLREF_EXE)
        if debug:
            print(f"Executing {executable_command}")
        transformed_lines = execute_and_capture_output(
            executable_command, error_expected
        ).splitlines()
        end_line = block_lines[-1]
        transformed_lines = (
            [begin_line, "\\begin{Verbatim}[fontsize=\\footnotesize, frame=single]"]
            + (["> " + command] if include_cmd else [])
            + transformed_lines
            + ["\\end{Verbatim}", end_line]
        )
        return transformed_lines


def transform_by_line(filenames: list[str], from_pattern: str, to_pattern: str):
    r"""
    Performs a line-by-line transformation for all files in 'filenames'.
    Lines matching 'from_pattern' are transformed using the 'to_pattern'.

    For example, the following substitutes a LaTeX macro:
    transform_by_line(
        files,
        r"All of the following apply \(\\textsc{(.*?)}\)",
        r"\\AllApplyCase{\1}"
    )
    """
    num_changes = 0
    for filename in filenames:
        new_lines: list[str] = []
        lines: list[str] = []
        with open(filename, "r", encoding="utf-8") as file:
            lines = file.readlines()
        for line_number, line in enumerate(lines, start=1):
            new_line = re.sub(from_pattern, to_pattern, line)
            new_lines.append(new_line)
            if new_line != line:
                print(
                    f"{filename} {line_number}: transformed from '{line.strip()}' to '{new_line.strip()}'"
                )
                num_changes += 1
        with open(filename, "w", encoding="utf-8") as file:
            file.writelines(new_lines)
    if num_changes > 0:
        print(f"Performed {num_changes} line transformations")


def apply_console_macros(aslref_path: str):
    global ASLREF_EXE
    ASLREF_EXE = aslref_path
    if not os.path.isfile(ASLREF_EXE):
        raise Exception(f"Unable to find aslref in path {ASLREF_EXE}. Perhaps you need to build it?")
    else:
        print(f"Using aslref path {ASLREF_EXE}")
    print("Extended macros: applying console macros... ")
    pruned_latex_sources = get_latex_sources(True)
    transform_by_line(
        pruned_latex_sources,
        r"\\AllApplyCase{(.*?)}:",
        r"\\AllApplyCase{\1}",
    )
    transform_by_line(
        pruned_latex_sources,
        r"\\AllApply:",
        r"\\AllApply",
    )
    transform_by_line(
        pruned_latex_sources,
        r"\\OneApplies:",
        r"\\OneApplies",
    )
    print("Extended macros: done")
