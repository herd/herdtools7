#!/usr/bin/python3

import os, fnmatch, subprocess, shlex
from abc import ABC, abstractmethod
from dataclasses import dataclass
from pathlib import Path
import re
from typing import List, Set

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
    generated_element_filename = "generated_elements.tex"  # Not a real LaTeX file
    latex_files.remove(generated_element_filename)
    if exclude:
        excluded_files = [
            "ASLReference.tex",
            "ASLmacros.tex",
            "ASLRefALP2.1ChangeLog.tex",
            "ASLRefALP2ChangeLog.tex",
            "generated_macros.tex",
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


class GeneratedSpecMacro(BlockMacro):
    r"""
    A generated spec macro starts with `% BEGIN_GENERATED_ELEMENT(<name>)`
    and ends with `% END_GENERATED_ELEMENT(<name>)`.
    The macro copies the range of lines with the same start-end delimiters from `generated_elements.tex`.
    """

    BEGIN_GENERATED_ELEMENT_PREFIX = "BEGIN_GENERATED_ELEMENT"
    END_GENERATED_ELEMENT_PREFIX = "END_GENERATED_ELEMENT"
    BEGIN_GENERATED_ELEMENT_REGEXP = macro_def_pattern = re.compile(
        r"%\s*BEGIN_GENERATED_ELEMENT\(([^)]+)\)"
    )
    END_GENERATED_ELEMENT_REGEXP = re.compile(r"%\s*END_GENERATED_ELEMENT")

    GENERATED_ELEMENTS_FILENAME = "generated_elements.tex"
    element_name_to_lines = dict()

    @classmethod
    def element_name(cls, str):
        match_begin = cls.BEGIN_GENERATED_ELEMENT_REGEXP.search(str)
        return match_begin.group(1) if match_begin else None

    @classmethod
    def element_names(cls, str):
        """Parse element name(s) from a BEGIN_GENERATED_ELEMENT line.
        Returns a list of element names, split by comma and stripped."""
        element_name_str = cls.element_name(str)
        if element_name_str is None:
            return []
        return [name.strip() for name in element_name_str.split(",")]

    @classmethod
    def load_generated_elements(cls):
        try:
            lines = read_file_lines(cls.GENERATED_ELEMENTS_FILENAME)
            blocks = cls.find_blocks(lines)
            for block in blocks:
                element_name = cls.element_name(lines[block.begin])
                cls.element_name_to_lines[element_name] = lines[
                    block.begin : block.end + 1
                ]
        except ValueError as e:
            e.add_note(f"while processing {cls.GENERATED_ELEMENTS_FILENAME}")
            raise e

    @classmethod
    def find_blocks(cls, lines):
        begin_lines = []
        end_lines = []
        for line_number, line in enumerate(lines):
            match_begin = cls.BEGIN_GENERATED_ELEMENT_REGEXP.search(line)
            if match_begin and match_begin.group(1):
                begin_lines.append(line_number)
            match_end_element = cls.END_GENERATED_ELEMENT_REGEXP.search(line)
            if match_end_element:
                end_lines.append(line_number)
        if len(begin_lines) != len(end_lines):
            error_message = (
                f"there are {len(begin_lines)} occurrences of {cls.BEGIN_GENERATED_ELEMENT_PREFIX}"
                f" and {len(end_lines)} occurrences of {cls.END_GENERATED_ELEMENT_PREFIX}! Aborting."
            )
            raise ValueError(error_message)
        blocks: list[Block] = []
        for begin_line_number, end_line_number in zip(begin_lines, end_lines):
            blocks.append(Block(begin_line_number, end_line_number))
        return blocks

    @classmethod
    def transform(cls, block_lines: list[str]) -> list[str]:
        element_names = cls.element_names(block_lines[0])
        element_blocks_content: list[list[str]] = (
            []
        )  # The blocks without their header/footer lines
        for element_name in element_names:
            if element_name in cls.element_name_to_lines:
                block: list[str] = cls.element_name_to_lines[element_name]
                block_content: list[str] = block[1:-1]
                element_blocks_content.append(block_content)
            else:
                raise Exception(
                    f"Element '{element_name}' referenced by '{block_lines[0]}' not found in {cls.GENERATED_ELEMENTS_FILENAME}"
                )
        if len(element_blocks_content) > 1:
            # Substitute lines containing "\begin{flalign*}" with "\\" from second block and on
            for i in range(1, len(element_blocks_content)):
                content_lines = element_blocks_content[i]
                # Remove lines containing "\begin{flalign*}" from blocks 2 and on
                new_content_lines = [
                    r"\\" if r"\begin{flalign*}" in line else line
                    for line in content_lines
                ]
                element_blocks_content[i] = new_content_lines
            # Remove lines containing "\end{flalign*}" from all blocks except the last one
            for i in range(len(element_blocks_content) - 1):
                content_lines = element_blocks_content[i]
                new_content_lines = [
                    line for line in content_lines if r"\end{flalign*}" not in line
                ]
                element_blocks_content[i] = new_content_lines
        # Concatenate lines from all blocks into a single list
        element_lines = sum(element_blocks_content, [])

        if "remove_hypertargets" in block_lines[0]:
            hypertarget_regexp = r"\\hypertarget\{.*?\}\{.*?\}"
            lines_without_hypertargets = list(
                map(
                    lambda line: re.sub(hypertarget_regexp, "", line).strip(),
                    element_lines,
                )
            )
            without_empty_lines = list(filter(None, lines_without_hypertargets))
            element_lines = without_empty_lines

        # Maintain the original first and last lines, as they may contain
        # extra annotations (like [remove_hypertargets]).
        element_lines = [block_lines[0]] + element_lines + [block_lines[-1]]
        return element_lines


def apply_generated_spec_macros():
    pruned_latex_sources = get_latex_sources(True)
    print("Extended macros: applying spec-generated copying... ")
    GeneratedSpecMacro.load_generated_elements()
    GeneratedSpecMacro.apply_to_files(pruned_latex_sources)
    print("Generated spec macros: done")


def apply_console_macros(aslref_path: str):
    global ASLREF_EXE
    ASLREF_EXE = aslref_path
    if not os.path.isfile(ASLREF_EXE):
        raise Exception(f"Unable to find aslref in path {ASLREF_EXE}")
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
