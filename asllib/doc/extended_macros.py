#!/usr/bin/python3

import os, fnmatch, subprocess
from abc import ABC, abstractmethod
from dataclasses import dataclass

ASLREF_EXE = '../../_build/default/asllib/aslref.exe'

debug = False

def yellow_error_message(msg: str) -> str:
    YELLOW = '\033[43m'
    COLOR_RESET = '\033[m'
    YELLOW + msg + COLOR_RESET

def get_latex_sources() -> list[str]:
    r"""
    Returns the list of .tex files in the current directory,
    excluding macros.tex.
    """
    latex_files = fnmatch.filter(os.listdir('.'), '*.tex')
    if 'macros.tex' in latex_files:
        latex_files.remove('macros.tex')
    return latex_files

def execute_and_capture_output(command: str) -> str:
    r"""
    Executes `command` and returns the output in a string.
    """
    args = command.split()
    try:
        output = subprocess.check_output(args, text=True)
        return output
    except Exception as e:
        print(yellow_error_message(f'Error: failed executing {command}! Aborting run'))
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
    def extend_to_all_blocks(cls, blocks_to_transform: list[Block], num_lines) -> list[tuple[Block, bool]]:
        r"""
        Adds the blocks that should not be transformed between
        `blocks_to_transform`. The output is a list of pairs, each consisting
        of a block and a Boolean value signifying whether the block should be
        transformed or not.
        The blocks in the output list should partition the list of lines in a
        given file.
        """
        assert blocks_to_transform
        all_blocks : list[Block] = []
        last_end_line = 0
        for block in blocks_to_transform:
            if block.begin > last_end_line:
                all_blocks.append( (Block(last_end_line, block.begin - 1), False) )
            all_blocks.append( (block, True) )
            last_end_line = block.end + 1
        # Append any lines after the last block to transform.
        if last_end_line < num_lines - 1:
            all_blocks.append( (Block(last_end_line, num_lines), False) )
        return all_blocks

    @classmethod
    def validate_blocks(cls, blocks: list[tuple[Block, bool]]):
        last_line_number = -1
        for (block, _) in blocks:
            assert block.end >= block.begin
            assert block.begin > last_line_number
            last_line_number = block.end

    @classmethod
    def apply_to_file(cls, source: str):
        lines : list[str] = []
        with open(source) as file:
            lines = file.readlines()
        blocks_to_transform = cls.find_blocks(lines)
        if not blocks_to_transform:
            return
        if debug:
            print(f'{source}: found {len(blocks_to_transform)} macro instance(s)')
        all_blocks : list[Block] = cls.extend_to_all_blocks(blocks_to_transform, len(lines))
        cls.validate_blocks(all_blocks)
        output_lines : list[str] = []
        number_of_changes = 0
        for (block, should_transform) in all_blocks:
            original_lines = lines[block.begin : block.end + 1]
            block_output_lines : list[str] = []
            if should_transform:
                block_output_lines = cls.transform(original_lines)
                # Ensure lines end with a newline character.
                block_output_lines = [line + '\n' if not line.endswith('\n') else line for line in block_output_lines]
                if block_output_lines != original_lines:
                    first_changed_line_number = len(output_lines) + 1
                    last_changed_line_number = first_changed_line_number + len(block_output_lines)
                    print(f'{source} lines {first_changed_line_number}-{last_changed_line_number} are new.')
                    number_of_changes += 1
            else:
                block_output_lines = original_lines
            output_lines.extend(block_output_lines)
        if number_of_changes > 0: # Avoid changing file timestamps
            with open(source, 'w') as file:
                file.writelines(output_lines)

    @classmethod
    def apply_to_files(cls, sources):
        for source in sources:
            try:
                cls.apply_to_file(source)
            except ValueError as e:
                print(f'Error in ./{source}: ', e)

class ConsoleMacro(BlockMacro):
    r"""
    A console macro starts with `% CONSOLE_BEGIN <cmd>`
    and ends with `CONSOLE_END`.
    The macro executes `<cmd>` and typesets its output.
    """
    CONSOLE_BEGIN = 'CONSOLE_BEGIN'
    CONSOLE_END = 'CONSOLE_END'

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
            error_message = f'there are {len(begin_lines)} occurrences of {cls.CONSOLE_BEGIN}'\
                f' and {len(end_lines)} occurrences of {cls.CONSOLE_END}! Aborting.'
            raise ValueError(error_message)
        blocks : list[Block] = []
        for (begin_line_number, end_line_number) in zip(begin_lines, end_lines):
            blocks.append(Block(begin_line_number, end_line_number))
        return blocks

    @classmethod
    def transform(cls, block_lines: list[str]) -> list[str]:
        assert block_lines
        begin_line : str = block_lines[0]
        command = (
            begin_line.replace('%', '')
            .replace(cls.CONSOLE_BEGIN, '')
            .replace('aslref', ASLREF_EXE)
            .replace('\\definitiontests', '../tests/ASLDefinition.t')
            .replace('\\syntaxtests', '../tests/ASLSyntaxReference.t')
            .replace('\\typingtests', '../tests/ASLTypingReference.t')
            .replace('\\semanticstests', '../tests/ASLSemanticsReference.t')
            .replace('\n', '')
            .strip()
        )
        if debug:
            print(f'Executing {command}')
        transformed_lines = execute_and_capture_output(command).splitlines()
        end_line = block_lines[-1]
        transformed_lines = [
            begin_line,
            '\\begin{small}',
            '\\begin{Verbatim}[frame=single]',
        ] + transformed_lines + [
            '\\end{Verbatim}',
            '\\end{small}',
            end_line
        ]
        return transformed_lines

def apply_all_macros():
    print('Extended macros: applying all macros... ')
    ConsoleMacro.apply_to_files(get_latex_sources())
    print('Extended macros: done')

def main():
    apply_all_macros()

if __name__ == "__main__":
    main()
