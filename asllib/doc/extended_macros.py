#!/usr/bin/python3

import os, fnmatch, subprocess, shlex, shutil
from dataclasses import dataclass
import re

debug = False


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
    latex_files = sorted(fnmatch.filter(os.listdir("."), "*.tex"))
    if exclude:
        excluded_files = [
            "ASLReference.tex",
            "ASLmacros.tex",
            "generated_macros.tex",
            "variable_name_macros.tex",
        ]
        for excluded_file in excluded_files:
            if excluded_file in latex_files:
                latex_files.remove(excluded_file)
    return latex_files


def execute_and_capture_output(args: list[str], error_expected: bool) -> str:
    r"""
    Executes `command` and returns the output in a string.
    """
    if not args:
        raise ValueError("Missing aslref command")
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
        print(
            yellow_error_message(
                f"Error: failed executing {shlex.join(args)}! Aborting run"
            )
        )
        raise e


@dataclass(frozen=True)
class ConsoleInvocation:
    identifier: str
    test_path: str
    aslref_options: tuple[str, ...]
    expect_error: bool
    show_command: bool


def find_console_invocations(sources: list[str]) -> list[ConsoleInvocation]:
    aslref_option_keys = {
        "type-check-no-warn": "--type-check-no-warn",
    }
    generator_option_keys = {"expect-error", "show-command"}
    console_invocation_re = re.compile(
        r"^\s*\\RenderConsoleFor(?:\[([^]]*)\])?"
        r"\{([A-Za-z][A-Za-z0-9-]*)\}\{([^{}]+)\}\s*$"
    )
    invocations = []
    identifiers = {}
    for source in sources:
        with open(source, encoding="utf-8") as file:
            for line_number, line in enumerate(file, start=1):
                if "\\RenderConsoleFor" not in line:
                    continue
                match = console_invocation_re.fullmatch(line.rstrip("\n"))
                if not match:
                    raise ValueError(
                        f"{source}:{line_number}: malformed RenderConsoleFor invocation; "
                        "the invocation must occupy one line"
                    )
                option_text, identifier, test_path = match.groups()
                option_keys = shlex.split(option_text or "")
                unknown_option_keys = set(option_keys).difference(
                    aslref_option_keys, generator_option_keys
                )
                if unknown_option_keys:
                    raise ValueError(
                        f"{source}:{line_number}: unknown RenderConsoleFor option(s): "
                        + ", ".join(sorted(unknown_option_keys))
                    )
                expect_error = "expect-error" in option_keys
                show_command = "show-command" in option_keys
                aslref_options = tuple(
                    aslref_option_keys[option]
                    for option in option_keys
                    if option in aslref_option_keys
                )
                if identifier in identifiers:
                    previous_source, previous_line = identifiers[identifier]
                    raise ValueError(
                        f"{source}:{line_number}: duplicate console ID {identifier!r}; "
                        f"first used at {previous_source}:{previous_line}"
                    )
                identifiers[identifier] = (source, line_number)
                invocations.append(
                    ConsoleInvocation(
                        identifier,
                        test_path,
                        aslref_options,
                        expect_error,
                        show_command,
                    )
                )
    return invocations


def resolve_test_path(test_path: str) -> str:
    test_path_replacements = {
        r"\definitiontests": "../tests/ASLDefinition.t",
        r"\syntaxtests": "../tests/ASLSyntaxReference.t",
        r"\typingtests": "../tests/ASLTypingReference.t",
        r"\semanticstests": "../tests/ASLSemanticsReference.t",
    }
    for latex_macro, directory in test_path_replacements.items():
        if test_path.startswith(latex_macro):
            return directory + test_path[len(latex_macro) :]
    raise ValueError(f"Unsupported test path {test_path!r}")


def generate_console_outputs(invocations: list[ConsoleInvocation], aslref_exe: str):
    output_dir = "generated_console_output"
    os.makedirs(output_dir, exist_ok=True)
    generated_output_files = set()
    for invocation in invocations:
        test_path = resolve_test_path(invocation.test_path)
        display_args = ["aslref", *invocation.aslref_options, test_path]
        executable_args = [aslref_exe, *invocation.aslref_options, test_path]
        if debug:
            print(f"Executing {shlex.join(executable_args)}")
        output = execute_and_capture_output(
            executable_args, invocation.expect_error
        ).splitlines()
        output_lines = []
        if invocation.show_command:
            output_lines.append("> " + " ".join(display_args))
        output_lines.extend(output)
        output_filename = os.path.join(
            output_dir, invocation.identifier + ".txt"
        )
        write_if_changed(output_filename, "\n".join(output_lines) + "\n")
        generated_output_files.add(os.path.basename(output_filename))
    for filename in os.listdir(output_dir):
        if filename not in generated_output_files:
            os.remove(os.path.join(output_dir, filename))


def write_if_changed(filename: str, content: str):
    try:
        with open(filename, encoding="utf-8") as file:
            if file.read() == content:
                return
    except FileNotFoundError:
        pass
    with open(filename, "w", encoding="utf-8") as file:
        file.write(content)
    print(f"Generated {filename}")


def apply_console_macros(aslref_path: str):
    resolved_aslref_path = (
        aslref_path if os.path.isfile(aslref_path) else shutil.which(aslref_path)
    )
    if not resolved_aslref_path:
        raise Exception(
            f"Unable to find aslref in path {aslref_path}. Perhaps you need to build it?"
        )
    print(f"Using aslref path {resolved_aslref_path}")
    print("Extended macros: generating console output... ")
    pruned_latex_sources = get_latex_sources(True)
    invocations = find_console_invocations(pruned_latex_sources)
    generate_console_outputs(invocations, resolved_aslref_path)
    print("Extended macros: done")
