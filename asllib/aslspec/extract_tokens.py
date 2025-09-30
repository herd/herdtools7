#!/usr/bin/env python3
"""
Extracts token strings from SpecLexer.mll and generates TokenStrings.ml
"""

import re
import sys


def extract_tokens(lexer_file):
    """
    Extract token mappings from lexer file.
    For a lexer line matching a pattern like
      | "string_literal" { token_name }
    the generated OCaml function contains a line like
      | token_name -> "string_literal"

    This pattern is enough to include all keywords, but not things like
    punctuation, which is sufficient for now.
    """
    pattern = r'^\s*\|\s*"([^"]+)"\s*\{\s*([A-Z_0-9]+)\s*\}'
    tokens = []
    with open(lexer_file, "r") as f:
        for line in f:
            match = re.match(pattern, line)
            if match:
                string_literal, token_name = match.groups()
                tokens.append((token_name, string_literal))
    return tokens


def generate_module(tokens):
    """
    Generate the TokenStrings.ml module content
    """
    lines = [
        "(* Auto-generated from SpecLexer.mll - DO NOT EDIT *)",
        "",
        "let string_of_token = function",
    ]
    for token_name, string_literal in tokens:
        lines.append(f'  | SpecParser.{token_name} -> "{string_literal}"')
    lines.append('  | _ -> failwith "string_of_token: unsupported token"')
    return "\n".join(lines)


if __name__ == "__main__":
    lexer_file = sys.argv[1] if len(sys.argv) > 1 else "SpecLexer.mll"
    tokens = extract_tokens(lexer_file)
    module_content = generate_module(tokens)
    print(module_content)
