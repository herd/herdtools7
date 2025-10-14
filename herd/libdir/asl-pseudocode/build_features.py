"""Build the list of features from the released json file.
"""

# SPDX-FileCopyrightText: Copyright 2022-2025 Arm Limited and/or its affiliates <open-source-office@arm.com>
# SPDX-License-Identifier: BSD-3-Clause

import argparse
import json
from pathlib import Path
from typing import Any, Iterable


def load_json(filename: str) -> Any:
    """Load json into a python object."""
    with open(filename, "r", encoding="utf-8") as f:
        return json.load(f)


def build_parser() -> argparse.ArgumentParser:
    """Build command line arguments."""
    parser = argparse.ArgumentParser(
        description="Process json features files as released by Arm into raw asl files."
    )

    parser.add_argument(
        "-o",
        "--output",
        action="store",
        type=Path,
        default=Path.cwd(),
        help="where all pseudocode should be written to. If a directory is"
        "passed, the output will be written in a 'features.asl' file in it. If"
        "a file is passed, it will be overwritten.",
    )

    parser.add_argument(
        "path",
        metavar="PATH",
        type=Path,
        help="the path to the features.json file to parse.",
    )

    return parser


def extract_features_names(obj) -> Iterable[str]:
    """Extract features names from feature list."""
    return sorted(p["name"] for p in obj["parameters"] if p["name"].startswith("FEAT"))


FEATURES_TEMPLATE = """
// Copyright (c) 2010-2025 Arm Limited or its affiliates. All rights reserved.
// This document is Non-Confidential. This document may only be used and
// distributed in accordance with the terms of the agreement entered into by
// Arm and the party that Arm delivered this document to.

// SPDX-FileCopyrightText: Copyright 2022-2025 Arm Limited and/or its affiliates <open-source-office@arm.com>
// SPDX-License-Identifier: BSD-3-Clause

// More information can be found in notice.html or
//    https://developer.arm.com/documentation/ddi0602/latest/Proprietary-Notice

// This document was automatically extracted from the XML files distributed at
//    https://developer.arm.com/downloads/-/exploration-tools
// by a script authored by Hadrien Renaud <hadrien.renaud.22@ucl.ac.uk> or 
// <hadrien.renaud2@arm.com> and available at bundler.py or
//    https://github.com/herd/herdtools7/blob/master/herd/libdir/asl-pseudocode/bundler.py


type Feature of enumeration {{
{content}}};
"""


def get_output_content(feature_names: Iterable[str]) -> str:
    """Get the content output."""
    return FEATURES_TEMPLATE.format(
        content="".join(f"  {name},\n" for name in feature_names)
    )


def write_output(output_arg: Path, output_content: str):
    """Write the content in the correct file."""
    output = output_arg.absolute()

    if output.is_dir():
        o_file = output / "features.asl"
    else:
        o_file = output

    with open(o_file, "w", encoding="utf8") as f:
        f.write(output_content)


def main():
    """Starting point."""
    args = build_parser().parse_args()
    obj = load_json(args.path)
    feature_names = extract_features_names(obj)
    output_file_content = get_output_content(feature_names)
    write_output(args.output, output_file_content)


if __name__ == "__main__":
    main()
