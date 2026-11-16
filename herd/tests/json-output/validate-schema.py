"""Run litmus tests with herd7 and validate their JSON output."""

import argparse
import json
import re
import subprocess
import sys
from pathlib import Path

HERE = Path(__file__).resolve().parent
ROOT = HERE.parents[2]
DEFAULT_ARGS = [
    "-set-libdir",
    str(ROOT / "herd" / "libdir"),
    "-I",
    str(HERE / "libdir"),
    "-show",
    "all",
    "-showevents",
    "all",
    "-badexecs",
    "true",
    "-showinitrf",
    "true",
    "-showfinalrf",
    "true",
    "-unroll",
    "3",
]
# Options needed by fixtures whose source does not specify its variant.
FIXTURE_ARGS = {
    "C11.litmus": ["-variant", "S128"],
    "self-modifying.litmus": ["-variant", "self"],
    "fault-handler.litmus": ["-variant", "vmsa"],
    "MP+tlbi-sync.ishpteoneafp+po.litmus": ["-variant", "vmsa"],
}
BLOCK = re.compile(
    r"^JSONBEGIN ([^\r\n]+)\r?\n(.*?)^JSONEND \1[ \t]*\r?$",
    re.MULTILINE | re.DOTALL,
)


def documents(text):
    """Extract complete JSON documents from herd's stdout."""
    starts = len(re.findall(r"^JSONBEGIN ", text, re.MULTILINE))
    ends = len(re.findall(r"^JSONEND ", text, re.MULTILINE))
    blocks = list(BLOCK.finditer(text))
    if not starts and not ends:
        raise ValueError(
            "No JSON output; check whether herd options suppress this test"
        )
    if len(blocks) != starts or len(blocks) != ends:
        raise ValueError("Unmatched JSONBEGIN/JSONEND delimiters")
    return [json.loads(match[2]) for match in blocks]


def parse_args():
    parser = argparse.ArgumentParser(
        description=__doc__,
        usage="%(prog)s [--schema-version VERSION] [--herd7 EXECUTABLE] [LITMUS] [HERD_ARGS ...]",
        epilog=(
            "Put the optional litmus path before herd arguments. Without a path, "
            "discover all .litmus files under json-output. Remaining arguments "
            "are passed to herd; use -- to separate them from script options."
        ),
        allow_abbrev=False,
    )
    parser.add_argument(
        "--schema-version",
        type=int,
        default=1,
        metavar="VERSION",
        help="validate against schema-vVERSION.json (default: 1)",
    )
    parser.add_argument(
        "--herd7",
        metavar="EXECUTABLE",
        help="herd7 executable (default: local dune build, then PATH)",
    )
    argv = sys.argv[1:]
    if "--" in argv:
        separator = argv.index("--")
        argv, herd_args = argv[:separator], argv[separator + 1 :]
    else:
        herd_args = []
    args, remaining = parser.parse_known_args(argv)
    litmus = None
    if remaining and not remaining[0].startswith("-"):
        litmus = Path(remaining.pop(0))
        if not litmus.is_file():
            parser.error(f"Litmus test does not exist: {litmus}")
    args.litmus = litmus
    args.herd_args = remaining + herd_args
    args.schema = HERE / f"schema-v{args.schema_version}.json"
    if not args.schema.is_file():
        parser.error(
            f"Schema version {args.schema_version} is unavailable: {args.schema}"
        )
    return parser, args


def main():
    parser, args = parse_args()
    try:
        import jsonschema
    except ImportError:
        parser.exit(
            2,
            "This optional script requires jsonschema. Install it with:\n"
            "  python3 -m pip install jsonschema\n",
        )

    if args.herd7:
        herd7 = args.herd7
    else:
        local_herd = ROOT / "_build" / "default" / "herd" / "herd.exe"
        try:
            subprocess.run(["dune", "build", str(local_herd)], check=True)
        except subprocess.CalledProcessError as e:
            parser.error(f"Failed to build herd7, exception: {e}")
        if local_herd.is_file():
            herd7 = str(local_herd)
        else:
            parser.error(f"herd7 built, but not found at {local_herd}")

    tests = [args.litmus] if args.litmus else sorted(HERE.rglob("*.litmus"))
    if not tests:
        parser.error(f"No litmus tests found under {HERE}")

    try:
        schema = json.loads(args.schema.read_text(encoding="utf-8"))
        validator_class = jsonschema.validators.validator_for(schema)
        validator_class.check_schema(schema)
        validator = validator_class(schema)

        count = 0
        for path in tests:
            fixture_args = FIXTURE_ARGS.get(path.name, []) if not args.litmus else []
            try:
                output = subprocess.check_output(
                    [
                        herd7,
                        *DEFAULT_ARGS,
                        *fixture_args,
                        *args.herd_args,
                        # Validation always needs JSON on stdout.
                        "-o",
                        "-",
                        "-output-format",
                        "json",
                        str(path.resolve()),
                    ],
                    text=True,
                )
                for document in documents(output):
                    validator.validate(document)
                    count += 1
            except jsonschema.ValidationError as error:
                print(f"{path}: {error.json_path}: {error.message}", file=sys.stderr)
                return 1
            except (OSError, ValueError, subprocess.CalledProcessError) as error:
                print(f"{path}: {error}", file=sys.stderr)
                return 1
            print(f"OK: {path.name}")
        print(f"Validated {count} documents against {args.schema.name}")
        return 0
    except (OSError, ValueError, jsonschema.SchemaError) as error:
        print(f"Validation failed: {error}", file=sys.stderr)
        return 1


if __name__ == "__main__":
    sys.exit(main())
