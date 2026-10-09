import jsonschema
from pathlib import Path
import yaml

def load_and_validate(file, schema):
    try:
        with open(file, "r") as f:
            yaml_str = f.read()
        yaml_obj = yaml.safe_load(yaml_str)
        jsonschema.validate(instance=yaml_obj, schema=schema)
        return True
    except Exception as e:
        print(e)
        return False

def get_schema(schema_filename):
    with open(schema_filename, "r") as f:
        schema_str = f.read()
    schema = yaml.safe_load(schema_str)
    print(f"loaded schema file {schema_filename}")
    return schema

def main():
    script_dir = Path(__file__).resolve().parent
    schema_filename = script_dir / "schema.yaml"
    schema = get_schema(schema_filename)

    yaml_files = list((script_dir / "tests").rglob("*.yaml"))
    number_of_yaml_files = len(yaml_files)
    print(f"found {number_of_yaml_files} yaml files")
    number_of_failed_validations = 0
    for yaml_file in yaml_files:
        success = load_and_validate(yaml_file, schema)
        if not success:
            print(f"Error: {yaml_file} could not be validated against {schema_filename}")
            number_of_failed_validations += 1
    print(f"Validated {number_of_yaml_files-number_of_failed_validations} out of {number_of_yaml_files} yaml files")
    if number_of_failed_validations != 0:
        print(f"Error: validation failed on {number_of_failed_validations} files")
        exit(1)

if __name__ == "__main__":
    main()
