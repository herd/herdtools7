FILE=$1
cat <<EOF
static void ass(FILE *out) {
EOF
sed -e 's|"|\\"|g' $1 | awk '{printf("  emit_string(out,\"%s\\n\");\n",$0) }'
cat <<EOF
}
EOF
