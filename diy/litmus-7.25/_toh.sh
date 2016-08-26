FILE=$1
cat <<EOF
static void ass(FILE *out) {
EOF
sed -e 's|"|\\"|g' $1 | awk '{printf("  fprintf(out,\"%%s\\n\",\"%s\");\n",$0) }'
cat <<EOF
}
EOF
