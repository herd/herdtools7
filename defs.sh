set -o errexit

VERSION=$(cat VERSION.txt)
REV=$(git rev-parse HEAD 2>/dev/null || echo exported)
