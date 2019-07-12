#!/bin/bash

# Assume run as ./publish.sh
. ./defs.sh

set -o errexit

if ! git branch | grep "^\* master$" >/dev/null
then
  echo "Cowardly refusing to publish from branch other than master."
  exit 1
fi

# In the following regex, the only significant thing is the space in second position: it detects files changed but not added to git index.
if git status --short | grep -v "^. .*$"
then
  echo "Please 'git add' all the changes you want in the publication commit before running './publish.sh'. Or 'git stash' them away."
  exit 1
fi

if git tag | grep "^$VERSION$"
then
  echo "Set (and 'git add') new version in file 'opam' before running './publish.sh'."
  exit 1
fi

if echo "$VERSION" | grep "+"
then
  echo "Cowardly refusing to publish a version containing a '+'."
  exit 1
fi

make all

git commit --allow-empty --message "Publish version $VERSION"
git tag $VERSION
git push origin master --tags

opam publish herdtools7.opam
trap "rm -r herdtools7.$VERSION" EXIT
exit 0
echo "Please check and fix files in herdtools7.$VERSION/. Press 'Enter' if correct, 'Ctrl+C' to interrupt publication."
read

opam-publish submit herdtools7.opam 
