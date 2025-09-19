


export ACL2_IMAGES=$(CURDIR)/bin
export ACL2_IMAGE_SRC_DIR=$(CURDIR)/image-src
export ACL2_PROJECTS=$(CURDIR)/PROJECT_DIRS

all : bin/acl2asl

bin :
	mkdir bin

bin/acl2asl : bin
	cert.pl -g acl2asl

# FIXME -- at the moment it generally won't work to build both acl2asl
# and the manual in a single make command, i.e. `make all manual -j 2`,
# because this would launch two cert.pl processes in parallel and
# potentially try to certify the same book twice in parallel.
.PHONY: manual

manual :
	cert.pl manual.lisp

.PHONY: git_status
git_status :
	@mkdir -p tmp
	@cert.pl manual.lisp \
	    --quiet --no-build \
	    --write-sources tmp/git_source_deps \
	    --write-otherdeps tmp/git_other_deps
	@cat tmp/git_source_deps tmp/git_other_deps \
	  | fgrep -v '/acl2/books/' | tr '\n' '\0' \
	  | xargs -0 git status -u --
