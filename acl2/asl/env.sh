#!/bin/echo "Source this file in bash or zsh"

if [[ ! -d $ACL2_DIR ]]; then
    echo "Must set environment variable ACL2_DIR to the directory of an ACL2 source tree"
    return 1;
fi

if [[ ! -d $ACL2ASL_DIR ]]; then
    echo "Must set environment variable ACL2ASL_DIR to the asllib/acl2 subdirectory of a herdtools7 source tree"
    return 1;
fi


export ACL2=${ACL2_DIR}/saved_acl2
alias acl2=${ACL2}
export ACL2_SYSTEM_BOOKS=${ACL2_DIR}/books
export ACL2_IMAGES=${ACL2ASL_DIR}/bin
export ACL2_IMAGE_SRC_DIR=${ACL2ASL_DIR}/image-src
export ACL2_PROJECTS=${ACL2ASL_DIR}/PROJECT_DIRS
export PATH=${ACL2_SYSTEM_BOOKS}/build:${ACL2_IMAGES}:$PATH




