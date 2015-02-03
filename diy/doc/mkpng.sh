#! /bin/sh -
BASE=$1
COM=$2
$COM -Tpng $BASE.dot -o $BASE.png
