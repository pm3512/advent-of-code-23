#!/usr/bin/bash

SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
DAY=$1
PREV_DAY=$((DAY-1))
NEW_DIR_NAME=day_${DAY}

cd $SCRIPT_DIR
cd ../lib
mv curday.ml day${PREV_DAY}.ml
cp sampleday.ml curday.ml
cd ../inputs
mkdir $NEW_DIR_NAME
cd $NEW_DIR_NAME
touch a_test.txt a.txt b_test.txt b.txt
cd ../..
dune build
