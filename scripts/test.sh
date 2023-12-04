#!/usr/bin/bash

SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
cd $SCRIPT_DIR
cd ..
export OCAMLRUNPARAM=b

dune build --profile release; dune exec aoc23 -- -d $1 -p $2 --test