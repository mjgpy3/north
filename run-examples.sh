#!/usr/bin/env bash

for file in `ls ./examples/*`;
do
    echo "========= RUNNING $file: ============"
    stack run -- $file
    echo
done
