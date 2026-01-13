#!/usr/bin/env bash

for file in `find src -name "*.hs"`;
do
    echo "========= RUNNING $file: ============"
    stack exec -- fourmolu $file -i
    echo
done

for file in `find test -name "*.hs"`;
do
    echo "========= RUNNING $file: ============"
    stack exec -- fourmolu $file -i
    echo
done
