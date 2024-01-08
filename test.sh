#! /bin/bash
SOURCES=castellan.el
TESTS=castellan-test.el

FILES=""

for file in ${SOURCES} ${TESTS}; do
    FILES="-l ${file} ${FILES}"
done

emacs --batch -L src -l ert $FILES -f ert-run-tests-batch-and-exit
