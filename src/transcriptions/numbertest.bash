#!/bin/bash
seq 1 1000000 > numbers-one-to-million
seq 1 1000 1000000000 > numbers-one-to-milliard-step-thousand
seq 1 100000 100000000000 > numbers-one-to-milliards-step-hundred-thousand

cat numbers-* | while read l ; do
    echo $l | hfst-lookup -q transcriptor-numbers-digit2text.filtered.lookup.hfstol
done > digits2texts.test

if fgrep '+?' digits2texts.test ; then
    echo found missing transcriptions
    exit 1
fi
