#!/bin/bash

# This is a shell script that will call the actual test runner with the
# specified transducer. This determines also the set of yaml test files looped
# over by the test runner.

###### Variables: #######
transducer=gt-norm

# source ./run-yaml-testcases.sh $transducer

lexcfail=0

for file in ${srcdir}/../../../src/morphology/*.lexc \
			${srcdir}/../../../src/morphology/*/*.lexc; do
	fsts=$(grep '^\!\!€[^ :]' $file | cut -d'€' -f2 | cut -d':' -f1 | sort -u)
	tests=$(grep '^\!\!€ ' $file | cut -d'€' -f2 | cut -d':' -f1 | sort -u)
	if [ "$fsts" == "" -a "$tests" == "" ]; then
		echo "* Warning: the LexC file"
		echo "$file"
		echo "doesn't contain any tests - SKIPPED"
		echo
	elif [ "$fsts" == "" -a ! "$tests" == "" ]; then
#		echo "$file has tests, but no fst specified - defaulting to gt-norm."
		echo "$file has tests, but no fst specified - SKIPPED"
#		source ./run-yaml-testcases.sh $transducer $file
	else
		for fst in $fsts; do
			source ./run-yaml-testcases.sh $fst $file
			let "lexcfail += $Fail"
		done
	fi
done

# At least one of the Xerox or HFST tests failed:
if [ "$lexcfail" -ge "1" ]; then
    exit 1
fi
