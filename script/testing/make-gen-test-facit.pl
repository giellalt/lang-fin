#!/usr/bin/perl -w
#
# make-gen-test-facit.pl
#
# Purpose:
#
# To create the expected output from a generation test run,
# to compare the test results with. Based on the comparison,
# one can make further reports on the success of the test run
#
# Input:	A three-field file:
#	field1:	baseform
#	field2:	infl. codes
#	field3: word form(s) corresponding to the infl. codes
#
# Output:
#	Control file containing the expected results
#
# Author:	Sjur N. Moshagen
# Date:		$Da$
# Version:	$Id$

while (<>) {
	chop;
	@parts = split /\t/, $_;	# Get all the fields in the input
	$wfs = $parts[2];			# We are only interested in the last field
	@wfs = split /,/, $wfs;		# Get all the word forms, one or more
	foreach $wf (@wfs) {		# Print all word forms, one on each line
		print "$wf\n";
	}
}

