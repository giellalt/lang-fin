#!/usr/bin/perl -w
#
# make-gen-test.pl
#
# Purpose:
#
# To split a created test file into the separate parts needed
# as input data and output control for testing generation.
#
# Input:	A three-field file:
#	field1:	baseform
#	field2:	infl. codes
#	field3: word form(s) corresponding to the infl. codes
#
# Output:
#	testfile for word form generation testing
#
# Author:	Sjur N. Moshagen
# Date:		$Da$
# Version:	$Id$


while (<>) {
	chop;
	@fields = split /\t/, $_, 3;	# Store the input fields in an array
	print "$fields[0]$fields[1]\n";	# Output only the relevant fields (1 & 2) concatenated
}

