#!/usr/bin/perl -w
#
# make-ana-test.pl
#
# Purpose:
#
# To create a test file for morphological analysis by spitting
# out all the possible word forms in a sorted order.
#
# Input:	A three-field file:
#	field1:	baseform
#	field2:	infl. codes
#	field3: word form(s) corresponding to the infl. codes
#
# Output:
#	Recasted test file for morphological analysis: needs to
#	be further sorted and uniq-ued, which is probably
#	best done using the command line tools (ie. in the Makefile).
#
# Author:	Sjur N. Moshagen
# Date:		$Da$
# Version:	$Id$

while (<>) {	# as long as there are lines in the file, for each line:
	chop;						# Remove the newline char
	@parts = split /\t/, $_;	# Get all the fields in the line
	$wfs = $parts[2];			# Store (all) the word forms
	@wfs = split /,/, $wfs;		# Get all the word forms, one or more
	foreach $wf (@wfs) {		# Print all word forms, one on each line
		print "$wf\t$parts[0]$parts[1]\n"; # with the baseform and the code
								# strung together appended with a tab
	}
}

