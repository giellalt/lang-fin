#!/usr/bin/perl -w
#
# Purpose:
#
# To create a file for testing paradigm generation by combining a tag list
# and a word (supposedly the base form).
#
#	ARG1:	input file with inflectional tags
#	ARG2:	the base form of the word we want to inflect
#
# Output file:
#
#	Baseform+tags for all tags given in the tag file
#	This can directly be given to xfst for word form generation
#
# Author: Sjur N. Moshagen
# Date:		$d
# Version:	$i

$baseform = "";
if (($file, $baseform) = @ARGV) {
	open (TAGFILE,   $file) or die "Can't find file $file: $!\n";
} else {
	print "Too many arguments in input! Should be only:\n";
	print "Code file, and the test word in base form.";
}

# print STDERR "Baseform: $baseform\n";
if ($baseform eq "") {
	print STDERR "*** Word to inflect not specified!\n";
	die "*** Please type: make n-para WORD=wordToInflect\n";
}

chop (@codes  = <TAGFILE>);

foreach $code (@codes) {
	print "$baseform$code\n";
}

close TAGFILE;

