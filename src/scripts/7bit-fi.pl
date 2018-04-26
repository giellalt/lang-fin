#!/usr/bin/perl -w

use utf8;
use feature 'unicode_strings';
BEGIN {
       $| = 1;
       binmode(STDIN, ':encoding(UTF-8)');
       binmode(STDOUT, ':encoding(UTF-8)');
}
use open qw( :encoding(UTF-8) :std );

while (<>) 
{
# convert the 6 sami letters from L6 to 7bit format
s/\{/ä/g ; 
s/\}/å/g ; 
s/\|/ö/g ; 



# remove punctuation, should be omitted when a preprocessor
# is in place.
#s/[0".,:;\?\-\*\(\)]//g ;
#s/[2-9]//g ;
#s/ 1//g ;
print ;
}
