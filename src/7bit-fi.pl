#!/usr/bin/perl -w

use utf8 ;

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
