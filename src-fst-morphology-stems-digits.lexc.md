# Digits and such expressions
Digit-strings are used in place of numerals. They inflect with colon, like
acronyms, and compound with hyphen only.

Digits are constructed as several cyclic structures: integers, decimals or
roman numerals.
Zero alone works quite differently:

* Digit zero examples:*
* *0:* `0+Num+Sg+Nom`

* int digit loop examples:*
* *13:* `13+Num+Card+Sg+Nom`
* *100:* `100+Num+Card+Sg+Nom`
* *0000005550000000:* `0000005550000000+Num+Card+Sg+Nom`

**LEXICON ARABICLOOP_pirinen ** essentially allows any number-sign combination, but is like the other lgs

**LEXICON ARABICLOOP_pirinen ** is for entries not looping back 

The digit strings that end in 10 to 12 + 6n 0's are inflected alike:

* int digit milliard loop examples:*
* *1000000000:* `1000000000+Num+Card+Sg+Nom`
* *300000000000:* `300000000000+Num+Card+Sg+Nom`
* *123456000000000000000000:* `123456000000000000000000+Num+Card+Sg+Nom`

The digit strings that end in 6 to 9 + 6n 0's are inflected alike:

* int digit million loop examples:*
* *1000000:* `1000000+Num+Card+Sg+Nom`
* *300000000:* `300000000+Num+Card+Sg+Nom`
* *123456000000000000000:* `123456000000000000000+Num+Card+Sg+Nom`

Decimal digit strings start with any number of digits 0 to 9, followed
by decimal separator comma. The decimal dot may be allowed as substandard
variant.

* decimal digit loop examples:*
* *1,0:* `1,0+Num+Sg+Nom`
* *314,1:* `314,1+Num+Sg+Nom`

The decimal digit strings end in any number of digits 0 to 9, inflected
along the last part.

* decimal digit loop more examples:*
* *3,141:* `3,141+Num+Sg+Nom`
* *123,345:* `123,345+Num+Sg+Nom`

The decimal digit strings with dot may be allowed as sub-standard option
with respective analysis.

# Roman numerals with inflection
Roman numerals are composed the symbols M, D, C, L, X, V, I in ascending
scale and some combinations, they denote ordinal numbers and inflect like
ones.

## Main lexicon for roman digits
This lexicon divides into four groups

* roman numeral examples:*
* *MM:* `MM+Num+Ord+Sg+Nom`
* *MCXI:* `MCXI+Num+Ord+Sg+Nom`
* *CMXCIX:* `CMXCIX+Num+Ord+Sg+Nom`

## Roman numerals according to digital class, one by one
### Roman thousands
Thousands can be followed by any of other parts

* roman numeral thousand examples:*
* *MII:* `MII+Num+Ord+Sg+Nom`
* *MCCCXXII:* `MCCCXXII+Num+Ord+Sg+Nom`

### Roman hundreds
Hundreds can be followed by anything but thousands:

* roman numeral hundred examples:*
* *CXXI:* `CXXI+Num+Ord+Sg+Nom`
* *DXXIV:* `DXXIV+Num+Ord+Sg+Nom`

### Roman tens
Tens can be followed by ones:

* roman numeral ten examples:*
* *XIX:* `XIX+Num+Ord+Sg+Nom`
* *XXVII:* `XXVII+Num+Ord+Sg+Nom`

### Roman ones
Ones come alone

* roman numerals one to nine examples:*
* *IX:* `IX+Num+Ord+Sg+Nom`
* *VIII:* `VIII+Num+Ord+Sg+Nom`
* *II:* `II+Num+Ord+Sg+Nom`

* * *

<small>This (part of) documentation was generated from [src/fst/morphology/stems/digits.lexc](https://github.com/giellalt/lang-fin/blob/main/src/fst/morphology/stems/digits.lexc)</small>
