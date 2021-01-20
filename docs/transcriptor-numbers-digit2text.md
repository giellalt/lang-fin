
## Number transcriptions
Transcribing numbers to words in Finnish is not completely trivial, one
reason is that numbers in Finnish are written as compounds, regardless of
length:
123456 satakaksikymmentäkolmetuhattaneljäsataaviisikymmentäkuusi
Another limitation is that inflections can be unmarked in running text,
that is digit expression is assumed to agree the case of the phrase it is in,
e.g.
27 kaksikymmentäseitsemän
27:lle kahdellekymmenelleseitsemälle
but in a phrase: "tarjosin 27 osanottajalle" 27 assumes the allative case
without marking and it is preferred grammatical form in good writing.

The morphotactics related to numbers and their transcriptions is that we
need to know the whole digit string to know how the length of whole digit
string to know what to start reading, and zeroes are not read out but
have an effect to readout.
The numerals are systematic and perfectly compositional:
the implementation of 100 000–999 999 is almost
exactly same as 100 000 000–999 000 000 and everything afterwads with the
change of word *tuhat*~*tuhatta*, *miljoona*~*miljoonaa*, *miljardia*,
*biljoonaa*, *biljardia* and so forth. The numbers are built from ~single
word length blocks in decreasing order with the exception of zig-zagging
over numbers 11–19 where the second digit comes before first.
The rest of this doc is the lexicons in descendign order of magnitude.

lexicon HUNDREDSMRD contains numbers 2-9 that need to be followed by exactly
11 digits: 200 000 000 000–999 999 999 999
this is to implement *N*sataa...miljardia...

lexicon CUODIMRD contains numbers 2-9 that need to be followed by exactly
this is to implement N*sataa*...miljardia...

lexicon HUNDREDMRD is for numbers in range: 100 000 000 000–199 000 000 000
this is to implement *sata*...miljardia...


lexicon TEENSMRD is for numbers with 11 000 000 000–19 000 000 000
this is to implement ...N*toista*...miljardia...

lexicon TEENMRD is for numbers with 11 000 000 000–19 000 000 000
this is to implement ...*N*toista...miljardia...


lexicon TENSMRD is for numbers with 20 000 000 000–90 000 000 000
this is to implement ...*N*kymmentä...miljardia...

lexicon TENMRD is for numbers with 10 000 000 000–10 999 999 999
this is to implement ...*kymmenen*miljardia...

lexicon LÅGEVMRD is for numbers with 20 000 000 000–90 000 000 000
this is to implement ...N*kymmentä*...miljardia...

lexicon ONESMRD is for numbers with 1 000 000 000–9 000 000 000
this is to implement ...*N*miljardia...

lexicon MILJARD is for numbers with 1 000 000 000–9 000 000 000
this is to implement ...N*miljardia*...


lexicon OVERMILLIONS is for the millions *part* of numbers greater than 1 milliard


lexicon HUNDREDSM contains numbers 2-9 that need to be followed by exactly
8 digits: 200 000 000–999 999 999
this is to implement *N*sataa...miljoonaa...

lexicon CUODIM contains numbers 2-9 that need to be followed by exactly
this is to implement N*sataa*...miljoonaa...

lexicon HUNDREDM is for numbers in range: 100 000 000–199 000 000
this is to implement *sata*...miljoonaa...


lexicon TEENSM is for numbers with 11 000 000–19 000 000
this is to implement ...N*toista*...miljoonaa...

lexicon TEENM is for numbers with 11 000 000–19 000 000
this is to implement ...*N*toista...miljoonaa...


lexicon TENSM is for numbers with 20 000 000–90 000 000
this is to implement ...*N*kymmentä...miljoonaa...

lexicon TENM is for numbers with 10 000 000–10 999 999
this is to implement ...*kymmenen*miljoonaa...

lexicon LÅGEVM is for numbers with 20 000 000–90 000 000
this is to implement ...N*kymmentä*...miljoonaa..

lexicon ONESM is for numbers with 1 000 000–9 000 000
this is to implement ...*N*miljoonaa...

lexicon MILJON is for numbers with 1 000 000–9 000 000
this is to implement ...N*miljoonaa*...


lexicon UNDERMILLION is for numbers with 100 000–900 000 after milliards

lexicon OVERTHOUSANDS is for the thousands *part* of numbers greater than 1 million

lexicon HUNDREDST contains numbers 2-9 that need to be followed by exactly
5 digits: 200 000–999 999
this is to implement *N*sataa...tuhatta...

lexicon CUODIT contains numbers 2-9 that need to be followed by exactly
this is to implement N*sataa*...tuhatta...

lexicon HUNDREDT is for numbers in range: 100 000–199 000
this is to implement *sata*...tuhatta...

lexicon TEENST is for numbers with 11 000–19 000
this is to implement ...N*toista*...tuhatta...


lexicon TEENT is for numbers with 11 000–19 000
this is to implement ...*N*toista...tuhatta...


lexicon TENST is for numbers with 20 000–90 000
this is to implement ...*N*kymmentä...tuhatta...


lexicon TENT is for numbers with 10 000 000–10 999 999
this is to implement ...*kymmenen*tuhatta...

lexicon LÅGEVT is for numbers with 20 000–90 000
this is to implement ...N*kymmentä*...tuhatta..

lexicon ONEST is for numbers with 1 000–9 000
this is to implement ...*N*tuhatta...

lexicon THOUSANDS is for numbers with 1 000–9 000
this is to implement ...N*tuhatta*...

*Thousands examples:*
* *2000:* `kaksituhatta`
* *3456:* `kolmetuhattaneljäsataaviisikymmentäkuusi`

lexicon THOUSAND is for the ones-tens-hundreds of numbers greater than thousand


lexicon UNDERTHOUSAND is for numbers with 100–900 after thousands

lexicon HUNDREDS contains numbers 2-9 that need to be followed by exactly
2 digits: 200–999
this is to implement *N*sataa...

lexicon CUODI contains numbers 2-9 that need to be followed by exactly
this is to implement N*sataa*...

*Hundreds examples:*
* *200:* `kaksisataa`
* *345:* `kolmesataaneljäkymmentäviisi`

lexicon HUNDRED is for numbers in range: 100–999

lexicon TEENS is for numbers with 11–19
this is to implement ...N*toista*

lexicon TEEN is for numbers with 11–19
this is to implement ...*N*toista

*Teens examples:*
* *11:* `yksitoista`
* *12:* `kaksitoista`
* *13:* `kolmetoista`



lexicon TENS is for numbers with 20–90
this is to implement ...*N*kymmentä...

lexicon LÅGEV is for numbers with 20–90
this is to implement ...N*kymmentä*...

*Tens examples:*
* *20:* `kaksikymmentä`
* *34:* `kolmekymmentäneljä`

lexicon JUSTTEN is for number 10
this is to implement ...*kymmenen*

*Ten examples:*
* *1:* `kymmenen`

lexicon ONES is for numbers with 1–9
this is to implement yksi, kaksi, kolme..., yhdeksän

*Ones examples:*
* *1:* `yksi`
* *2:* `kaksi`
* *3:* `kolme`

lexicon ZERO is for number 0
nolla

*Zero examples:*
* *0:* `nolla`
