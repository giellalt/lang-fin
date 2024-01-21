## Number transcriptions
Transcribing numbers to words in Finnish is not completely trivial, one
reason is that numbers in Finnish are written as compounds, regardless of
length:
123456 is *satakaksikymmentäkolmetuhattaneljäsataaviisikymmentäkuusi*.
Another limitation is that inflections can be unmarked in running text,
that is digit expression is assumed to agree the case of the phrase it is in,
e.g.
27 is *kaksikymmentäseitsemän*, and
27:lle *kahdellekymmenelleseitsemälle*
but in a phrase: "tarjosin 27 osanottajalle" 27 assumes the allative case
without marking and it is preferred grammatical form in good writing.

### Flag diacritics
Flag diacritics in number transcribing are used to control case agreement:
in Finnish numeral compounds all words agree in case except in nominative
singular where 10's exponential multipliers are in singular partitive.
* `@U.CASE.SGNOM@ ` for singular nominative agreement
* `@U.CASE.SGALL@ ` for singular allative agreement

### Morphotactics of digit strings
The morphotactics related to numbers and their transcriptions is that we
need to know the whole digit string to know how the length of whole digit
string to know what to start reading, and zeroes are not read out but
have an effect to readout.
The numerals are systematic and perfectly compositional:
the implementation of 100 000–999 999 is almost
exactly same as 100 000 000–999 000 000 and everything afterwads with the
change of word *tuhat*~*tuhatta*, *miljoona*~*miljoonaa*, *miljardia*,
*biljoonaa*, *biljardia* and so forth–that is along the long scale British
(French) system where American billion = milliard etc.
The numbers are built from ~single
word length blocks in decreasing order with the exception of zig-zagging
over numbers 11–19 where the second digit comes before first.
The rest of this documentation describes the morphotactic implementation
by the lexicon structure in descending order of magnitude with examples.

* Digits of all magnitudes examples:*
* *1:* `yksi`
* *21:* `kaksikymmentäyksi`
* *321:* `kolmesataakaksikymmentäyksi`
* *4321:* `neljätuhattakolmesataakaksikymmentäyksi`
* *54321:* `viisikymmentäneljätuhattakolmesataakaksikymmentäyksi`
* *654321:* `kuusisataaviisikymmentäneljätuhattakolmesataakaksikymmentäyksi`
* *7654321:* `seitsemänmiljoonaakuusisataaviisikymmentäneljätuhattakolmesataakaksikymmentäyksi`

Lexicon `HUNDREDSMRD` contains numbers 2-9 that need to be followed by exactly
11 digits: 200 000 000 000–999 999 999 999
this is to implement *N*sataa...miljardia...

Lexicon `CUODIMRD` contains numbers 2-9 that need to be followed by exactly
this is to implement N*sataa*...miljardia...

* hundreds of milliards examples:*
* *20000000000:* `kaksisataamiljardia`

Lexicon `HUNDREDMRD` is for numbers in range: 100 000 000 000–199 000 000 000
this is to implement *sata*...miljardia...

* hundred milliards examples:*
* *1000000000:* `satamiljardia`

Lexicon `TEENSMRD` is for numbers with 11 000 000 000–19 000 000 000
this is to implement ...N*toista*...miljardia...

Lexicon `TEENMRD` is for numbers with 11 000 000 000–19 000 000 000
this is to implement ...*N*toista...miljardia...

* teen milliards examples:*
* *1200000000:* `kaksitoistailjardia`

Lexicon `TENSMRD` is for numbers with 20 000 000 000–90 000 000 000
this is to implement ...*N*kymmentä...miljardia...

Lexicon `TENMRD` is for numbers with 10 000 000 000–10 999 999 999
this is to implement ...*kymmenen*miljardia...

* ten milliards examples:*
* *1000000000:* `kymmenenmiljardia`

Lexicon `LÅGEVMRD` is for numbers with 20 000 000 000–90 000 000 000
this is to implement ...N*kymmentä*...miljardia...

* tens of milliards examples:*
* *20000000000:* `kaksikymmentämiljardia`

Lexicon `ONESMRD` is for numbers with 1 000 000 000–9 000 000 000
this is to implement ...*N*miljardia...

Lexicon `MILJARD` is for numbers with 1 000 000 000–9 000 000 000
this is to implement ...N*miljardia*...

* milliards examples:*
* *2000000000:* `kaksimiljardia`

Lexicon `OVERMILLIONS` is for the millions *part* of numbers greater than 1 milliard

Lexicon `HUNDREDSM` contains numbers 2-9 that need to be followed by exactly
8 digits: 200 000 000–999 999 999
this is to implement *N*sataa...miljoonaa...

Lexicon `CUODIM` contains numbers 2-9 that need to be followed by exactly
this is to implement N*sataa*...miljoonaa...

* Hundreds of millions examples:*
* *200000000:* `kaksisataamiljoonaa`

Lexicon `HUNDREDM` is for numbers in range: 100 000 000–199 000 000
this is to implement *sata*...miljoonaa...

Lexicon `TEENSM` is for numbers with 11 000 000–19 000 000
this is to implement ...N*toista*...miljoonaa...

Lexicon `TEENM` is for numbers with 11 000 000–19 000 000
this is to implement ...*N*toista...miljoonaa...

* Teen millions examples:*
* *1200000:* `kaksitoistamiljoonaa`

Lexicon `TENSM` is for numbers with 20 000 000–90 000 000
this is to implement ...*N*kymmentä...miljoonaa...

Lexicon `TENM` is for numbers with 10 000 000–10 999 999
this is to implement ...*kymmenen*miljoonaa...

* Ten millions examples:*
* *2000000:* `kymmenenmiljoonaa`

Lexicon `LÅGEVM` is for numbers with 20 000 000–90 000 000
this is to implement ...N*kymmentä*...miljoonaa..

* Tens of millions examples:*
* *2000000:* `kaksikymmentämiljoonaa`

Lexicon `ONESM` is for numbers with 1 000 000–9 000 000
this is to implement ...*N*miljoonaa...

Lexicon `MILJON` is for numbers with 1 000 000–9 000 000
this is to implement ...N*miljoonaa*...

* Millions examples:*
* *200000:* `kaksisataamiljoonaa`

Lexicon `UNDERMILLION` is for numbers with 100 000–900 000 after milliards

Lexicon `OVERTHOUSANDS` is for the thousands *part* of numbers greater than 1 million

Lexicon `HUNDREDST` contains numbers 2-9 that need to be followed by exactly
5 digits: 200 000–999 999
this is to implement *N*sataa...tuhatta...

Lexicon `CUODIT` contains numbers 2-9 that need to be followed by exactly
this is to implement N*sataa*...tuhatta...

* Hundreds of thousands examples:*
* *20000:* `kaksisataatuhatta`

Lexicon `HUNDREDT` is for numbers in range: 100 000–199 000
this is to implement *sata*...tuhatta...

Lexicon `TEENST` is for numbers with 11 000–19 000
this is to implement ...N*toista*...tuhatta...

Lexicon `TEENT` is for numbers with 11 000–19 000
this is to implement ...*N*toista...tuhatta...

* Teens of thousands examples:*
* *12000:* `kaksitoistatuhatta`

Lexicon `TENST` is for numbers with 20 000–90 000
this is to implement ...*N*kymmentä...tuhatta...

Lexicon `TENT` is for numbers with 10 000 000–10 999 999
this is to implement ...*kymmenen*tuhatta...

* Ten thousands examples:*
* *10000:* `kymmenentuhatta`

Lexicon `LÅGEVT` is for numbers with 20 000–90 000
this is to implement ...N*kymmentä*...tuhatta..

* Tens of thousands examples:*
* *20000:* `kaksikymmentätuhatta`

Lexicon `ONEST` is for numbers with 1 000–9 000
this is to implement ...*N*tuhatta...

Lexicon `THOUSANDS` is for numbers with 1 000–9 000
this is to implement ...N*tuhatta*...

* Thousands examples:*
* *2000:* `kaksituhatta`
* *3456:* `kolmetuhattaneljäsataaviisikymmentäkuusi`

Lexicon `THOUSAND` is for the ones-tens-hundreds of numbers greater than thousand

Lexicon `UNDERTHOUSAND` is for numbers with 100–900 after thousands

Lexicon `HUNDREDS` contains numbers 2-9 that need to be followed by exactly
2 digits: 200–999
this is to implement *N*sataa...

Lexicon `CUODI` contains numbers 2-9 that need to be followed by exactly
this is to implement N*sataa*...

* Hundreds examples:*
* *200:* `kaksisataa`
* *345:* `kolmesataaneljäkymmentäviisi`

Lexicon `HUNDRED` is for numbers in range: 100–999

Lexicon `TEENS` is for numbers with 11–19
this is to implement ...N*toista*

Lexicon `TEEN` is for numbers with 11–19
this is to implement ...*N*toista

* Teens examples:*
* *11:* `yksitoista`
* *12:* `kaksitoista`
* *13:* `kolmetoista`

Lexicon `TENS` is for numbers with 20–90
this is to implement ...*N*kymmentä...

Lexicon `LÅGEV` is for numbers with 20–90
this is to implement ...N*kymmentä*...

* Tens examples:*
* *20:* `kaksikymmentä`
* *34:* `kolmekymmentäneljä`

Lexicon `JUSTTEN` is for number 10
this is to implement ...*kymmenen*

* Ten examples:*
* *10:* `kymmenen`

Lexicon `ONES` is for numbers with 1–9
this is to implement yksi, kaksi, kolme..., yhdeksän

* Ones examples:*
* *1:* `yksi`
* *2:* `kaksi`
* *3:* `kolme`

Lexicon `ZERO` is for number 0
nolla

* Zero examples:*
* *0:* `nolla`

Lexicon `LOPPU` is to implement potential case inflection with a colon.

* Digits with explicit cases examples:*
* *1\:lle:* `yhdelle`
*Note:* accepting or rejecting case inflected digit strings without explicit
suffix can be changed here.

* * *

<small>This (part of) documentation was generated from [src/fst/transcriptions/transcriptor-numbers-digit2text.lexc](https://github.com/giellalt/lang-fin/blob/main/src/fst/transcriptions/transcriptor-numbers-digit2text.lexc)</small>

---

