
# Continuation lexicons for Finnish abbreviations

Abbreviations are shortened forms that do not inflect. They have whatever
classification they would have if they were read out, mostly that of nouns.
Lot of abbreviations end in a full stop, which may complicate analysis and
tokenisation in real-world applications

* Abbreviations examples:*
* *eaa.:* `eaa.+N`
* *esim.:* `esim.+N`
* ★*esim:* `esim+N` (is not standard language)
Some ar less classified
* *ab:* `ab+ABBR`

* `KalPa+N:KalPa  ab-nodot-noun-itrab  ; `+N: Kalmiston pallo
* `MyPa+N:MyPa   ab-nodot-noun-itrab   ; `+N: Myllykosken Pallo
* `RoPS+N:RoPS   ab-nodot-noun-itrab  ; `+N: Rovaniemen palloseura
* `jssk+N:jssk   ab-nodot-noun-itrab   ; `+N: jossakin (somewhere)
* `jstk+N:jstk   ab-nodot-noun-itrab  ; `+N: jostakin (from)
* `jtak+N:jtak   ab-nodot-noun-itrab   ; `+N: jotakin (to)
* `k%:lo+N:k%:lo   ab-dot-noun-trnumab   ; `+N: kello

* * *

<small>This (part of) documentation was generated from [src/fst/stems/fin-abbreviations.lexc](https://github.com/giellalt/lang-fin/blob/main/src/fst/stems/fin-abbreviations.lexc)</small>

---

