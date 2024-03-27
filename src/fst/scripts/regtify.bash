cp ~/src/omorfi/src/morphology/roots/*.lexc src/fst/morphology/stems/
for f in ~/src/omorfi/src/morphology/stemparts/*.lexc ; do cp -v $f src/fst/morphology/stems/altstem-${f##*/}; done
cp ~/src/omorfi/src/morphology/inflection/*.lexc src/fst/morphology/affixes/ -v
mv src/fst/morphology/stems/abbreviations.lexc src/fst/morphology/stems/abbr.lexc
mv src/fst/morphology/stems/acronyms.lexc src/fst/morphology/stems/acro.lexc
mv src/fst/morphology/stems/adjectives.lexc src/fst/morphology/stems/adj.lexc
mv src/fst/morphology/stems/adverbs.lexc src/fst/morphology/stems/adv.lexc
mv src/fst/morphology/stems/conjunctions.lexc src/fst/morphology/stems/cc.lexc
cp src/fst/morphology/stems/cc.lexc src/fst/morphology/stems/cs.lexc
mv src/fst/morphology/stems/nouns.lexc src/fst/morphology/stems/noun.lexc
mv src/fst/morphology/stems/interjections.lexc src/fst/morphology/stems/interj.lexc
mv src/fst/morphology/stems/numerals.lexc src/fst/morphology/stems/numeral.lexc
mv src/fst/morphology/stems/adpositions.lexc src/fst/morphology/stems/pp.lexc
mv src/fst/morphology/stems/pronouns.lexc src/fst/morphology/stems/pron.lexc
mv src/fst/morphology/stems/proper-nouns.lexc src/fst/morphology/stems/propernoun.lexc
mv src/fst/morphology/stems/punctuation.lexc src/fst/morphology/stems/punct.lexc
mv src/fst/morphology/stems/verbs.lexc src/fst/morphology/stems/verb.lexc
