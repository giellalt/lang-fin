cp ~/src/omorfi/src/morphology/roots/*.lexc src/morphology/stems/
for f in ~/src/omorfi/src/morphology/stemparts/*.lexc ; do cp -v $f src/morphology/stems/altstem-${f##*/}; done
cp ~/src/omorfi/src/morphology/inflection/*.lexc src/morphology/affixes/ -v
mv src/morphology/stems/abbreviations.lexc src/morphology/stems/abbr.lexc
mv src/morphology/stems/acronyms.lexc src/morphology/stems/acro.lexc
mv src/morphology/stems/adjectives.lexc src/morphology/stems/adj.lexc
mv src/morphology/stems/adverbs.lexc src/morphology/stems/adv.lexc
mv src/morphology/stems/conjunctions.lexc src/morphology/stems/cc.lexc
cp src/morphology/stems/cc.lexc src/morphology/stems/cs.lexc
mv src/morphology/stems/nouns.lexc src/morphology/stems/noun.lexc
mv src/morphology/stems/interjections.lexc src/morphology/stems/interj.lexc
mv src/morphology/stems/numerals.lexc src/morphology/stems/numeral.lexc
mv src/morphology/stems/adpositions.lexc src/morphology/stems/pp.lexc
mv src/morphology/stems/pronouns.lexc src/morphology/stems/pron.lexc
mv src/morphology/stems/proper-nouns.lexc src/morphology/stems/propernoun.lexc
mv src/morphology/stems/punctuation.lexc src/morphology/stems/punct.lexc
mv src/morphology/stems/verbs.lexc src/morphology/stems/verb.lexc
