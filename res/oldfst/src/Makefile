# This is a makefile that builds the Northern Sami morphological parser
# *****************************************************************

XFST = xfst
LEXC = lexc
TWOLC = twolc

LEX = fin-lex.txt adj-fin-lex.txt \
	adv-fin-lex.txt noun-fin-lex.txt noun-fin-morph.txt \
	verb-fin-lex.txt verb-fin-morph.txt \
	closed-fin-lex.txt pp-fin-lex.txt propernoun-fin-lex.txt \
	abbr-fin-lex.txt


# The ultimate goal is to build ifin.fst, the generator
# This goal depends on fin.save being up to date.

xifin.fst: ../bin/xifin.fst
../bin/xifin.fst: ../bin/xfin.fst ../bin/fin.save ../bin/tok.fst \
	../bin/allcaps.fst
	@echo
	@echo "*** Building the inverse ifin.fst ***"
	@echo
	@printf "load ../bin/fin.save \n\
	invert net \n\
	save stack ../bin/ifin.fst \n\
	quit \n" > /tmp/ifin-fst-script
	$(XFST) < /tmp/ifin-fst-script
	@rm -f /tmp/ifin-fst-script


# The first goal is to build fin.fst, the analyzer.
# This goal depends on fin.save and caseconv.fst being up to date

xfin.fst: ../bin/xfin.fst
../bin/xfin.fst: ../bin/fin.save ../bin/caseconv.fst ../bin/allcaps.fst \
	../bin/tok.fst
	@echo
	@echo "*** Building fin.fst ***" ;
	@echo
	@printf "read regex @\"../bin/fin.save\" .o. \
	@\"../bin/caseconv.fst\" ; \n\
	save stack ../bin/fin.fst \n\
	quit \n" > /tmp/fin-fst-script
	$(XFST) < /tmp/fin-fst-script
	@rm -f /tmp/fin-fst-script

# The second goal is to build the caseconv.fst file
# This goal depends on case.regex

caseconv.fst: ../bin/caseconv.fst
../bin/caseconv.fst: case.regex
	@echo
	@echo "*** Building caseconv.fst ***" ;
	@echo
	@printf "read regex < case.regex \n\
	save stack ../bin/caseconv.fst \n\
	quit \n" > /tmp/caseconv-script
	$(XFST) < /tmp/caseconv-script
	@rm -f /tmp/caseconv-script


# This goal depends on allcaps.xfst

allcaps.fst: ../bin/allcaps.fst
../bin/allcaps.fst: allcaps.xfst
	@echo
	@echo "*** Building allcaps.fst ***" ;
	@echo
	@printf "source allcaps.xfst \n\
	save stack ../bin/allcaps.fst \n\
	quit \n" > /tmp/tok-script
	$(XFST) < /tmp/tok-script
	@rm -f /tmp/tok-script


# The third goal is to build fin.save
# This goal depends on twol-fin.bin and a bunch of lexicon files

fin.save: ../bin/fin.save
../bin/fin.save: ../bin/twol-fin.bin $(LEX)
	@echo
	@echo "*** Building fin.save ***" ;
	@echo
	printf "compile-source $(LEX) \n\
	read-rules ../bin/twol-fin.bin \n\
	compose-result \n\
	save-result ../bin/fin.save \n\
	quit \n" > /tmp/fin-save-script
	$(LEXC) < /tmp/fin-save-script
	rm -f /tmp/fin-save-script


# The fourth goal is to build twol-fin.bin
# This goal depends on twol-fin.txt

twol-fin.bin: ../bin/twol.fin.bin
../bin/twol-fin.bin: twol-fin.txt
	@echo
	@echo "*** Building twol-fin.bin ***" ;
	@echo
	@printf "read-grammar twol-fin.txt \n\
	compile \n\
	save-binary ../bin/twol-fin.bin \n\
	quit \n" > /tmp/twol-fin-script
	$(TWOLC) < /tmp/twol-fin-script
	@rm -f /tmp/twol-fin-script

# Here we include a preprocessor.
# This goal depends on tok.txt

tok.fst: ../bin/tok.fst 
../bin/tok.fst: tok.txt
	@echo
	@echo "*** Building the tokenizer tok.fst ***" ;
	@echo
	@printf "source tok.txt \n\
	save stack ../bin/tok.fst \n\
	quit \n" > /tmp/tok-script
	$(XFST) < /tmp/tok-script
	@rm -f /tmp/tok-script


# Here we make the abbrevation file for our current preprocessor, 
# the perl-based preprocess (located in the script catalogue)

empty:=
comma:=,
space:=$(empty) $(empty)
ABBRSRCS=$(subst $(space),$(comma),$(LEX))

scripts=$(HOME)/gt/script

abbr: ../bin/abbr.txt
../bin/abbr.txt: $(scripts)/abbr-extract.pl $(scripts)/langTools/Util.pm $(LEX)
	@echo
	@echo "*** Extracting abbreviations from abbr-fin-lex.txt to abbr.txt ***" ;
	@echo
	@perl -I $(scripts) $(scripts)/abbr-extract.pl \
		--output=$@ \
		--abbr_lex=abbr-fin-lex.txt \
		--lex=$(ABBRSRCS)


# "make clean" is there to remove the binary files at will.

clean:
	@rm -f ../bin/*.fst ../bin/*.save ../bin/*.bin
#	@mv ../bin/fin.fst ../bin/fin.old




