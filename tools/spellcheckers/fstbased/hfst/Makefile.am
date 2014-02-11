## Process this file with automake to produce Makefile.in

## Copyright (C) 2011 Samediggi

## This program is free software: you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.

## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.

## You should have received a copy of the GNU General Public License
## along with this program.  If not, see <http://www.gnu.org/licenses/>.

# Local modifications of the speller transducer:
#   - replace the copy command with a sequence of whatever is desired
# Regular processing of the speller transducer will proceed after the
# local processing.

# (Un)comment one of the following two:
# EITHER: Add weights (modify as needed):
#$(GT_SPELLER_HFST): ../$(GT_SPELLER_HFST)
#	$(AM_V_GEN)cat $< \
#		| $(HFST_REWEIGHT) -a 0.1 -S +Cmp   \
#		| $(HFST_REWEIGHT) -a 0.1 -S +Der   \
#		| $(HFST_REWEIGHT) -a 0.1 -S +Der1  \
#		| $(HFST_REWEIGHT) -a 0.1 -S +Der2  \
#		| $(HFST_REWEIGHT) -a 0.1 -S +Der3  \
#		| $(HFST_REWEIGHT) -a 0.1 -S +Der4  \
#		| $(HFST_REWEIGHT) -a 0.2 -S +PxSg1 \
#		| $(HFST_REWEIGHT) -a 0.2 -S +PxSg2 \
#		| $(HFST_REWEIGHT) -a 0.2 -S +PxSg3 \
#		| $(HFST_REWEIGHT) -a 0.2 -S +PxPl1 \
#		| $(HFST_REWEIGHT) -a 0.2 -S +PxPl2 \
#		| $(HFST_REWEIGHT) -a 0.2 -S +PxPl3 \
#		| $(HFST_REMOVE_EPSILONS) \
#		| $(HFST_PUSH_WEIGHTS) -p end \
#		| $(HFST_DETERMINIZE) \
#		| $(HFST_MINIMIZE) $(HFST_FLAGS) \
#		-o $@

SURFWEIGHTS=surfweights.test.hfst
UNITWEIGHT=test.unitweight
TAGWEIGHTS=tags.reweight
ALPHA=1.0

%.cleaned: %.texts
	$(AM_V_GEN)cat $< |\
		sed -e 's/\( [].:;,(){}!?[]\)\([[:alpha:]]\)/\1 \2/g' \
		-e 's/\([[:alpha:]]\)\([],:;(){}[] \)/\1 \2/g' \
		-e 's/\([[:alpha:]]\)\([.!?]\ \+\)\([[:upper:]]\)/\1 \2\3/g' \
		-e 's/[].:;,(){}!?[]$$/ \0/g' -e 's/^[].:;,(){}!?[]/\0 /g' |\
		egrep -v '^[[:punct:][:digit:]]*$$' |\
		tr -s ' ' '\n' > $@

%.sorted: %.cleaned
	$(AM_V_GEN)sort < $< > $@

%.wordcount: %.sorted
	$(AM_V_GEN)wc -l < $< > $@

%.uniqed: %.sorted
	$(AM_V_at)uniq -c < $< > $@

%.typecount: %.uniqed
	$(AM_V_GEN)wc -l < $< > $@

%.unitweight: %.wordcount %.typecount
	$(AM_V_GEN)paste $*.wordcount $*.typecount |\
		sed -e "s/^/scale=5; -l($(ALPHA)\/(/" \
		-e "s/	/ + ($(ALPHA) */" -e "s/$$/)))/" |\
		bc -l > $@

%.tropical: %.uniqed %.wordcount %.typecount
	$(AM_V_GEN)cat $*.uniqed |\
		$(AWK) -v CS="$$(cat $*.wordcount)" -v ALPHA=$(ALPHA) \
		-v DS="$$(cat $*.typecount)" -f uniqc2tropical.awk > $@

surfweights.%.hfst: %.tropical
	$(AM_V_GEN)cat $< |\
		$(HFST_STRINGS2FST) -j $(HFST_FLAGS) -f openfst-tropical -o $@

# OR: Just copy (no modification):
#$(GT_SPELLER_HFST): ../$(GT_SPELLER_HFST)
#	$(AM_V_GEN)cp -f $< $@

unitweighted.hfst: ../$(GT_SPELLER_HFST) $(UNITWEIGHT)
	$(AM_V_GEN)$(HFST_REWEIGHT) $(HFST_FLAGS) -e -a $$(cat $(UNITWEIGHT)) $< \
		-o unitweighted.hfst

$(GT_SPELLER_HFST): ../$(GT_SPELLER_HFST) $(SURFWEIGHTS) $(TAGWEIGHTS) unitweighted.hfst
	$(AM_V_GEN)$(HFST_COMPOSE) $(HFST_FLAGS) $< $(SURFWEIGHTS) \
		| $(HFST_DISJUNCT) $(HFST_FLAGS) - unitweighted.hfst \
		| $(HFST_MINIMIZE) $(HFST_FLAGS) -o $@

#$(GT_SPELLER_HFST): ../$(GT_SPELLER_HFST) $(SURFWEIGHTS) $(TAGWEIGHTS)
#	$(AM_V_at)$(HFST_REWEIGHT) $(HFST_FLAGS) -e -a $$(cat unitweight) $< \
#		-o unitweighted.hfst
#	$(AM_V_GEN)$(HFST_COMPOSE) $(HFST_FLAGS) $< $(SURFWEIGHTS) \
#		$(HFST_REWEIGHT) $(HFST_FLAGS) -T $(TAGWEIGHTS) |\
#		$(HFST_DISJUNCT) $(HFST_FLAGS) - unitweighted.hfst |\
#		$(HFST_MINIMIZE) $(HFST_FLAGS) -o $@

include $(top_srcdir)/am-shared/tools-spellcheckers-fstbased-hfst-include.am

# vim: set ft=automake:
