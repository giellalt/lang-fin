## Process this file with automake to produce Makefile.in
## Copyright: Sámediggi/Divvun/UiT
## Licence: GPL v3+

# always build . last here, and tagsets have to be built after morphology
SUBDIRS = fst filters phonetics hyphenation orthography cg3 transcriptions tagsets .

####### Automake targets: ########

# Define target variables first, before assigning to them:
GT_ANALYSERS=
GT_GENERATORS=
CUSTOM_FSTS=

#### Local modifications in *fst processing: ####
####
#### Copy the fallback targets, and rename them to the desired targets. Then:
#### Replace the 'cp' command (Xerox) / Prepend the hfst-invert command (Hfst -
#### remember to move the $<) with whatever you need to complete
#### the processing to get the final target transducer.
#### Remember to add the dependencies as well.
#### Also make sure that HFST and Xerox processing are the same.
####
#### If you add new transducers to be built, you need to add them to the
#### relevant variable, e.g.:
####
#### if CAN_HFST
#### GT_GENERATORS+=generator-oahpa-gt-norm.hfst
#### endif
####
#### NB!!!! The HFST targets should get a hyphen after 'analyser'/'generator'
#### respectively, to make the local targets minimally different from and
#### slightly more specific than the fallback targets. This is to avoid warnings
#### about duplicate targets. That is, the local targets should looke like:
####
#### analyser-%.hfst: analyser-%.tmp.hfst
#### generator-%.hfst: generator-%.tmp.hfst

##################################################################
#### BEGIN: Add local processing instructions BELOW this line ####
##################################################################

########################################################
#### Add language-specific transducer targets here: ####

#### Xerox transducers:
if CAN_XFST
GT_ANALYSERS+=
GT_GENERATORS+=

if WANT_CUSTOM_FSTS
CUSTOM_FSTS+=
endif # WANT_CUSTOM_FSTS

endif # CAN_XFST

#### HFST transducers
if CAN_HFST
GT_ANALYSERS+=
GT_GENERATORS+=

if WANT_CUSTOM_FSTS
CUSTOM_FSTS+=
endif # WANT_CUSTOM_FSTS

endif # CAN_HFST

#### FOMA transducers
if CAN_FOMA
GT_ANALYSERS+=
GT_GENERATORS+=

if WANT_CUSTOM_FSTS
CUSTOM_FSTS+=
endif # WANT_CUSTOM_FSTS

endif # CAN_FOMA

#################################################
#### Add language-specific build rules here: ####


# Hfst - add weights to compounds if using tropical-semiring fst format:
if WITH_OFST_TROPICAL
generator-raw-gt-desc.hfst: generator-raw-gt-desc.tmp.hfst
	$(AM_V_REWEIGHT)$(HFST_REWEIGHT) $(MORE_VERBOSITY) $(HFST_FLAGS) \
			-S '+Cmp' -a 10 --arcs-only -i $< \
		> $@
endif

guesser-raw-gt-desc.hfst: fst/guesser.hfst \
					 fst/phonology.compose.hfst
	$(AM_V_INTRSCT)\
		  $(HFST_DETERMINIZE) $(MORE_VERBOSITY) $(HFST_FLAGS) $<\
		| $(HFST_MINIMIZE) $(MORE_VERBOSITY) $(HFST_FLAGS) \
		| $(HFST_COMPOSE_INTERSECT) $(COMPOSE_INTERSECT_FLAG) \
			$(MORE_VERBOSITY) $(HFST_FLAGS) \
			-2 fst/phonology.compose.hfst \
		| $(HFST_MINIMIZE) $(MORE_VERBOSITY) $(HFST_FLAGS) \
		-o $@

# This is the default, descriptive analyser:
# Visible tags (ie do NOT remove):
#   - variant tags
#   - the Err/Orth tag
# Invisible tags (ie to be removed):
#   - semantic tags
#   - homonymy tags
guesser-gt-desc.tmp.%: guesser-raw-gt-desc.%                \
					filters/remove-area-tags.%		\
					filters/remove-dialect-tags.%	     \
					filters/remove-number-string-tags.%       \
					filters/remove-semantic-tags.%	    \
					filters/remove-hyphenation-marks.%	\
					filters/remove-infl_deriv-borders.%       \
					filters/remove-word-boundary.%	    \
					filters/remove-orthography-tags.%	 \
					filters/remove-Orth_IPA-strings.%	 \
					filters/remove-orig_lang-tags.%	   \
					filters/remove-Use_GC-strings.%	   \
					filters/remove-Use_minusGC-tags.%	 \
					filters/remove-Use_minus_PMatch-tags.%    \
					filters/remove-Use_PMatch-strings.%       \
					filters/remove-mwe-tags.%		 \
					orthography/inituppercase.compose.%       \
					orthography/spellrelax.compose.%	  \
					$(GLT_DOWNCASE_FILTER)
	$(AM_V_XFST_TOOL)$(PRINTF) "read regex \
				@\"filters/remove-area-tags.$*\"		\
			.o. @\"filters/remove-dialect-tags.$*\"	     \
			.o. @\"filters/remove-number-string-tags.$*\"       \
			.o. @\"filters/remove-semantic-tags.$*\"	    \
			.o. @\"filters/remove-orig_lang-tags.$*\"	   \
			.o. @\"filters/remove-orthography-tags.$*\"	 \
			.o. @\"filters/remove-Orth_IPA-strings.$*\"	 \
			.o. @\"filters/remove-Use_minus_PMatch-tags.$*\"    \
			.o. @\"filters/remove-Use_GC-strings.$*\"	   \
			.o. @\"filters/remove-Use_minusGC-tags.$*\"	 \
			.o. @\"filters/remove-Use_PMatch-strings.$*\"       \
			.o. @\"filters/remove-mwe-tags.$*\"		 \
			.o. @\"$<\" \
			    $(GLT_DOWNCASE_COMPOSE) \
			.o. @\"filters/remove-hyphenation-marks.$*\"	\
			.o. @\"filters/remove-infl_deriv-borders.$*\"       \
			.o. @\"filters/remove-word-boundary.$*\"	    \
			; \n\
			define fst \n\
			set flag-is-epsilon ON\n\
			read regex fst \
			.o. @\"orthography/inituppercase.compose.$*\"       \
			.o. @\"orthography/spellrelax.compose.$*\"	  \
			;\n\
		 save stack $@\n\
		 quit\n" | $(XFST_TOOL)

guesser-gt-desc.hfstol: guesser-gt-desc.tmp.hfst
	hfst-minimise $< | hfst-invert | hfst-fst2fst -f olw -o $@

##################################################################
#### END: Add local processing instructions ABOVE this line ######
##################################################################

include $(top_srcdir)/../giella-core/am-shared/src-dir-include.am
