## Process this file with automake to produce Makefile.in
## Copyright: Sámediggi/Divvun/UiT
## Licence: GPL v3+

# Subdirs in build order:
# SUBDIRS = filters weights .

GT_PREDICT_EMOJI=transcriptor-lemma2emoji.compose.hfst
GT_PREDICT_ERRMODELS=edit-1.hfst
GT_PREDICT_MODELS=predict-3.hfst predict-inf.hfst morph-completions.hfst

GT_PREDICT_HFST=analyser-predict-gt-norm.hfst fin-predict.hfstol

transcriptor-lemma2emoji.compose.hfst: $(topbuilddir)/src/transcriptors/transcriptor-lemma2emoji.compose.hfst
	cp -v $< $@

surf2emoji.hfst: analyser-predict-gt-norm.hfst transcriptor-lemma2emoji.compose.hfst
	hfst-compose -F analyser-predict-gt-norm.hfst transcriptor-lemma2emoji.compose.hfst -o $@

fin-predict.hfst: analyser-predict-gt-norm.hfst predict-3.hfst surf2emoji.hfst
	hfst-project -p input analyser-predict-gt-norm.hfst |\
		hfst-disjunct surf2emoji.hfst |\
		hfst-invert |\
		hfst-compose predict-3.hfst -o $@

noinst_DATA=$(GT_PREDICT_HFST)

include $(top_srcdir)/../giella-core/am-shared/tools-predict-dir-include.am
include $(top_srcdir)/../giella-core/am-shared/regex-include.am
