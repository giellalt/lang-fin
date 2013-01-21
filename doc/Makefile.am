## Process this file with automake to produce Makefile.in
## Copyright: Sámediggi/Divvun/UiT
## Licence: GPL v3+

if CAN_DOCC

# INSTALL these files:
JSP_PAGES=root-all.jspwiki\
          51-stems.jspwiki \
          abbr-stems.jspwiki \
          acro-stems.jspwiki \
          adj-stems.jspwiki \
          adv-stems.jspwiki \
          cc-stems.jspwiki \
          cs-stems.jspwiki \
          digits-stems.jspwiki \
          exceptions-stems.jspwiki \
          interj-stems.jspwiki \
          noun-stems.jspwiki \
          numeral-stems.jspwiki \
          particles-stems.jspwiki \
          pp-stems.jspwiki \
          prefixes-stems.jspwiki \
          pron-stems.jspwiki \
          propernoun-stems.jspwiki \
          punct-stems.jspwiki \
          suffixes-stems.jspwiki \
          verb-stems.jspwiki \
          acronyms-affixes.jspwiki \
          adjectives-affixes.jspwiki \
          adv-affixes.jspwiki \
          digits-affixes.jspwiki \
          nouns-affixes.jspwiki \
          numeral-affixes.jspwiki

# Files with no document content:
#          codifieds-affixes.jspwiki \
#          pp-affixes.jspwiki \
#          prefix-affixes.jspwiki \
#          pron-affixes.jspwiki \
#          propernouns-affixes.jspwiki \
#          verbs-affixes.jspwiki

doc_DATA=$(JSP_PAGES) fin.jspwiki

DOCC2RST=$(top_srcdir)/src/scripts/doccomments2rst.awk
DOCC2TEX=$(top_srcdir)/src/scripts/doccomments2tex.awk
DOCC2JSPWIKI=$(top_srcdir)/src/scripts/doccomments2jspwiki.awk

RST2HTML=rst2html
LATEX=xelatex

%-all.jspwiki: $(top_srcdir)/src/morphology/%.lexc
	$(AWK) -f $(DOCC2JSPWIKI) < $< > $@

%-stems.jspwiki: $(top_srcdir)/src/morphology/stems/%.lexc
	$(AWK) -f $(DOCC2JSPWIKI) < $< > $@

%-affixes.jspwiki: $(top_srcdir)/src/morphology/affixes/%.lexc
	$(AWK) -f $(DOCC2JSPWIKI) < $< > $@

fin.jspwiki: preamble.jspwiki $(JSP_PAGES)
	-rm -f $@
	cat $(srcdir)/preamble.jspwiki $(JSP_PAGES) > $@

fin.tex: preamble.tex doc-comments.conf
	-rm -f $@
	cat $(srcdir)/preamble.tex > $@


fin.rst: preamble.rst doc-comments.conf
	-rm -f $@
	cat $(srcdir)/preamble.rst > $@

%.html: %.rst
	$(RST2HTML) < $< > $@

%.pdf: %.tex
	$(LATEX) $<
	$(LATEX) $<
	$(LATEX) $<

endif # CAN_DOCC
