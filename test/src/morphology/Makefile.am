## Process this file with automake to produce Makefile.in
## Copyright: Sámediggi/Divvun/UiT
## Licence: GPL v3+

######## Test targets: ###########

# List here (space separated) all test scripts that should be run:
TESTS=generate-noun-lemmas.sh

# Add your shell scripts for running python-based tests here:
if CAN_YAML_TEST
DOCC2YAML=$(top_srcdir)/src/scripts/doccomments2yaml.awk
YAML_COMMENTS=roots_gt-norm.yaml \
			  noun-stems_gt-norm.yaml \
			  adj-stems_gt-norm.yaml \
			  nouns-affixes_gt-norm.yaml \
			  adjectives-affixes_gt-norm.yaml
check_DATA=$(YAML_COMMENTS)

roots_gt-norm.yaml: $(top_srcdir)/src/morphology/root.lexc
	$(AWK) -f $(DOCC2YAML) < $< > $@

roots_gt-desc.yaml: $(top_srcdir)/src/morphology/root.lexc
	$(AWK) -f $(DOCC2YAML) --assign=TYPE=gt-desc < $< > $@

%-stems_gt-norm.yaml: $(top_srcdir)/src/morphology/stems/%.lexc
	$(AWK) -f $(DOCC2YAML) < $< > $@

%-stems_gt-desc.yaml: $(top_srcdir)/src/morphology/stems/%.lexc
	$(AWK) -f $(DOCC2YAML) --assign=TYPE=gt-desc < $< > $@
 
%-affixes_gt-norm.yaml: $(top_srcdir)/src/morphology/affixes/%.lexc
	$(AWK) -f $(DOCC2YAML) < $< > $@

%-affixes_gt-desc.yaml: $(top_srcdir)/src/morphology/affixes/%.lexc
	$(AWK) -f $(DOCC2YAML) --assign=TYPE=gt-desc < $< > $@

TESTS+=run-gt-norm-yaml-testcases.sh run-gt-desc-yaml-testcases.sh
endif

# List tests that are presently (expected) failures here, ie things that should
# be fixed *later*, but is not critical at the moment:
# XFAIL_TESTS=

####### Other targets: ###########
clean-local:
	-rm -f *.txt
