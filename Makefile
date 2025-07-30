EMACS ?= emacs
ORG_TEST ?= $(ORG_TEST_DIR)

all: org-micron.elc

test: org-micron.elc
	@$(EMACS) -Q -batch \
		-l ert \
		-l org-micron.el \
		-l $(ORG_TEST)/org-test.el \
		-l org-micron-tests.el \
		--eval "(let ((ert-batch-print-level 10) \
	                    (ert-batch-print-length 120)) \
				(ert-run-tests-batch-and-exit))"

org-micron.elc: org-micron.el
	$(EMACS) -Q -batch -f batch-byte-compile $<

clean:
	rm -f *.elc
