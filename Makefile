SBCL ?= sbcl
ROOT := $(dir $(abspath $(lastword $(MAKEFILE_LIST))))

# The suite package. zebra.asd's test-op names the same one separately, so
# adding a test package means adding it in both places. Overridable so the
# gate's own red and zero-test controls exercise this recipe rather than a
# copy of it that could drift out of agreement with it.
SUITE ?= zebra.test

.PHONY: test

# Runs the suite in a fresh image and takes the verdict from the result
# objects rather than from the printed summary, which undercounts: a test
# that errors before asserting is dropped from the "Failed:" line while
# results-with-status still returns it.
#
# The zero-test check is here because this repository is the framework the
# gate runs on. A gate that loads but executes nothing would otherwise find
# no failures and report success.
#
# The source-directory check is not belt and braces. asdf:load-asd does NOT
# signal when handed a path that does not exist, and this system is also
# reachable through the ambient registry, so a wrong path here resolves to
# whatever copy the registry finds first and the suite passes against source
# nobody meant to test.
test:
	@$(SBCL) --noinform --no-userinit --disable-debugger \
	  --eval "(require :asdf)" \
	  --eval "(asdf:load-asd \"$(ROOT)zebra.asd\")" \
	  --eval "(asdf:load-system :zebra/test)" \
	  --eval "(let ((want (truename \"$(ROOT)\")) \
	                (got (truename (asdf:system-source-directory :zebra)))) \
	            (unless (equal want got) \
	              (format *error-output* \"~&gate: loaded ~a, expected ~a~%\" got want) \
	              (uiop:quit 1)))" \
	  --eval "(let* ((r (zebra:test :$(SUITE))) \
	                 (failed (length (zebra:results-with-status :failed r))) \
	                 (ran (length (remove-duplicates \
	                                (append (zebra:tests-with-status :passed r) \
	                                        (zebra:tests-with-status :failed r) \
	                                        (zebra:tests-with-status :skipped r)))))) \
	            (format t \"~&gate: suite=~a tests=~d failing-results=~d~%\" :$(SUITE) ran failed) \
	            (when (zerop ran) \
	              (format *error-output* \"~&gate: ran no tests~%\")) \
	            (when (plusp failed) \
	              (format *error-output* \"~&gate: ~d failing result(s)~%\" failed)) \
	            (uiop:quit (if (and (plusp ran) (zerop failed)) 0 1)))"
