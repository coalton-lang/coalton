.PHONY: test
test:
	sbcl --non-interactive \
	     --eval "(asdf:test-system :coalton)"

.PHONY: docs
docs:
	sbcl --non-interactive \
	     --eval "(ql:quickload :coalton)" \
	     --eval "(with-open-file (out \"docs/reference.md\" :direction :output :if-exists :supersede) \
	               (coalton-impl/doc::write-library-documentation-to-markdown coalton-impl::*global-environment* out \"../src/library/\"))"
