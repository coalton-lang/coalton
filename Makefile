.PHONY: docs
docs:
	sbcl --non-interactive \
	     --eval "(ql:quickload :coalton)" \
	     --eval "(with-open-file (out \"docs/reference.md\" :direction :output :if-exists :supersede) \
	               (coalton-impl/doc::write-library-documentation-to-markdown coalton-impl::*global-environment* out))"
