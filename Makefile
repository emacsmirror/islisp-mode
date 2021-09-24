EMACS=emacs
FILES=islisp-mode.el

.PHONY: help package elpa clean make-test compile-test test lint

help:
	@printf "\
Main targets\n\
compile    -- compile .el files\n\
elpa 	   -- create a package with the elpa format \n\
package    -- create a tar.gz file with the .el files \n\
test       -- run tests in batch mode\n\
clean      -- delete generated files\n\
lint       -- run package-lint in batch mode\n\
help       -- print this message\n"

package: *.el
	@ver=`grep -o "Version: .*" islisp-mode.el | cut -c 10-`; \
	tar czvf islisp-mode-$$ver.tar.gz --mode 644 $$(find . -name \*.el)

elpa: *.el
	@version=`grep -o "Version: .*" islisp-mode.el | cut -c 10-`; \
	dir=islisp-mode-$$version; \
	mkdir -p "$$dir"; \
	cp $$(find . -name \*.el) islisp-mode-$$version; \
	echo "(define-package \"islisp-mode\" \"$$version\" \
	\"Modular in-buffer completion framework\")" \
	> "$$dir"/islisp-mode-pkg.el; \
	tar cvf islisp-mode-$$version.tar --mode 644 "$$dir"

clean:
	@rm -rf islisp-mode-*/ islisp-mode-*.tar islisp-mode-*.tar.bz2 *.elc ert.el .elpa/

make-test:
	${EMACS}  --batch -l test/make-install.el -l test/make-test.el 

test: make-test clean

compile:
	${EMACS} --batch  -l test/make-install.el -L . -f batch-byte-compile islisp-mode.el islisp-mode*-.el

compile-test: compile clean

lint:
	${EMACS} --batch -l test/make-install.el -f package-lint-batch-and-exit ${FILES}
