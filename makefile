PACKAGE_SHORTNAME=wsd-mode
VERSION=$(shell grep ";; Version " $(PACKAGE_SHORTNAME).el | cut -d ":" -f2 | cut -c2-)
PACKAGE_NAME:=$(PACKAGE_SHORTNAME)-$(VERSION)
PACKAGE_DIR:=./.tmp/$(PACKAGE_NAME)
#PACKAGE_DIR:=/tmp/$(PACKAGE_NAME)

EMACS=$(shell which emacs) -Q -batch -L .
ELS = wsd-mode.el wsd-core.el
ELCS = $(ELS:.el=.elc)

package: $(PACKAGE_DIR)
	tar cvf ../$(PACKAGE_NAME).tar --exclude="*#" --exclude="*~" --exclude="*tests*" --exclude "*package-template*" --exclude="makefile" --exclude="run-travis-ci.sh" -C $(PACKAGE_DIR)/.. $(PACKAGE_NAME)

$(PACKAGE_DIR):
	mkdir -p $@
	cp -r ../$(PACKAGE_SHORTNAME)/* $@
	sed -re "s/VERSION/$(VERSION)/" $@/$(PACKAGE_SHORTNAME)-package-template.el > $@/$(PACKAGE_SHORTNAME)-pkg.el

test:
	+ $(EMACS) -l wsd-tests.el -f ert-run-tests-batch-and-exit

%.elc: %.el
	$(EMACS) -f batch-byte-compile $<

all: $(ELCS) test package

clean:
	rm -f ../$(PACKAGE_NAME).tar
	rm -rf $(PACKAGE_DIR)
	rm -rf $ELCS

# end
