# OASIS_START
# DO NOT EDIT (digest: a3c674b4239234cbbe53afe090018954)

SETUP = ocaml setup.ml

build: setup.data
	$(SETUP) -build $(BUILDFLAGS)

doc: setup.data build
	$(SETUP) -doc $(DOCFLAGS)

test: setup.data build
	$(SETUP) -test $(TESTFLAGS)

all:
	$(SETUP) -all $(ALLFLAGS)

install: setup.data
	$(SETUP) -install $(INSTALLFLAGS)

uninstall: setup.data
	$(SETUP) -uninstall $(UNINSTALLFLAGS)

reinstall: setup.data
	$(SETUP) -reinstall $(REINSTALLFLAGS)

clean:
	$(SETUP) -clean $(CLEANFLAGS)

distclean:
	$(SETUP) -distclean $(DISTCLEANFLAGS)

setup.data:
	$(SETUP) -configure $(CONFIGUREFLAGS)

configure:
	$(SETUP) -configure $(CONFIGUREFLAGS)

.PHONY: build doc test all install uninstall reinstall clean distclean configure

# OASIS_STOP

.PHONY: oasis
oasis:
	oasis setup -setup-update dynamic

%.out: %.in
	./origami.native solve best_bb $< > $@ || rm $@

%.done: %.out
	cp $< $@

%.in.png: %.in
	./origami.native render $<

all_out=$(patsubst %.in,%.out,$(wildcard data/*.in))
all_perfect_out=$(patsubst %.perfect_score,%.out,$(wildcard data/*.perfect_score))

#all_png=$(patsubst %.in,%.in.png,$(wildcard data/*.in))

.PHONY: redo
redo:
	$(MAKE) -B $(sort $(filter-out $(all_perfect_out),$(all_out)))

.PHONY: new
new:
	$(MAKE) -B $(sort $(filter-out $(all_perfect_out) $(wildcard data/*.out), $(all_out)))
