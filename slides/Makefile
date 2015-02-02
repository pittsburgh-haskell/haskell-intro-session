PANDOC = pandoc
BEAMER_THEME = Boadilla

all:	lightning.pdf

# Create slides with pandoc
%.pdf:	%.md Makefile
	$(PANDOC) --template=default.beamer -s --slide-level=2 -t beamer -V theme:$(BEAMER_THEME) -o $@ $<

.PHONY:	all
