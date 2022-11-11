TEX = xelatex
DOC = slides

compile:
	$(TEX) $(DOC).tex
	biber $(DOC)
	$(TEX) $(DOC).tex
	$(TEX) $(DOC).tex

clean:
	/usr/bin/rm $(DOC).bbl $(DOC).aux $(DOC).bcf $(DOC).blg $(DOC).log $(DOC).run.xml $(DOC).toc $(DOC).out

all: clean compile

.PHONY: clean compile all

.DEFAULT_GOAL := compile
