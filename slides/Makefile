TEX = xelatex
DOC = slides

compile:
	$(TEX) -shell-escape $(DOC).tex
	biber $(DOC)
	$(TEX) -shell-escape $(DOC).tex
	$(TEX) -shell-escape $(DOC).tex

clean:
	/usr/bin/rm $(DOC).bbl $(DOC).aux $(DOC).bcf $(DOC).blg $(DOC).log $(DOC).run.xml $(DOC).toc $(DOC).out

all: clean compile

.PHONY: clean compile all

.DEFAULT_GOAL := compile
