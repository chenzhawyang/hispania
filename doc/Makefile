TEX = xelatex
DOC = hispania

all:
	xelatex $(DOC).tex
	biber $(DOC)
	xelatex $(DOC).tex
	xelatex $(DOC).tex

clean:
	/usr/bin/rm $(DOC).bbl $(DOC).aux $(DOC).bcf $(DOC).blg $(DOC).log $(DOC).run.xml $(DOC).toc $(DOC).out

.PHONY: clean

.DEFAULT_GOAL := all
