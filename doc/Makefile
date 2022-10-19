TEX = xelatex
DOC = hispania

all:
	xelatex -shell-escape $(DOC).tex
	biber $(DOC)
	xelatex -shell-escape $(DOC).tex
	xelatex -shell-escape $(DOC).tex

clean:
	/usr/bin/rm $(DOC).bbl $(DOC).aux $(DOC).bcf $(DOC).blg $(DOC).log $(DOC).run.xml $(DOC).toc $(DOC).out

.PHONY: clean

.DEFAULT_GOAL := all
