TEX = xelatex
DOC = hispania

all:
	xelatex -shell-escape $(DOC).tex
	biber $(DOC)
	xelatex -shell-escape $(DOC).tex
	xelatex -shell-escape $(DOC).tex

clean-bib:
	/usr/bin/rm $(DOC).bbl $(DOC).aux

.PHONY: clean-bib

.DEFAULT_GOAL := all
