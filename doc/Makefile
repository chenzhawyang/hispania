TEX = xelatex
DOC = hispania

compile:
	xelatex $(DOC).tex
	biber $(DOC)
	xelatex $(DOC).tex
	xelatex $(DOC).tex

clean:
	/usr/bin/rm $(DOC).bbl $(DOC).aux $(DOC).bcf $(DOC).blg $(DOC).log $(DOC).run.xml $(DOC).toc $(DOC).out

all: clean compile

.PHONY: clean compile all

.DEFAULT_GOAL := compile
