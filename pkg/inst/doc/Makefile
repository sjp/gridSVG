# Makefile to use knitr for package vignettes

# put all PDF targets here, separated by spaces
PDFS= animation.pdf extensibility.pdf gridSVG.pdf

all: $(PDFS) 

clean:
	rm -rf *.tex *.bbl *.blg *.aux *.out *.log *.R

%.pdf: %.Rnw
	"$(R_HOME)/bin/R" CMD Sweave --pdf '$*.Rnw'
