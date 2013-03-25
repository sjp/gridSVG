PDFS= animation.pdf extensibility.pdf gridSVG.pdf

all: $(PDFS) 

clean:
	rm -rf *.tex *.bbl *.blg *.aux *.out *.log *.R

%.pdf: %.Rnw
	"$(R_HOME)/bin/R" CMD Sweave --pdf '$*.Rnw'
