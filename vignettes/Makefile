whoami=$(shell whoami)

all: 	
	make gRain-intro.pdf gRain-evidence.pdf

clean:
	rm -f *.tex *.bbl *.blg *.aux *.out *.log *.toc *.Rnw~ 

copy:
	cp gRain-intro.Rnw ../gRain/inst/doc/
	cp gRain-intro.pdf ../gRain/inst/doc/
	cp gRain-evidence.Rnw ../gRain/inst/doc/
	cp gRain-evidence.pdf ../gRain/inst/doc/
	cp gRain.bib 	   ../gRain/inst/doc/	

setvars:
ifeq (${R_HOME},)
R_HOME=	$(shell R RHOME)
endif
RPROG=	$(R_HOME)/bin/R
RSCRIPT=$(R_HOME)/bin/Rscript

gRain-intro.pdf:
	$(RPROG) CMD Sweave --encoding=latin1 gRain-intro.Rnw
	$(RSCRIPT) --vanilla -e "tools::texi2dvi( 'gRain-intro.tex', pdf = TRUE, clean = FALSE )"
	$(RSCRIPT) --vanilla -e "tools::texi2dvi( 'gRain-intro.tex', pdf = TRUE, clean = FALSE )"
	rm -fr *.tex

gRain-evidence.pdf:
	$(RPROG) CMD Sweave --encoding=latin1 gRain-evidence.Rnw
	$(RSCRIPT) --vanilla -e "tools::texi2dvi( 'gRain-evidence.tex', pdf = TRUE, clean = FALSE )"
	$(RSCRIPT) --vanilla -e "tools::texi2dvi( 'gRain-evidence.tex', pdf = TRUE, clean = FALSE )"
	rm -fr *.tex


