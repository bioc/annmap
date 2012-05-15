R=${R_HOME}/bin/R
SUITE=../inst/cookbook/delia.R

all:	pdf clean

pdf:
	${R} --vanilla --verbose < ${SUITE}
	texi2dvi --pdf annmap.tex
	texi2dvi --pdf INSTALL.tex

clean:
	rm *.aux
	rm *.log
	rm *.out
	rm *.toc
	rm *.tex
	
