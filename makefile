all: submodule figures pdf clean


submodule:
	git submodule update --init .

figures:
	make -C figures/

pdf:
	pdflatex poster.tex
	pdflatex poster.tex
	pdflatex poster.tex
	pdflatex poster.tex

clean: 
	- rm -f *.log
	- rm -f *.soc
	- rm -f *.toc
	- rm -f *.aux
	- rm -f *.out
	- rm -f main.idx
	- rm -f *.bbl
	- rm -f *.bbg
	- rm -f *.dvi
	- rm -f *.blg
	- rm -f *.lof
	- rm -f *.nav
	- rm -f *.snm
	- rm -f *~

