######################
## R requirements   ##
## /usr/bin/Rscript ##
######################

all: datos/
	qrencode -s 24 -o qr.png  cor.to/socialLearning
	convert qr.png qr.pdf
	/usr/bin/Rscript main.R
	rm Rplots.pdf
	pdflatex elo-factor-en.tex
	pdflatex trueskill_versionGeneral-en.tex

datos/:
	wget https://github.com/glandfried/mlss2018/releases/download/v0.0/datos.zip
	unzip datos.zip


