all: paper


paper : paper.pdf


paper.pdf : *.Rnw
	Rscript -e "knitr::knit2pdf('paper.Rnw')"
