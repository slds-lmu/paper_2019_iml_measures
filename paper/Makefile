all: paper


paper : paper.pdf


paper.pdf : *.Rnw ../R/iml-measures.R
	Rscript -e "knitr::knit2pdf('paper.Rnw')"
