all: paper


paper : paper.pdf


paper.pdf : *.Rnw ../R/*.R
	Rscript -e "knitr::knit2pdf('paper.Rnw')"

install_deps : 
	Rscript  --vanilla -e "devtools::install_dev_deps()"
