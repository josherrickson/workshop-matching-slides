build:
	@rm -f _main.Rmd
	Rscript -e "rmarkdown::render('index.Rmd')"

open:
	open index.html
