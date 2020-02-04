build:
	@rm -f _main.Rmd
	Rscript -e "bookdown::render_book('index.Rmd')"

open:
	open _main.html

clean:
	rm -rf _book _bookdown_files
