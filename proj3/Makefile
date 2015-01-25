# This Makefile is used to generate the final document

project3.html: project3.Rmd precinct.json
	Rscript -e "library(rmarkdown);render('project3.Rmd')"

precinct.json: project3.R
	Rscript project3.R


# Helper

.PHONY: clean
clean:
	rm -f project3.html