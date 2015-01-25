# This Makefile is used to generate the final document

proj5.html: proj5.Rmd 
	Rscript -e "library(rmarkdown);render('proj5.Rmd')"



# Helper

.PHONY: clean
clean:
	rm -f proj5.html
