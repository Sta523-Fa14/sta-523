# This Makefile is used to generate the final document

HW2.html: HW2.Rmd LaQuinta/Inns_Pages/La_Quinta_Inns_Data.Rdata dennys/dennys_data.Rdata
	Rscript -e "library(rmarkdown);render('HW2.Rmd')"


# La Quinta files

LaQuinta/Inns_Pages/La_Quinta_Inns_Data.Rdata: LaQuinta_Data_Parse_Script.R LaQuinta/Inns_Pages/%.html LaQuinta/LaQuinta_Hreflinks_Data.Rdata
	Rscript LaQuinta_Data_Parse_Script.R

LaQuinta/LaQuinta_Hreflinks_Data.Rdata LaQuinta/Inns_Pages/%.html: LaQuinta_Data_Load_Script.R
	Rscript LaQuinta_Data_Load_Script.R


# Denny's files

dennys/dennys_data.Rdata: Dennys_Parse.R dennys/%.html
	Rscript Dennys_Parse.R

dennys/%.html: Dennys_Download.R
	Rscript Dennys_Download.R


# Helper

.PHONY: clean
clean:
	rm -rf dennys/
	rm -rf LaQuinta/
	rm -f hw2.html