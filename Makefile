all:
	Rscript -e "devtools::document()"
	cd ..; R CMD build gtools; R CMD check --as-cran gtools_3.9.2.9000.tar.gz
## FIXME - auto-version number
