VERSION=`grep "^Version" DESCRIPTION | cut -f 2 -d ' '`
VERSION2=`grep "^Version" gtools/DESCRIPTION | cut -f 2 -d ' '`

ver:
	@echo $(VERSION)

all:
	Rscript -e "devtools::document()"
	cd ..; R CMD build gtools; R CMD check --as-cran gtools_$(VERSION2).tar.gz
## FIXME - auto-version number
