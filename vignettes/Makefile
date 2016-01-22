all:
	"$(R_HOME)/bin$(R_ARCH_BIN)/Rscript" -e "library(knitrBootstrap); library(rmarkdown);render('IntroToEpivizr.Rmd', bootstrap_document(theme='Readable'))"
clean:
	rm -f *.md
