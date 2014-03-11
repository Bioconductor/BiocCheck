all:
	"$(R_HOME)/bin$(R_ARCH_BIN)/Rscript" -e "library(knitrBootstrap); knit_bootstrap('BiocCheck.Rmd', show_code=TRUE, boot_style='Flatly')"
clean:
	rm -f *.md
