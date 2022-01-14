# R_DEFAULT_PACKAGES=NULL "$HOME/src/svn/r-devel/R/lib/R/bin/R" --vanilla --args "$HOME/bioc/BiocCheck/inst/testpackages/testpkg0" "/tmp/RtmpRE92fA/file26de182a35c4/lib"
suppressMessages({
    suppressWarnings({
        args <- commandArgs(TRUE)
        pkgdir <- args[[1L]]
        libloc <- args[[2L]]
        options(useFancyQuotes=FALSE)
        # BiocCheck is not installed -- use basename
        pkgname <- basename(pkgdir)
        .libPaths(c(libloc, .libPaths()))
        cat(utils::capture.output(
            codetools::checkUsageEnv(
                getNamespace(pkgname)
            )
        ), sep="\n")
    })
})
