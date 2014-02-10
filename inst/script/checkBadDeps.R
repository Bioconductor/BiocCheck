suppressMessages({
    suppressWarnings({
        pkgdir <- commandArgs(TRUE)
        options(useFancyQuotes=FALSE)
        pkgname <- strsplit(basename(pkgdir), "_")[[1]][1]
        #libdir <- sub(pkgname, "", pkgdir)
        libdir <- dirname(pkgname)
        .libPaths(c(libdir, .libPaths()))
        library(codetools)
        cat(capture.output(checkUsageEnv(getNamespace(pkgname))), sep="\n")
    })
})
