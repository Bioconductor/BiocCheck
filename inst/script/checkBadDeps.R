suppressMessages({
    suppressWarnings({
        pkgdir <- commandArgs(TRUE)
        options(useFancyQuotes=FALSE)
        pkgname <- strsplit(basename(pkgdir), "_")[[1]][1]
        libdir <- sub(pkgname, "", pkgdir)
        .libPaths(c(libdir, .libPaths()))
        library(codetools)
        cat(capture.output(checkUsageEnv(getNamespace(pkgname))), sep="\n")
    })
})
