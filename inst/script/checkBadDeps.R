suppressMessages({
    suppressWarnings({
        pkgdir <- commandArgs(TRUE)
        options(useFancyQuotes=FALSE)
        pkgname <- strsplit(basename(pkgdir), "_")[[1]][1]
        if (.Platform$OS.type == "windows")
        {
            libdir <- file.path(tempdir(), "libdir")
            install.packages(pkgname, repos=null, type="source",
                INSTALL_opts=sprintf("--library=%s",
                    libdir))
        } else {
            libdir <- sub(paste0(pkgname, "$"), "", pkgdir)
            libdir <- sub("/$|\\$", "", libdir)
        }
        .libPaths(c(libdir, .libPaths()))
        library(codetools)
        cat(capture.output(checkUsageEnv(getNamespace(pkgname))), sep="\n")
    })
})
