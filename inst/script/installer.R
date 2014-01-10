#!/usr/bin/env Rscript --vanilla

library(tools)

srcFile <- file.path("inst", "script", "BiocCheck")
destDir <- file.path(Sys.getenv("R_HOME"), "bin")
destFile <- file.path(destDir, "BiocCheck")
alreadyExists <- file.exists(destFile)


if ( (!alreadyExists) || (md5sum(srcFile) != md5sum(destFile)))
{
    res <- FALSE
    suppressWarnings(
        tryCatch({
                res <- file.copy(srcFile, destDir, overwrite<-TRUE)
                res <- Sys.chmod(destFile, "0755")
                    },
            error=function(e) res=-1)
    )

    if (is.null(res) || !res || res == -1)
    {
        message(paste('Failed to copy the "inst/script/BiocCheck" script to\n',
            file.path(Sys.getenv("R_HOME"), "bin"), "\n",
            "If you want to be able to run 'R CMD BiocCheck' you'll\n",
            "need to copy it yourself to a directory on your PATH,\n",
            "making sure it is executable.", sep=""))
    }
}

