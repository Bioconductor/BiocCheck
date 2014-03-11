#!/usr/bin/env Rscript --vanilla

library(tools)
onWindows <- (.Platform$OS.type == "windows")

srcDir <- file.path("inst", "script")
srcFile <- file.path(srcDir, "BiocCheck")
if (onWindows)
    batFile <- c(srcFile, file.path(srcDir, "BiocCheck.bat"))
destDir <- file.path(Sys.getenv("R_HOME"), "bin")
destFile <- file.path(destDir, "BiocCheck")
tmpFile <- file.path(tempdir(), "BiocCheck")
alreadyExists <- file.exists(destFile)

lines <- readLines(srcFile)
lines <- gsub("RSCRIPT_PATH", file.path(Sys.getenv("R_HOME"),
    "bin", "Rscript"), lines)
cat(lines, file=tmpFile, sep="\n")
Sys.chmod(tmpFile, "0755")

if ( (!alreadyExists) || (md5sum(tmpFile) != md5sum(destFile)))
{
    res <- FALSE
    suppressWarnings(
        tryCatch({
                res <- file.copy(tmpFile, destDir, overwrite<-TRUE)

                if (onWindows)
                {
                    res <- file.copy(batFile, destDir, overwrite<-TRUE)
                } else {
                    res <- Sys.chmod(destFile, "0755")
                }
                    },
            error=function(e) res=-1)
    )

    if (is.null(res) || !res || res == -1)
    {
        cat(strwrap(paste(
            'Failed to copy the "inst/script/BiocCheck" script to',
            paste0(file.path(Sys.getenv("R_HOME"), "bin"), "."),
            "If you want to be able to run 'R CMD BiocCheck' you'll",
            "need to copy it yourself to a directory on your PATH,",
            "making sure it is executable. Edit the copied version,",
            "replacing RSCRIPT_PATH with the full path to Rscript.",
            "See BiocCheck vignette for more information.", sep=" ")),
            sep="\n")
        if (onWindows)
            message("Windows users need to copy BiocCheck.bat as well.")
    }
}

