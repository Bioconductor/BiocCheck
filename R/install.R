.installScript <- function()
{
    onWindows <- (.Platform$OS.type == "windows")
    files <- "BiocCheck"
    if (onWindows)
        files <- append(files, "BiocCheck.bat")

    srcDir <- system.file("script", package="BiocCheck")
    srcFile <- file.path(srcDir, files)
    destDir <- file.path(Sys.getenv("R_HOME"), "bin")
    destFile <- file.path(destDir, files)
    alreadyExists <- all(file.exists(destFile))


    if ( (!alreadyExists) )
    {


        tmpFile <- file.path(tempdir(), "BiocCheck")
        lines <- readLines(system.file("script", "BiocCheck",
            package="BiocCheck"))
        lines <- sub("PATH_TO_RSCRIPT",
            file.path(Sys.getenv("R_HOME"), "bin", "Rscript"),
            lines)
        cat(lines, sep="\n", file=tmpFile)
        Sys.chmod(tmpFile, "0755")

        filesToCopy <- c(tmpFile)
        if (onWindows)
            filesToCopy <- append(filesToCopy, system.file("script", "BiocCheck.bat",
                package="BiocCheck"))

        res <- FALSE
        suppressWarnings(
            tryCatch({
                    res <- 
                        all(file.copy(filesToCopy, destDir, overwrite=TRUE))

                        },
                error=function(e) res=-1)
        )

        destFiles <- file.path(destDir, basename(filesToCopy))

        res <- all(file.exists(destFiles))

        if (interactive())
            func <- packageStartupMessage
        else
            func <- message

        if (is.null(res) || !res || res == -1)
        {
            msg <- strwrap(paste(
                'Failed to copy the "script/BiocCheck" script to',
                paste0(file.path(Sys.getenv("R_HOME"), "bin"), "."),
                "If you want to be able to run 'R CMD BiocCheck' you'll",
                "need to copy it yourself to a directory on your PATH,",
                "making sure it is executable.",
                "Edit the copied version,",
                "replacing RSCRIPT_PATH with the full path to Rscript.",
                "See BiocCheck vignette for more information."))
            for (i in 1:length(msg))
                func(msg[i])
            if (onWindows)
                func(
                    "Windows users need to copy BiocCheck.bat as well.")
        } else {
            func("BiocCheck script installed.")
        }
    }


}