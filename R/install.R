.installScript <- function()
{
    onWindows <- (.Platform$OS.type == "windows")
    files <- "BiocCheck"
    if (onWindows)
        files <- append(files, "BiocCheck.bat")

    srcDir <- file.path("inst", "script")
    srcFile <- file.path(srcDir, files)
    destDir <- file.path(Sys.getenv("R_HOME"), "bin")
    destFile <- file.path(destDir, files)
    alreadyExists <- all(file.exists(destFile))


    if ( (!alreadyExists) )
    {
        res <- FALSE
        suppressWarnings(
            tryCatch({
                    res <- file.copy(srcFile, destDir, overwrite<-TRUE)

                    if (!onWindows)
                    {
                        res <- Sys.chmod(destFile, "0755")
                    }
                        },
                error=function(e) res=-1)
        )

        if (interactive())
            func <- packageStartupMessage
        else
            func <- message


        if (is.null(res) || !res || res == -1)
        {
            msg <- strwrap(paste(
                'Failed to copy the "inst/script/BiocCheck" script to',
                paste0(file.path(Sys.getenv("R_HOME"), "bin"), "."),
                "If you want to be able to run 'R CMD BiocCheck' you'll",
                "need to copy it yourself to a directory on your PATH,",
                "making sure it is executable. Edit the copied version,",
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