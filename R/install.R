.installScript <- function()
{
    onWindows <- (.Platform$OS.type == "windows")
    files <- "BiocCheck"
    if (onWindows)
        files <- "BiocCheck.bat"

    srcDir <- system.file("script", package="BiocCheck")
    srcFile <- file.path(srcDir, files)
    destDir <- file.path(Sys.getenv("R_HOME"), "bin")
    destFile <- file.path(destDir, files)
    alreadyExists <- all(file.exists(destFile))


    if ( (!alreadyExists) )
    {
        res <- FALSE
        suppressWarnings(
            tryCatch({
                    res <- 
                        all(file.copy(srcFile, destDir, overwrite=TRUE))

                        },
                error=function(e) res=-1)
        )

        destFiles <- file.path(destDir, basename(srcFile))

        res <- all(file.exists(destFiles))

        if (interactive())
            func <- packageStartupMessage
        else
            func <- message

        if (is.null(res) || !res || res == -1)
        {
            script <- "BiocCheck"
            if (onWindows)
                script <- "BiocCheck.bat"
            msg <- strwrap(paste(
                'Failed to copy the', paste0("script/", script), 'script to',
                paste0(file.path(Sys.getenv("R_HOME"), "bin"), "."),
                "If you want to be able to run 'R CMD BiocCheck' you'll",
                "need to copy it yourself to a directory on your PATH,",
                "making sure it is executable.",
                "See the BiocCheck vignette for more information."))
            for (i in 1:length(msg))
                func(msg[i])
        } else {
            func("BiocCheck script installed.")
        }
    }


}