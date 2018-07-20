.isScriptInstalled <- function()
{
    if (all(as.logical(nchar(
        Sys.which(c("BiocCheck", "BiocCheckGitClone"))))))
        return(TRUE)

    onWindows <- (.Platform$OS.type == "windows")

    if (onWindows)
        file <- c("BiocCheck.bat", "BiocCheckGitClone.bat")
    else
        file <- c("BiocCheck", "BiocCheckGitClone")

    path <- file.path(Sys.getenv("R_HOME"), "bin")

    all(file.exists(file.path(path, file)))

}

.onLoad <- function(libname, pkgname)
{
    if (.isScriptInstalled())
        return()
    .installScript()
}
