.isScriptInstalled <- function()
{
    if (nchar(Sys.which("BiocCheck")))
        return(TRUE)

    onWindows <- (.Platform$OS.type == "windows")

    if (onWindows)
        file <- "BiocCheck.bat"
    else
        file <- "BiocCheck"

    path <- file.path(Sys.getenv("R_HOME"), "bin")

    all(file.exists(file.path(path, file)))

}

.onLoad <- function(libname, pkgname)
{
    if (.isScriptInstalled())
        return()
    .installScript()
}
