installScript <- function()
{
    srcFile = system.file("script", "BiocCheck", package="BiocCheck")
    destDir = file.path(Sys.getenv("R_HOME"), "bin")
    ## FIXME put this in tryCatch, complain if we can't copy, suggest an alternative
    res = file.copy(srcFile, destDir)
    print(paste("Result was:", res))
    destFile = file.path(destDir, "BiocCheck")
    Sys.chmod(destFile, "0755")
}


isScriptInstalled <- function()
{
    # FIXME throw warning if no exec perms
    file.exists(Sys.getenv("R_HOME"), "bin", "BiocCheck")
}

