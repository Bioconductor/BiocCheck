BiocCheckGitClone <- function(package=".", ...){

    .zeroCounters()
    package <- normalizePath(package)
    isTar <- grepl("\\.tar\\.gz$", package)
    if (isTar)
        .stop("Run 'BiocCheckGitClone' on the git-cloned package directory.")
    if (!dir.exists(package))
        .stop("Package directory does not exist")
    # be careful here:
    if (.Platform$OS.type=="windows")
        package <- gsub("\\\\", "/", package)
    oldwarn <- getOption("warn")
    on.exit(options(warn=oldwarn))
    options(warn=1)

    dots <- list(...)
    if (length(dots) == 1L && is.list(dots[[1]]))
        dots <- dots[[1]]               # command line args come as list

    handleMessage(
        "This is BiocCheckGitClone version ", packageVersion("BiocCheck"), ". ",
        "BiocCheckGitClone is a work in progress. Output and severity of issues may ",
        "change.", indent=0, exdent=0)

    handleCheck("Checking valid files...")
    checkBadFiles(package)

    handleCheck("Checking DESCRIPTION...")
    checkDescription(package)

    handleCheck("Checking CITATION...")
    checkForCitationFile(package)

    ## Summary
    .msg("\n\nSummary:")
    .msg("ERROR count: %d", .BiocCheck$getNum("error"))
    .msg("WARNING count: %d", .BiocCheck$getNum("warning"))
    .msg("NOTE count: %d", .BiocCheck$getNum("note"))
    .msg(paste0(
        "For detailed information about these checks, see the BiocCheck ",
        "vignette, available at ",
        sprintf(
            "https://bioconductor.org/packages/%s/bioc/vignettes/BiocCheck/inst/doc/BiocCheck.html#interpreting-bioccheck-output",
            BiocManager::version())),
        exdent=0)


    if (.BiocCheck$getNum("error") > 0)
    {
        errcode <- 1
        .msg("BiocCheckGitClone FAILED.")
    } else {
        errcode <- 0
    }

    if (isTRUE(dots[["quit-with-status"]])) {
        q("no", errcode)
    } else {
        return(.BiocCheck)
    }
}
