getOptionList2 <- function()
{
    list(
         make_option("--quit-with-status", action="store_true",
            help="enable exit code option when performing check")
        )
}

getArgParser2 <- function()
{
    option_list <- getOptionList2()
    OptionParser(usage = "R CMD BiocCheckGitClone [options] package",
        option_list=option_list)
}

usage2 <- function()
{
    print_help(getArgParser2())
    if (interactive())
    {
        cat("When running interactively, options can be passed like so:\n")
        cat("BiocCheck(package, `quit-with-status`=TRUE)\n")
    }
}

.BiocCheckGitCloneFromCommandLine <- function()
{
    .Deprecated(msg="\nDEPRECATED: R CMD BiocCheckGitClone\n  Running BiocCheckGitClone from command line is deprecated in 3.12 and will be removed in 3.13.\n  Please run from within R as\n `BiocCheckGitClone(<pkg>)`")
    parser <- getArgParser2()
    arguments <- parse_args(parser, positional_arguments = 1)
    opt <- arguments$options
    file <- arguments$args

    opt$Called_from_command_line <- TRUE
    BiocCheckGitClone(file, opt)
}


BiocCheckGitClone <- function(package=".", ...){

    .zeroCounters()
    package <- normalizePath(package)
    isTar <- grepl("\\.tar\\.gz$", package)
    if (isTar)
        stop("BiocCheckGitClone is run on the raw git clone of package repository")
    if (!dir.exists(package))
        stop("Package directory does not exist")
    # be careful here:
    if (.Platform$OS.type=="windows")
        package=gsub("\\\\", "/", package)
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
    .msg("ERROR count: %d", .error$getNum())
    .msg("WARNING count: %d", .warning$getNum())
    .msg("NOTE count: %d", .note$getNum())
    .msg(paste0(
        "For detailed information about these checks, see the BiocCheck ",
        "vignette, available at ",
        sprintf(
            "https://bioconductor.org/packages/%s/bioc/vignettes/BiocCheck/inst/doc/BiocCheck.html#interpreting-bioccheck-output",
            BiocManager::version())),
        exdent=0)


    if (.error$getNum() > 0)
    {
        errcode <- 1
        .msg("BiocCheckGitClone FAILED.")
    } else {
        errcode <- 0
    }

    if (isTRUE(dots[["quit-with-status"]]) ||
        "Called_from_command_line" %in% names(dots))
    {
        q("no", errcode)
    } else {
        return (list(
            error=.error$get(),
            warning=.warning$get(),
            note=.note$get()))
    }
}
