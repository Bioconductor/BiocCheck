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
    # be careful here:
    if (.Platform$OS.type=="windows")
        package=gsub("\\\\", "/", package)
    oldwarn <- getOption("warn")
    on.exit(options(warn=oldwarn))
    options(warn=1)
    checkingDir <- FALSE
    if (file.exists(package) && file.info(package)$isdir)
        checkingDir <- TRUE

    dots <- list(...)
    if (length(dots) == 1L && is.list(dots[[1]]))
        dots <- dots[[1]]               # command line args come as list

    if (length(package)==0)
        .stop("Supply a package directory or source tarball.")
    package_dir <- .get_package_dir(package)
    package_name <- .get_package_name(package)

    handleMessage(
        "This is BiocCheckGitClone version ", packageVersion("BiocCheck"), ". ",
        "BiocCheckGitClone is a work in progress. Output and severity of issues may ",
        "change. Installing package...", indent=0, exdent=0)
    installAndLoad(package)

    source_tarball <- grepl("\\.tar\\.gz$", package)
    if(source_tarball)
        stop("BiocCheckGitClone is run on the raw git clone of package repository")

    handleCheck("Checking valid files...")
    checkBadFiles(package_dir)

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
