getOptionList <- function()
{
    list(
        make_option("--no-check-vignettes", action="store_true",
            help="disable vignette checks"),
        make_option("--new-package", action="store_true",
            help="enable checks specific to new packages"),
        make_option("--no-check-CRAN", action="store_true",
            help="disable check for if package exists in CRAN"),
        make_option("--no-check-bioc-views", action="store_true",
            help="disable biocViews-specific checks (for non-BioC packages)"),
        make_option("--no-check-bioc-help", action="store_true",
            help="disable check for registration on Bioconductor mailing list and support site"),
        make_option("--build-output-file", type="character",
            help="file containing R CMD build output, for additional analysis",
            metavar="build-output-file"),
        make_option("--quit-with-status", action="store_true",
            help="enable exit code option when performing check")
        )
}

getArgParser <- function()
{
    option_list <- getOptionList()
    OptionParser(usage = "R CMD BiocCheck [options] package",
        option_list=option_list)
}

usage <- function()
{
    print_help(getArgParser())
    if (interactive())
    {
        cat("When running interactively, options can be passed like so:\n")
        cat("BiocCheck(package, `no-check-vignettes`=TRUE)\n")
    }
}

.BiocCheckFromCommandLine <- function()
{
    parser <- getArgParser()
    arguments <- parse_args(parser, positional_arguments = 1)
    opt <- arguments$options
    file <- arguments$args

    opt$Called_from_command_line <- TRUE
    BiocCheck(file, opt)
}

BiocCheck <- function(package=".", ...)
{
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
        "This is BiocCheck version ", packageVersion("BiocCheck"), ". ",
        "BiocCheck is a work in progress. Output and severity of issues may ",
        "change. Installing package...", indent=0, exdent=0)
    installAndLoad(package)

    ## checks

    if (!checkingDir) {
        handleCheck("Checking for version number mismatch...")
        checkForVersionNumberMismatch(package, package_dir)
    }

    checkForBadDepends(file.path(tempdir(), "lib", package_name))


    if (is.null(dots[["no-check-vignettes"]]))
    {
        pkgType <- getPkgType(package_dir)
        if ((is.na(pkgType)) || pkgType == "Software")
        {
            handleCheck("Checking vignette directory...")
            msg <- sprintf(
                "This is a%s package",
                if (is.na(pkgType)) "n unknown type of" else " software")
            handleMessage(msg)
            checkVignetteDir(package_dir, checkingDir)
            if ("build-output-file" %in% names(dots))
            {
                handleCheck(
                    "Checking whether vignette is built with 'R CMD build'...")
                checkIsVignetteBuilt(package_dir, dots[["build-output-file"]])
            }
        } else {
            handleMessage(
                "This is not a software package, skipping vignette checks...")
        }
    }

    handleCheck("Checking version number...")
    if (!is.null(dots[["new-package"]]))
    {
        handleCheck("Checking new package version number...")
        checkNewPackageVersionNumber(package_dir)
    } else {
        handleMessage("Checking version number validity...")
        checkVersionNumber(package_dir)
    }

    handleCheck("Checking R Version dependency...")
    checkRVersionDependency(package_dir)

    source_tarball <- grepl("\\.tar\\.gz$", package)
    handleCheck("Checking package size...")
    if (source_tarball){
        checkPackageSize(package, package_dir, size=4)
    } else {
        handleMessage("Skipped... only checked on source tarball",
                      indent=8)
    }

    handleCheck("Checking individual file sizes...")
    checkIndivFileSizes(package_dir)

    if (is.null(dots[["no-check-bioc-views"]]))
    {
        handleCheck("Checking biocViews...")
        result <- checkBiocViews(package_dir)
        if(result)
        {
            .msg("See http://bioconductor.org/developers/how-to/biocViews/")
        }
    }
    handleCheck("Checking build system compatibility...")
    checkBBScompatibility(package_dir)

    handleCheck("Checking unit tests...")
    checkUnitTests(package_dir)

    handleCheck("Checking skip_on_bioc() in tests...")
    checkSkipOnBioc(package_dir)

    handleCheck("Checking library calls...")
    checkLibraryCalls(package_dir)

    handleCheck("Checking coding practice...")
    checkCodingPractice(package_dir)

    parsedCode <- parseFiles(package_dir)

    handleCheck("Checking native routine registration...")
    checkRegistrationOfEntryPoints(package_name, parsedCode)

    if (suppressMessages(suppressWarnings(requireNamespace("codetoolsBioC",
        quietly=TRUE))))
    {
        handleCheck("Checking for namespace import suggestions...")
        checkImportSuggestions(package_name)
    }

    handleCheck("Checking for deprecated package usage...")
    checkDeprecatedPackages(package_dir)

    handleCheck("Checking parsed R code in R directory, examples, vignettes...")

    handleCheck("Checking for direct slot access...")
    checkForDirectSlotAccess(parsedCode, package_name)

    handleCheck("Checking for browser()...")
    res <- findSymbolInParsedCode(parsedCode, package_name, "browser",
        "SYMBOL_FUNCTION_CALL")
    if (res > 0)
        handleWarning("Remove browser() statements (found in ", res, " files)")

    handleCheck("Checking for <<-...")
    res <- findSymbolInParsedCode(parsedCode, package_name, "<<-",
        "LEFT_ASSIGN")
    if (res > 0)
        handleNote("Avoid '<<-' if possible (found in ", res, " files)")

    handleCheck(sprintf("Checking for library/require of %s...",
        package_name))
    checkForLibraryMe(package_name, parsedCode)

    handleCheck("Checking DESCRIPTION/NAMESPACE consistency...")
    checkDescriptionNamespaceConsistency(package_name)

    handleCheck("Checking function lengths", appendLF=FALSE)
    checkFunctionLengths(parsedCode, package_name)

    handleCheck("Checking man pages...") # could add more man page checks...
    checkForValueSection(package_dir)

    handleCheck("Checking exported objects have runnable examples...")
    checkExportsAreDocumented(package_dir, package_name)

    handleCheck("Checking package NEWS...")
    checkNEWS(package_dir)

    handleCheck(
        "Checking formatting of DESCRIPTION, NAMESPACE, ",
        "man pages, R source, and vignette source...")
    checkFormatting(package_dir)

    handleCheck("Checking for canned comments in man pages...")
    checkForPromptComments(package_dir)

    if (is.null(dots[["no-check-CRAN"]]))
    {
        handleCheck("Checking if package already exists in CRAN...")
        checkIsPackageAlreadyInRepo(package_name, "CRAN")
    }

    if (!is.null(dots[["new-package"]]))
    {
        handleCheck("Checking if new package already exists in Bioconductor...")
        checkIsPackageAlreadyInRepo(package_name, "BioCsoft")
    }

    if (is.null(dots[["no-check-bioc-help"]]))
    {
        handleCheck("Checking for bioc-devel mailing list subscription...")
        checkForBiocDevelSubscription(package_dir)

        handleCheck("Checking for support site registration...")
        checkForSupportSiteRegistration(package_dir)
    }

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
        .msg("BiocCheck FAILED.")
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

.get_package_name <- function(input)
{
    strsplit(basename(input), "_")[[1]][1]
}

.get_package_dir <- function(pkgname)
{
    if (!file.exists(pkgname))
    {
        stop(.printf("'%s' does not exist!", pkgname))
    }
    if (file.info(pkgname)$isdir)
        return(pkgname)

    if(!grepl("\\.tar\\.gz$", pkgname))
    {
        stop(.printf("'%s' is not a directory or package source tarball.",
            pkgname))
    }

    expectedPackageName <- strsplit(basename(pkgname), "_")[[1]][1]
    t = tempdir()
    untar(pkgname, exdir=t)
    file.path(t, expectedPackageName)
}
