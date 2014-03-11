.printf <- function(...) cat(noquote(sprintf(...)), "\n")


getArgParser <- function()
{
    option_list <- list(
        make_option("--no-check-vignettes", action="store_true",
            help="disable vignette checks"),
        make_option("--new-package", action="store_true",
            help="enable checks specific to new packages"),
        make_option("--no-check-bioc-views", action="store_true",
            help="disable biocViews-specific checks (for non-BioC packages)")

        )
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

BiocCheck <- function(package, ...)
{
    loadRefClasses()
    package <- normalizePath(package)
    oldwarn <- getOption("warn")
    on.exit(options(warn=oldwarn))
    options(warn=1)
    checkingDir <- FALSE
    if (file.exists(package) && file.info(package)$isdir)
        checkingDir <- TRUE

    d <- list(...)
    if (length(d))
        dots <- list(...)[[1]]
    else
        dots <- list()

    if (length(package)==0)
        .stop("Supply a package directory or source tarball.")
    package_dir <- .get_package_dir(package)
    package_name <- .get_package_name(package)


    handleMessage(sprintf("This is BiocCheck, version %s.",
        packageVersion("BiocCheck")))

    handleMessage(paste0("BiocCheck is a work in progress. Output",
        "  and severity of issues may change."))

    handleMessage(paste0("For detailed information about these checks, ",
        "see the BiocCheck vignette, temporarily available at ",
            "http://bioconductor.org/developers/BiocCheck.html#Interpreting_BiocCheck_output"))

    handleMessage("Installing package...")
    installAndLoad(package)

    ## checks

    checkForBadDepends(file.path(tempdir(), "lib", package_name))


    if (is.null(dots[["no-check-vignettes"]]))
    {
        handleMessage("Checking vignette directories...")
        checkVignetteDir(package_dir, checkingDir)
    }
    handleMessage("Checking version number...")
    if (!is.null(dots[["new-package"]]))
    {
        handleMessage("Checking new package version number...")
        checkNewPackageVersionNumber(package_dir)
    }
    checkVersionNumber(package_dir, !is.null(dots[["new-package"]]))

    if (is.null(dots[["no-check-bioc-views"]]))
    {
        handleMessage("Checking biocViews...")
        checkBiocViews(package_dir)
    }
    handleMessage("Checking build system compatibility...")
    checkBBScompatibility(package_dir)
    handleMessage("Checking unit tests...")
    checkUnitTests(package_dir)
    handleMessage("Checking native routine registration...")
    checkRegistrationOfEntryPoints(package_name)
    if (suppressMessages(suppressWarnings(require(codetoolsBioC))))
    {
        handleMessage("Checking for namespace import suggestions...")
        checkImportSuggestions(package_name)
    }

    handleMessage("Checking for deprecated package usage...")
    checkDeprecatedPackages(package_dir)

    handleMessage("Parsing R code in R directory, examples, vignettes...")

    parsedCode <- parseFiles(package_dir)

    handleMessage("Checking for T...")
    res <- findSymbolInParsedCode(parsedCode, package_name, "T",
        "SYMBOL")
    if (res > 0) handleRecommended(sprintf(
        "Use TRUE instead of T (found in %s files)",
        res))
    handleMessage("Checking for F...")
    res <- findSymbolInParsedCode(parsedCode, package_name, "F",
        "SYMBOL")
    if (res > 0) handleRecommended(sprintf(
        "Use FALSE instead of F (found in %s files)",
        res))

    handleMessage("Checking for browser()...")
    res <- findSymbolInParsedCode(parsedCode, package_name, "browser",
        "SYMBOL_FUNCTION_CALL")
    if (res > 0)
        handleRecommended(sprintf(
            "Remove browser() statements (found in %s files)",
            res))

    handleMessage("Checking for <<-...")
res <- findSymbolInParsedCode(parsedCode, package_name, "<<-",
    "LEFT_ASSIGN")
if (res > 0)
    handleRecommended(sprintf("Avoid <<- if possible (found in %s files)",
        res))

    handleMessage(sprintf("Checking for library/require of %s...",
        package_name))
    checkForLibraryMe(package_name, parsedCode)


    handleMessage("Checking DESCRIPTION/NAMESPACE consistency...")
    checkDescriptionNamespaceConsistency(package_name)

    handleMessage("Checking function lengths", appendLF=FALSE)
    checkFunctionLengths(parsedCode, package_name)

    handleMessage("Checking exported objects have runnable examples...")
    checkExportsAreDocumented(package_dir, package_name)

    handleMessage("Checking package NEWS...")
    checkNEWS(package_dir)

    handleMessage(paste0("Checking formatting of DESCRIPTION, NAMESPACE, ",
        "man pages, R source, and vignette source..."))
    checkFormatting(package_dir)

    handleMessage("Checking for canned comments in man pages...")
    checkForPromptComments(package_dir)


    ## Summary
    .msg("\n\nSummary:")
    .msg("REQUIRED count: %s", .requirements$getNum())
    .msg("RECOMMENDED count: %s", .recommendations$getNum())
    .msg("CONSIDERATION count: %s", .considerations$getNum())

    if (.requirements$getNum() > 0)
    {
        errcode <- 1
        .msg("BiocCheck FAILED.")
    } else {
        errcode <- 0
    }

    if ("Called_from_command_line" %in% names(dots))
    {
        q("no", errcode)
    } else {
        return (list(requirements=.requirements$get(),
            recommendations=.recommendations$get(),
            considerations=.considerations$get()))
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