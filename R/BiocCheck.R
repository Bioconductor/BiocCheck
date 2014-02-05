.printf <- function(...) cat(noquote(sprintf(...)), "\n")

.BiocCheckFromCommandLine <- function()
{
    option_list <- list(
        make_option("--no-check-vignettes", action="store_true",
            help="disable vignette checks"),
        make_option("--new-package", action="store_true",
            help="enable checks specific to new packages"),
        make_option("--no-check-bioc-views", action="store_true",
            help="disable biocViews-specific checks (for non-BioC packages)")

        )
    parser <- OptionParser(usage = "R CMD BiocCheck [options] package",
        option_list=option_list)
    arguments <- parse_args(parser, positional_arguments = 1)
    opt <- arguments$options
    file <- arguments$args

    opt$Called_from_command_line <- TRUE
    BiocCheck(file, opt)
}

BiocCheck <- function(package, ...)
{
    package <- normalizePath(package)
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



    handleMessage("Installing package...")
    installAndLoad(package)

    checkForBadDepends(file.path(tempdir(), "lib", package_name))


    ## checks
    if (is.null(dots[["no-check-vignettes"]]))
    {
        handleMessage("Checking vignette directories...")
        checkVignetteDir(package_dir)
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

    ## FIXME - these should probably tell the user
    ## which files (with line number?) the 'offending'
    ## symbols were found in.

    handleMessage("Checking for T...")
    res <- findSymbolInParsedCode(parsedCode, package_name, "T",
        "SYMBOL")
    if (res > 0) handleWarning(sprintf("T was found in %s files",
        res))
    handleMessage("Checking for F...")
    res <- findSymbolInParsedCode(parsedCode, package_name, "F",
        "SYMBOL")
    if (res > 0) handleWarning(sprintf("F was found in %s files",
        res))

    handleMessage("Checking for .C()...")
    res <- findSymbolInParsedCode(parsedCode, package_name, ".C",
        "SYMBOL_FUNCTION_CALL")
    if (res > 0) handleNote(sprintf(".C() was found in %s files",
        res))
    handleMessage("Checking for browser()...")
    res <- findSymbolInParsedCode(parsedCode, package_name, "browser",
        "SYMBOL_FUNCTION_CALL")
    if (res > 0)
        handleWarning(sprintf("browser() was found in %s files",
            res))

    handleMessage("Checking for <<-...")
res <- findSymbolInParsedCode(parsedCode, package_name, "<<-",
    "LEFT_ASSIGN")
if (res > 0)
    handleWarning(sprintf("<<- was found in %s files", res))

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

    handleMessage(paste0("Checking formatting of DESCRIPTION, NAMESPACE,\n",
        "  man pages, R source, and vignette source..."))
    checkFormatting(package_dir)

    ## Summary
    .msg("\nSummary:")
    .msg("REQUIRED count: %s", .errors$getNum())
    .msg("RECOMMENDED count: %s", .warnings$getNum())
    .msg("NOTE count: %s", .notes$getNum())

    if (.errors$getNum() > 0)
        .msg("BiocCheck FAILED.")

    if ("Called_from_command_line" %in% names(dots))
    {
        q("no", 1)
    } else {
        return (list(errors=.errors$get(),
            warnings=.warnings$get(),
            notes=.notes$get()))
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