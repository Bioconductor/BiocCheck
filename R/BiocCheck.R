.printf <- function(...) cat(noquote(sprintf(...)), "\n")

.BiocCheckFromCommandLine <- function()
{
    option_list <- list(
        make_option("--no-check-vignettes", action="store_true",
            help="disable vignette checks"),
        make_option("--new-package", action="store_true",
            help="enable checks specific to new packages")
        )
    parser <- OptionParser(usage = "R CMD BiocCheck [options] package", option_list=option_list)
    arguments <- parse_args(parser, positional_arguments = 1)
    opt <- arguments$options
    file <- arguments$args

    opt$Called_from_command_line <- TRUE
    BiocCheck(file, opt)
}

BiocCheck <- function(package, ...)
{
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
    handleMessage("Checking biocViews...")
    checkBiocViews(package_dir)
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

    handleMessage("Checking for deprecated package usage....")
    checkDeprecatedPackages(package_dir)

    handleMessage("Parsing R code in R directory, examples, vignettes...")

    parsedCode <- parseFiles(package_dir)

    handleMessage("Checking for T and F symbols...")
    checkTorF(parsedCode)
    handleMessage("Checking for .C...")
    checkForDotC(parsedCode, package_name)

    handleMessage("Checking DESCRIPTION/NAMESPACE consistency...")
    checkDescriptionNamespaceConsistency(package_name)

    ## Summary
    .msg("Summary:")
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