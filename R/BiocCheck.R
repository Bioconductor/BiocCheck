getOptionList <- function()
{
    list(
        make_option("--new-package", action="store_true",
            help="enable checks specific to new packages"),
        make_option("--no-check-dependencies", action="store_true",
            help="disable check for bad dependencies"),
        make_option("--no-check-deprecated", action="store_true",
            help="disable check for usage of deprecated packages"),
        make_option("--no-check-remotes", action="store_true",
            help="disable check for usage of remote packages other than those hosted on CRAN or Bioconductor"),
        make_option("--no-check-version-num", action="store_true",
            help="disable check for valid version number"),
        make_option("--no-check-R-ver", action="store_true",
            help="disable check for valid R version"),
        make_option("--no-check-pkg-size", action="store_true",
            help="disable check for package tarball size"),
        make_option("--no-check-file-size", action="store_true",
            help="disable check for individual file size"),
        make_option("--no-check-bioc-views", action="store_true",
            help="disable biocViews-specific checks (for non-BioC packages)"),
        make_option("--no-check-bbs", action="store_true",
            help="disable BBS-specific checks (for non-BioC packages). Valid DESCRIPTION"),
        make_option("--no-check-description", action="store_true",
            help="disable DESCRIPTION file checks"),
        make_option("--no-check-namespace", action="store_true",
            help="disable namespace checks"),
        make_option("--no-check-vignettes", action="store_true",
            help="disable vignette checks"),
        make_option("--no-check-library-calls", action="store_true",
            help="disable check usage of functions that install or update packages"),
        make_option("--no-check-install-self", action="store_true",
            help="disable check for require or library of itself"),
        make_option("--no-check-coding-practices", action="store_true",
            help="disable check for some common best coding practices"),
        make_option("--no-check-function-len", action="store_true",
            help="disable check for function length"),
        make_option("--no-check-man-doc", action="store_true",
            help="disable checks for man page documentation"),
        make_option("--no-check-news", action="store_true",
            help="disable checks for NEWS file"),
        make_option("--no-check-unit-tests", action="store_true",
            help="disable checks for unit tests"),
        make_option("--no-check-skip-bioc-tests", action="store_true",
            help="disable check for tests that skip when on bioc"),
        make_option("--no-check-formatting", action="store_true",
            help="disable checks for file formatting"),
        make_option("--no-check-CRAN", action="store_true",
            help="disable check for if package exists in CRAN"),
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
    package_install_dir <- installAndLoad(package)

    ## checks
    if (is.null(dots[["no-check-dependencies"]])){
        handleCheck("Checking Package Dependencies...")
        checkForBadDepends(file.path(package_install_dir, "lib", package_name))
    }

    if (is.null(dots[["no-check-deprecated"]])){
        handleCheck("Checking for deprecated package usage...")
        checkDeprecatedPackages(package_dir)
    }

    if (is.null(dots[["no-check-remotes"]])){
        handleCheck("Checking for remote package usage...")
        checkRemotesUsage(package_dir)
    }

    handleCheck("Checking for 'LazyData: true' usage...")
    checkLazyDataUsage(package_dir)

    if (is.null(dots[["no-check-version-num"]])){
        handleCheck("Checking version number...")
        if (!checkingDir) {
            handleCheck("Checking for version number mismatch...")
            checkForVersionNumberMismatch(package, package_dir)
        }

        if (!is.null(dots[["new-package"]]))
            {
                handleCheck("Checking new package version number...")
                checkNewPackageVersionNumber(package_dir)
            } else {
                handleCheck("Checking version number validity...")
                checkVersionNumber(package_dir)
            }
    }

    if (is.null(dots[["no-check-R-ver"]])){
        handleCheck("Checking R Version dependency...")
        checkRVersionDependency(package_dir)
    }

    source_tarball <- grepl("\\.tar\\.gz$", package)
    if (is.null(dots[["no-check-pkg-size"]])){
        handleCheck("Checking package size...")
        if (source_tarball){
            checkPackageSize(package, package_dir, size=5)
        } else {
            handleMessage("Skipped... only checked on source tarball",
                          indent=8)
        }
    }

    if (is.null(dots[["no-check-file-size"]])){
        handleCheck("Checking individual file sizes...")
        checkIndivFileSizes(package_dir)
    }

    if (is.null(dots[["no-check-bioc-views"]]))
    {
        handleCheck("Checking biocViews...")
        result <- checkBiocViews(package_dir)
        if(result)
        {
            .msg("See http://bioconductor.org/developers/how-to/biocViews/")
        }
    }

    if (is.null(dots[["no-check-bbs"]])){
        handleCheck("Checking build system compatibility...")
        checkBBScompatibility(package_dir, source_tarball)
    }

    if (is.null(dots[["no-check-description"]])) {
        ## the following is redundant with other outputs...
        ## handleCheck("Checking DESCRIPTION file...")
        checkDESCRIPTIONFile(package_dir)
    }

    if (is.null(dots[["no-check-namespace"]])){
        handleCheck("Checking DESCRIPTION/NAMESPACE consistency...")
        checkDescriptionNamespaceConsistency(package_name)

        if (suppressMessages(suppressWarnings(requireNamespace("codetoolsBioC",
                                                               quietly=TRUE))))
            {
                handleCheck("Checking for namespace import suggestions...")
                checkImportSuggestions(package_name)
            }
    }

    handleCheck("Checking .Rbuildignore...")
    checkRbuildignore(package_dir)

    if (is.null(dots[["no-check-vignettes"]])) {
        handleCheck("Checking vignette directory...")
        checkVignetteDir(package_dir, checkingDir)
        if ("build-output-file" %in% names(dots)) {
            handleCheck(
                "Checking whether vignette is built with 'R CMD build'..."
            )
            checkIsVignetteBuilt(package_dir, dots[["build-output-file"]])
        }
    }

    if (is.null(dots[["no-check-library-calls"]])){
        handleCheck("Checking package installation calls in R code...")
        checkPkgInstallCalls(package_dir)
    }

    parsedCode <- parseFiles(package_dir)

    if (is.null(dots[["no-check-install-self"]])){
        handleCheck(sprintf("Checking for library/require of %s...",
                            package_name))
        checkForLibraryMe(package_name, parsedCode)
    }

    if (is.null(dots[["no-check-coding-practices"]])){
        handleCheck("Checking coding practice...")
        checkCodingPractice(package_dir, parsedCode, package_name)
    }

    if (is.null(dots[["no-check-function-len"]])){
        handleCheck("Checking function lengths", appendLF=FALSE)
        checkFunctionLengths(parsedCode, package_name)
    }

    if (is.null(dots[["no-check-man-doc"]])){
        handleCheck("Checking man page documentation...")
        checkManDocumentation(package_dir, package_name)
    }

    if (is.null(dots[["no-check-news"]])){
        handleCheck("Checking package NEWS...")
        checkNEWS(package_dir)
    }

    if (is.null(dots[["no-check-unit-tests"]])){
        handleCheck("Checking unit tests...")
        checkUnitTests(package_dir)
    }

    if (is.null(dots[["no-check-skip-bioc-tests"]])){
        handleCheck("Checking skip_on_bioc() in tests...")
        checkSkipOnBioc(package_dir)
    }

    if (is.null(dots[["no-check-formatting"]])){
        handleCheck(
            "Checking formatting of DESCRIPTION, NAMESPACE, ",
            "man pages, R source, and vignette source...")
        checkFormatting(package_dir)
    }

    if (is.null(dots[["no-check-CRAN"]]))
    {
        handleCheck("Checking if package already exists in CRAN...")
        checkIsPackageNameAlreadyInUse(package_name, "CRAN")
    }

    if (!is.null(dots[["new-package"]]))
    {
        handleCheck("Checking if new package already exists in Bioconductor...")
        checkIsPackageNameAlreadyInUse(package_name, "BioCsoft")
        checkIsPackageNameAlreadyInUse(package_name, "BioCann")
        checkIsPackageNameAlreadyInUse(package_name, "BioCexp")
        checkIsPackageNameAlreadyInUse(package_name, "BioCworkflows")
        # TODO: add VIEWS files for books
        # checkIsPackageNameAlreadyInUse(package_name, "BioCbooks")
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
    isTar <- grepl("\\.tar\\.gz$", input)
    if (isTar) {
        pkg_dir <- file.path(
            tempdir(), gsub("(\\w+)_.*", "\\1", basename(input))
        )
        if (!dir.exists(pkg_dir))
            dir.create(pkg_dir)
        on.exit(unlink(pkg_dir, recursive = TRUE))
        suppressMessages({
            untar(input, exdir = pkg_dir)
        })
        desc <- list.files(pkg_dir, pattern = "DESCRIPTION",
            full.names = TRUE, recursive = TRUE)
    } else {
        desc <- file.path(input, "DESCRIPTION")
    }
    read.dcf(desc, fields = "Package")[[1]]
}

.get_package_dir <- function(pkgname)
{
    if (!file.exists(pkgname))
    {
        .stop("'%s' does not exist!", pkgname)
    }
    if (file.info(pkgname)$isdir)
        return(pkgname)

    if(!grepl("\\.tar\\.gz$", pkgname))
    {
        .stop("'%s' is not a directory or package source tarball.", pkgname)
    }

    expectedPackageName <- strsplit(basename(pkgname), "_")[[1]][1]
    dir.create(t <- tempfile())
    untar(pkgname, exdir=t)
    file.path(t, expectedPackageName)
}
