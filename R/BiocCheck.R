## 'package' can be a directory or tarball
BiocCheck <- function(package=".", ...)
{
    .zeroCounters()
    package <- normalizePath(package)
    if (!file.exists(package))
        stop("Package directory or tarball does not exist")
    # be careful here:
    if (identical(.Platform$OS.type, "windows"))
        package <- gsub("\\\\", "/", package)
    oldwarn <- getOption("warn")
    on.exit(options(warn=oldwarn))
    options(warn=1)
    isTar <- grepl("\\.tar\\.gz$", package)
    checkingDir <- !isTar && file.info(package)[["isdir"]]
    package_dir <- .get_package_dir(package, isTar)

    dots <- list(...)
    if (length(dots) == 1L && is.list(dots[[1]]))
        dots <- dots[[1]]               # command line args come as list

    if (!length(package))
        .stop("Supply a package directory or source tarball.")
    package_name <- .get_package_name(package)

    handleMessage(
        "This is BiocCheck version ", packageVersion("BiocCheck"), ". ",
        "BiocCheck is a work in progress. Output and severity of issues may ",
        "change. Installing package...", indent=0, exdent=0)
    package_install_dir <- installAndLoad(package)
    libloc <- file.path(package_install_dir, "lib")

    ## checks
    if (is.null(dots[["no-check-dependencies"]])){
        handleCheck("Checking Package Dependencies...")
        checkForBadDepends(package, libloc)
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

        if (!is.null(dots[["new-package"]])) {
            handleCheck("Checking new package version number...")
            checkNewPackageVersionNumber(package_dir)
        } else {
            handleCheck("Checking version number validity...")
            checkVersionNumber(package_dir)
        }
    }

    if (is.null(dots[["no-check-R-ver"]])) {
        handleCheck("Checking R Version dependency...")
        checkRVersionDependency(package_dir)
    }

    source_tarball <- grepl("\\.tar\\.gz$", package)
    if (is.null(dots[["no-check-pkg-size"]])){
        handleCheck("Checking package size...")
        if (source_tarball){
            checkPackageSize(package, package_dir, size=5)
        } else {
            handleMessage("Skipped... only checked on source tarball", indent=6)
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
        checkDescriptionNamespaceConsistency(package_name, libloc)

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
        checkForLibraryRequire(package_dir)
    }

    if (is.null(dots[["no-check-coding-practices"]])){
        handleCheck("Checking coding practice...")
        checkCodingPractice(package_dir, parsedCode, package_name)
    }

    if (is.null(dots[["no-check-function-len"]])){
        handleCheck("Checking function lengths...")
        checkFunctionLengths(parsedCode, package_name)
    }

    if (is.null(dots[["no-check-man-doc"]])){
        handleCheck("Checking man page documentation...")
        checkManDocumentation(package_dir, package_name, libloc)
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

    if (is.null(dots[["no-check-bioc-help"]])) {
        handleCheck("Checking for bioc-devel mailing list subscription...")
        if (!nzchar(Sys.getenv("BIOC_DEVEL_PASSWORD"))) {
            handleNote(
                "Cannot determine whether maintainer is subscribed to the ",
                "bioc-devel mailing list (requires admin credentials). ",
                "Subscribe here: ",
                "https://stat.ethz.ch/mailman/listinfo/bioc-devel"
            )
        } else {
            checkForBiocDevelSubscription(package_dir)
        }

        handleCheck("Checking for support site registration...")
        checkForSupportSiteRegistration(package_dir)
    }

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
        .msg("BiocCheck FAILED.")
    } else {
        errcode <- 0
    }

    if (isTRUE(dots[["quit-with-status"]])) {
        q("no", errcode)
    } else {
        return(.BiocCheck)
    }

}

# input can either be tarball or pkg source dir
.get_package_name <- function(input)
{
    isTar <- grepl("\\.tar\\.gz$", input)
    if (isTar) {
        tmp_pkg_dir <- .temp_package_dir_tarball(input)
        on.exit({
            unlink(dirname(tmp_pkg_dir), recursive = TRUE)
        })
        desc <- file.path(tmp_pkg_dir, "DESCRIPTION")
        if (!file.exists(desc))
            stop("The package folder is inconsistent with the tarball name.",
                call. = FALSE)
    } else {
        desc <- file.path(input, "DESCRIPTION")
    }
    read.dcf(desc, fields = "Package")[[1]]
}

.temp_package_dir_tarball <- function(pkg_tarball)
{
    tmp_dir <- tempfile()
    pkg_name <- gsub("(\\w+)_.*", "\\1", basename(pkg_tarball))
    if (!dir.exists(tmp_dir))
        dir.create(tmp_dir)
    suppressMessages({
        untar(pkg_tarball, exdir = tmp_dir)
    })
    # pkg_name must match with tarred folder
    file.path(tmp_dir, pkg_name)
}

.get_package_dir <- function(input, isTar) {
    if (isTar) {
        .temp_package_dir_tarball(input)
    } else {
        if (file.info(input)[["isdir"]])
            input
        else
            .stop("'%s' is not a directory or package source tarball.", input)
    }
}
