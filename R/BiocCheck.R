#' Check a package's adherence with the Bioconductor Package Guidelines
#'
#' Analyzes an R package for adherence with Bioconductor package guidelines and
#' best practices. The check outputs are categorized into ERROR, WARNING, and
#' NOTE. See the vignette for more details. `BiocCheck` is complementary
#' to `R CMD check`, which should always be run first.
#'
#' `BiocCheck()` reviews R packages for adherence with Bioconductor
#' package guidelines and best practices. See
#' \url{https://contributions.bioconductor.org} for the latest guidance for
#' writing Bioconductor software. Some rationale behind these best practices
#' can be seen in the vignette and pages in the `references` section. The
#' vignette also provides detailed explanations of all the checks performed by
#' `BiocCheck`.
#'
#' `BiocCheck` is called within R with \preformatted{ BiocCheck(<package>)
#' } where `package` points to the source directory or the `.tar.gz`
#' tarball that was created using `R CMD build`.
#'
#' \emph{Note} that `BiocCheck` is complementary to `R CMD check`.
#' `R CMD check` should always be run first for best results.
#'
#' @section dot-options:
#'
#' \describe{
#'   \item{new-package}{ enable checks specific to new packages}
#'   \item{no-check-dependencies}{ disable check for bad dependencies}
#'   \item{no-check-deprecated}{ disable check for usage of deprecated packages}
#'   \item{no-check-remotes}{ disable check for usage of remote packages other
#'     than those hosted on CRAN or Bioconductor}
#'   \item{no-check-version-num}{ disable check for valid version number}
#'   \item{no-check-R-ver}{ disable check for valid R version}
#'   \item{no-check-pkg-size}{ disable check for package tarball size}
#'   \item{no-check-file-size}{ disable check for individual file size}
#'   \item{no-check-bioc-views}{ disable biocViews-specific checks (for non-BioC
#'     packages)}
#'   \item{no-check-bbs}{ disable BBS-specific checks (for non-BioC packages).
#'     Valid DESCRIPTION}
#'   \item{no-check-description}{ disable DESCRIPTION file checks}
#'   \item{no-check-vignettes}{ disable vignette checks}
#'   \item{no-check-library-calls}{ disable check usage of functions that
#'     install or update packages}
#'   \item{no-check-install-self}{ disable check for require or library of
#'     itself}
#'   \item{no-check-coding-practices}{ disable check for some common best coding
#'     practices}
#'   \item{no-check-function-len}{ disable check for function length}
#'   \item{no-check-man-doc}{ disable checks for man page documentation}
#'   \item{no-check-news}{ disable checks for NEWS file}
#'   \item{no-check-unit-tests}{ disable checks for unit tests}
#'   \item{no-check-skip-bioc-tests}{ disable check for tests that skip when on
#'     bioc}
#'   \item{no-check-formatting}{ disable checks for file formatting}
#'   \item{no-check-CRAN}{ disable check for if package exists in CRAN}
#'   \item{no-check-bioc-help}{ disable check for registration on Bioconductor
#'     mailing list and support site}
#'   \item{build-output-file}{ file containing R CMD build output, for
#'     additional analysis}
#'   \item{quit-with-status}{ enable exit code option when performing check}
#' }
#'
#' @param package The path to an R package directory or tarball (`.tar.gz`).
#'   The `BiocCheck` function is intended to be run from the package
#'   directory; therefore, the current working directory (given by `getwd()`)
#'   is the default.
#'
#' @param checkDir The directory where the `BiocCheck` output directory will be
#'   stored. By default, it will be placed in the same directory as the package
#'   directory i.e., `dirname(pkg_dir)`.
#'
#' @param debug Whether to append the names of functions that correspond to
#' each condition raised by `BiocCheck` in the written log (i.e., in the
#' `'<package_name>.BiocCheck'` folder). This option is only relevant to
#' developers and contributors to `BiocCheck`.
#'
#' @param callr logical(1) Whether to use the `callr` package to run `BiocCheck`
#'   in an isolated R session to prevent namespace collisions.
#'
#' @param \dots See the details section for available options. When running
#' `BiocCheck`, options can be specified as:
#' \preformatted{ BiocCheck(package, `no-check-vignettes`=TRUE) }
#'
#' @return `BiocCheck()` is chiefly called for the side effect of the check
#'   reporting. The function also creates a `<package_name>.BiocCheck` folder
#'   and returns a `BiocCheck` reference class with three main list elements:
#'
#' \item{error}{Items to address before the package can be accepted}
#'
#' \item{warning}{Strongly suggested items that may require attention}
#'
#' \item{note}{Items to consider, though not required, before acceptance}
#'
#' @author Dan Tenenbaum, Lori Shepherd, and Marcel Ramos
#'
#' @md
#'
#' @references \url{https://contributions.bioconductor.org}
#' @seealso \link{BiocCheck-class}, \link{Message-class}
#'
#' @importFrom utils untar
#'
#' @examples
#'
#' packageDir <- system.file("testpackages", "testpkg0", package="BiocCheck")
#' BiocCheck(packageDir, `quit-with-status`=FALSE)
#'
#' @export BiocCheck
BiocCheck <- function(
    package = getwd(),
    checkDir = dirname(package),
    debug = FALSE,
    callr = FALSE,
    ...
) {
    if (callr) {
        callr::r(
            function(...) { BiocCheck:::BiocCheckRun(...) },
            args = list(
                package = package, checkDir = checkDir, debug = debug, ...
            ),
            cmdargs = c("--no-echo", "--no-save", "--no-restore"),
            show = TRUE
        )
    } else {
        BiocCheckRun(
            package = package, checkDir = checkDir, debug = debug, ...
        )
    }
}

BiocCheckRun <-
    function(package, checkDir, debug, ...)
{
    .zeroCounters()
    package <- normalizePath(package)
    if (!file.exists(package))
        .stop("Package directory or tarball does not exist.")
    if (!length(package))
        .stop("Supply a package directory or source tarball.")
    # be careful here:
    if (identical(.Platform$OS.type, "windows"))
        package <- gsub("\\\\", "/", package)

    dots <- list(...)
    if (length(dots) == 1L && is.list(dots[[1]]))
        dots <- dots[[1]]               # command line args come as list

    oldwarn <- getOption("warn")
    on.exit(options(warn=oldwarn))
    options(warn=1)

    isTar <- grepl("\\.tar\\.gz$", package)
    checkingDir <- !isTar && file.info(package)[["isdir"]]
    package_dir <- .getPackageDir(package, isTar)
    package_name <- .getPackageName(package)
    package_install_dir <- installAndLoad(package)
    libloc <- file.path(package_install_dir, "lib")
    pkgver <- .getPackageVersion(package_dir)
    bioccheckver <- as.character(packageVersion("BiocCheck"))
    biocver <- as.character(BiocManager::version())
    checkDir <- .getBiocCheckDir(package_name, checkDir)
    isBBS <- Sys.getenv("IS_BIOC_BUILD_MACHINE")
    onBBS <- nzchar(isBBS) && identical(tolower(isBBS), "true")
    hasAdmin <- nzchar(Sys.getenv("BIOC_DEVEL_PASSWORD"))

    .BiocCheck$metadata <- list(
        BiocCheckVersion = bioccheckver,
        BiocVersion = biocver,
        Package = package_name, PackageVersion = pkgver,
        sourceDir = package_dir, installDir = package_install_dir,
        BiocCheckDir = checkDir,
        platform = .Platform$OS.type, isTarBall = isTar
    )
    .BiocCheck$show_meta()

    handleMessage("* Installing package...", indent = 0, exdent = 0)

    # BiocCheck checks --------------------------------------------------------
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
        handleCheck("Checking R version dependency...")
        checkRVersionDependency(package_dir)
    }

    source_tarball <- grepl("\\.tar\\.gz$", package)
    if (is.null(dots[["no-check-pkg-size"]])){
        handleCheck("Checking package size...")
        if (source_tarball){
            checkPackageSize(package, package_dir, size=5)
        } else {
            handleMessage("Skipped... only checked on source tarball", indent=4)
        }
    }

    if (is.null(dots[["no-check-file-size"]])){
        handleCheck("Checking individual file sizes...")
        checkIndivFileSizes(package_dir)
        checkDataFileSizes(package_dir)
    }

    if (is.null(dots[["no-check-bioc-views"]]))
    {
        handleCheck("Checking biocViews...")
        result <- checkBiocViews(package_dir)
        if(result)
        {
            .msg("Search 'biocViews' at https://contributions.bioconductor.org")
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

    handleCheck("Checking .Rbuildignore...")
    checkRbuildignore(package_dir)

    handleCheck("Checking for stray BiocCheck output folders...")
    checkBiocCheckOutputFolder(package_dir, package_name)

    if (!isTar) {
        handleCheck("Checking for inst/doc folders...")
        checkInstDocFolder(package_dir, package_name)
    }

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
        if (hasAdmin) {
            checkForBiocDevelSubscription(package_dir)
        } else {
            handleNote(
                "Cannot determine whether maintainer is subscribed to the ",
                "Bioc-Devel mailing list (requires admin credentials). ",
                "Subscribe here: ",
                "https://stat.ethz.ch/mailman/listinfo/bioc-devel"
            )
        }

        handleCheck("Checking for support site registration...")
        checkForSupportSiteRegistration(package_dir)
    }

    # BiocCheck results -------------------------------------------------------
    message("\n\U2500 BiocCheck results \U2500\U2500")
    .msg(
        "%d ERRORS | %d WARNINGS | %d NOTES",
        .BiocCheck$getNum("error"),
        .BiocCheck$getNum("warning"),
        .BiocCheck$getNum("note")
    )
    message(
        "\nSee the ", package_name, ".BiocCheck folder and run\n",
        "    browseVignettes(package = 'BiocCheck')\n",
        "for details."
    )

    .BiocCheck$report(debug, onBBS)

    if (isTRUE(dots[["quit-with-status"]])) {
        errcode <- as.integer(.BiocCheck$getNum("error") > 0)
        q("no", errcode)
    }

    return(.BiocCheck)

}

# input can either be tarball or pkg source dir
.getPackageName <- function(input)
{
    isTar <- grepl("\\.tar\\.gz$", input)
    if (isTar) {
        tmp_pkg_dir <- .tempPackageDirTarball(input)
        on.exit({
            unlink(dirname(tmp_pkg_dir), recursive = TRUE)
        })
        desc <- file.path(tmp_pkg_dir, "DESCRIPTION")
        if (!file.exists(desc))
            .stop("The package folder is inconsistent with the tarball name.")
    } else {
        desc <- file.path(input, "DESCRIPTION")
    }
    read.dcf(desc, fields = "Package")[[1]]
}

.tempPackageDirTarball <- function(pkg_tarball)
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

.getPackageDir <- function(input, isTar) {
    if (isTar) {
        .tempPackageDirTarball(input)
    } else {
        if (file.info(input)[["isdir"]])
            input
        else
            .stop("'%s' is not a directory or package source tarball.", input)
    }
}

.getBiocCheckDir <- function(pkgName, checkDir) {
    checkDir <- normalizePath(checkDir, winslash = "/")
    file.path(
        checkDir, paste(pkgName, "BiocCheck", sep = ".")
    )
}

.getPackageVersion <- function(pkgdir) {
    desc <- file.path(pkgdir, "DESCRIPTION")
    as.character(read.dcf(desc, fields = "Version"))
}
