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
#'   * `build-output-file`: file containing R CMD build output, for
#'     additional analysis
#'   * `new-package`:  enable checks specific to new packages
#'   * `no-check-bbs`: disable BBS-specific checks (for non-BioC packages).
#'     Valid DESCRIPTION
#'   * `no-check-bioc-help`: disable check for registration on Bioconductor
#'   * `no-check-bioc-views`: disable biocViews-specific checks (for non-BioC
#'     packages)
#'     mailing list and support site
#'   * `no-check-coding-practices`: disable check for some common best coding
#'     practices
#'   * `no-check-CRAN`:  disable check for if package exists in CRAN
#'   * `no-check-dependencies`:  disable check for bad dependencies
#'   * `no-check-deprecated`:  disable check for usage of deprecated packages
#'   * `no-check-description`:  disable DESCRIPTION file checks
#'   * `no-check-file-size`:  disable check for individual file size
#'   * `no-check-formatting`:  disable checks for file formatting
#'   * `no-check-function-len`:  disable check for function length
#'   * `no-check-install-self`: disable check for require or library of
#'     itself
#'   * `no-check-library-calls`: disable check usage of functions that
#'     install or update packages
#'   * `no-check-man-doc`:  disable checks for man page documentation
#'   * `no-check-news`:  disable checks for NEWS file
#'   * `no-check-pkg-size`:  disable check for package tarball size
#'   * `no-check-R-ver`:  disable check for valid R version
#'   * `no-check-remotes`: disable check for usage of remote packages other
#'     than those hosted on CRAN or Bioconductor
#'   * `no-check-skip-bioc-tests`: disable check for tests that skip when on
#'     bioc
#'   * `no-check-unit-tests`:  disable checks for unit tests
#'   * `no-check-version-num`:  disable check for valid version number
#'   * `no-check-vignettes`:  disable vignette checks
#'   * `quit-with-status`:  enable exit code option when performing check
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
#' * **error**: Items to address before the package can be accepted
#'
#' * **warning**: Strongly suggested items that may require attention
#'
#' * **note**: Items to consider, though not required, before acceptance
#'
#' @author Dan Tenenbaum, Lori Shepherd, and Marcel Ramos
#'
#' @references \url{https://contributions.bioconductor.org}
#' @seealso \link{BiocCheck-class}, \link{Message-class}
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

#' @importFrom BiocBaseUtils isScalarCharacter
BiocCheckRun <-
    function(package, checkDir, debug, ...)
{
    .BiocCheck$zero()
    package <- normalizePath(package)
    if (!file.exists(package) || !isScalarCharacter(package))
        .stop("Package directory or tarball provided does not exist.")
    # be careful here:
    if (identical(.Platform$OS.type, "windows"))
        package <- gsub("\\\\", "/", package)

    dots <- list(...)
    if (length(dots) == 1L && is.list(dots[[1]]))
        dots <- dots[[1]]               # command line args come as list

    oldwarn <- getOption("warn")
    oldwidth <- getOption("cli.width")
    on.exit({
        options(warn = oldwarn, cli.width = oldwidth)
    })
    options(warn = 1, cli.width = 80)

    .BiocPackage <- .BiocPackage$initialize(
        packageDir = package, checkDir = checkDir
    )

    ## consider merging these operations into one
    cli::cli_div(theme = list(.pkg = list(color = "orange")))
    cli::cli_rule("Installing {.pkg { .BiocPackage$packageName }}")
    package_install_dir <- installAndLoad(.BiocPackage)
    cli::cli_alert_success("Package installed successfully")
    libloc <- file.path(package_install_dir, "lib")
    isBBS <- Sys.getenv("IS_BIOC_BUILD_MACHINE")
    onBBS <- nzchar(isBBS) && identical(tolower(isBBS), "true")
    hasAdmin <- nzchar(Sys.getenv("BIOC_DEVEL_PASSWORD"))

    .BiocCheck$addMetadata(
        BiocPackage = .BiocPackage, installDir = package_install_dir
    )
    cli::cli_rule("{.pkg { .BiocPackage$packageName }} session metadata")
    .BiocCheck$show_meta()
    cli::cli_rule("Running BiocCheck on {.pkg { .BiocPackage$packageName }}")

    # BiocCheck checks --------------------------------------------------------
    if (is.null(dots[["no-check-deprecated"]])) {
        handleCheck("Checking for deprecated package usage...")
        checkDeprecatedPackages(.BiocPackage)
    }

    if (is.null(dots[["no-check-remotes"]])){
        handleCheck("Checking for remote package usage...")
        checkRemotesUsage(.BiocPackage)
    }

    handleCheck("Checking for 'LazyData: true' usage...")
    checkLazyDataUsage(.BiocPackage)

    if (is.null(dots[["no-check-version-num"]])){
        handleCheck("Checking version number...")
        if (!.BiocPackage$isSourceDir) {
            handleCheck("Checking for version number mismatch...")
            checkForVersionNumberMismatch(.BiocPackage)
        }

        if (!is.null(dots[["new-package"]])) {
            handleCheck("Checking new package version number...")
            checkNewPackageVersionNumber(.BiocPackage)
        } else {
            handleCheck("Checking version number validity...")
            checkVersionNumber(.BiocPackage)
        }
    }

    if (is.null(dots[["no-check-R-ver"]])) {
        handleCheck("Checking R version dependency...")
        checkRVersionDependency(.BiocPackage)
    }

    if (is.null(dots[["no-check-pkg-size"]])){
        handleCheck("Checking package size...")
        if (.BiocPackage$isTar){
            checkPackageSize(.BiocPackage, size=5)
        } else {
            handleMessage("Skipped... only checked on source tarball", indent=4)
        }
    }

    if (is.null(dots[["no-check-file-size"]])){
        handleCheck("Checking individual file sizes...")
        checkIndivFileSizes(.BiocPackage)
        checkDataFileSizes(.BiocPackage)
    }

    if (is.null(dots[["no-check-bioc-views"]]))
    {
        handleCheck("Checking biocViews...")
        result <- checkBiocViews(.BiocPackage)
        if(result)
        {
            cli::cli_alert_info(
                "Search 'biocViews' at https://contributions.bioconductor.org"
            )
        }
    }

    if (is.null(dots[["no-check-bbs"]])){
        handleCheck("Checking build system compatibility...")
        checkBBScompatibility(.BiocPackage)
    }

    if (is.null(dots[["no-check-description"]])) {
        checkDESCRIPTIONFile(.BiocPackage)
    }

    handleCheck("Checking .Rbuildignore...")
    checkRbuildignore(.BiocPackage)

    handleCheck("Checking for stray BiocCheck output folders...")
    checkBiocCheckOutputFolder(.BiocPackage)

    if (!.BiocPackage$isTar) {
        handleCheck("Checking for inst/doc folders...")
        checkInstDocFolder(.BiocPackage)
    }

    if (is.null(dots[["no-check-vignettes"]])) {
        handleCheck("Checking vignette directory...")
        checkVignetteDir(.BiocPackage)
        if ("build-output-file" %in% names(dots)) {
            handleCheck(
                "Checking whether vignette is built with 'R CMD build'..."
            )
            checkIsVignetteBuilt(dots[["build-output-file"]])
        }
    }

    if (is.null(dots[["no-check-library-calls"]])){
        handleCheck("Checking package installation calls in R code...")
        checkPkgInstallCalls(.BiocPackage)
    }

    package_dir <- .BiocPackage$sourceDir
    package_name <- .BiocPackage$packageName
    parsedCode <- parseFiles(.BiocPackage)

    if (is.null(dots[["no-check-install-self"]])){
        handleCheck(sprintf("Checking for library/require of %s...",
                            package_name))
        checkForLibraryRequire(.BiocPackage)
    }

    if (is.null(dots[["no-check-coding-practices"]])){
        handleCheck("Checking coding practice...")
        checkCodingPractice(.BiocPackage, parsedCode)
    }

    if (is.null(dots[["no-check-function-len"]])){
        handleCheck("Checking function lengths...")
        checkFunctionLengths(parsedCode, package_name)
    }

    if (is.null(dots[["no-check-man-doc"]])){
        handleCheck("Checking man page documentation...")
        checkManDocumentation(.BiocPackage, libloc)
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
        checkFormatting(.BiocPackage)
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
            checkForBiocDevelSubscription(.BiocPackage)
        } else {
            handleNote(
                "Cannot determine whether maintainer is subscribed to the ",
                "Bioc-Devel mailing list (requires admin credentials). ",
                "Subscribe here: ",
                "https://stat.ethz.ch/mailman/listinfo/bioc-devel"
            )
        }

        handleCheck("Checking for support site registration...")
        checkForSupportSiteRegistration(.BiocPackage)
    }

    cli::cli_rule(
        left = paste0("BiocCheck v", packageVersion("BiocCheck"), " results")
    )
    cli::cli_text(
        paste0(
            "{symbol$cross} { .BiocCheck$getNum('error') } ERRORS | ",
            "{symbol$warning} { .BiocCheck$getNum('warning') } WARNINGS | ",
            "{symbol$info} { .BiocCheck$getNum('note') } NOTES\n"
        )
    )
    cli::cli_alert_info(
        paste0(
            "\nSee the { .BiocPackage$packageName }.BiocCheck folder and run\n",
            "  browseVignettes(package = 'BiocCheck')\n",
            "  for details."
        )
    )

    .BiocCheck$report(debug, onBBS)

    if (isTRUE(dots[["quit-with-status"]])) {
        errcode <- as.integer(.BiocCheck$getNum("error") > 0)
        q("no", errcode)
    }

    return(.BiocCheck)

}
