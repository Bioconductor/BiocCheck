#' Check a package's adherence with the Bioconductor Package Guidelines
#'
#' Analyzes an R package for adherence with Bioconductor package guidelines and
#' best practices. The check outputs are categorized into ERROR, WARNING, and
#' NOTE. See the vignette for more details. `BiocCheck` is complementary
#' to `R CMD check`, which should always be run first.
#'
#' `BiocCheck()` reviews R packages for adherence with Bioconductor
#' package guidelines and best practices. See
#' <https://contributions.bioconductor.org> for the latest guidance for
#' writing Bioconductor software. Some rationale behind these best practices
#' can be seen in the vignette and pages in the `references` section. The
#' vignette also provides detailed explanations of all the checks performed by
#' `BiocCheck`.
#'
#' `BiocCheck` is called within R with `BiocCheck("package")`
#' where `package` points to the source directory or the `.tar.gz`
#' tarball that was created using `R CMD build`.
#'
#' **Note** that `BiocCheck` is complementary to `R CMD check`.
#' `R CMD check` should always be run first for best results.
#'
#' To skip installation of the package during the check, set the
#' `install` option to `FALSE` or `NULL`:
#' ```r
#' BiocCheck(package, install=FALSE)
#' ## OR
#' BiocCheck(package, install=NULL)
#' ```
#' To re-use an existing installation log file, set the `install` option
#' to the name of the installation log file.
#' For example, the following will put the `install_out.txt` log file in the
#' `<packageName>.BiocCheck` directory:
#' `BiocCheck(package, install="check:install_out.txt")`
#'
#' @section dot-options:
#' To use the dot-options, `BiocCheck` can be called with named arguments
#' corresponding to the options below. Typically, these options are set to
#' `TRUE` to disable specific checks, e.g.,
#' `BiocCheck(package, 'no-check-vignettes'=TRUE)`. Unless
#' otherwise stated, these options can be left unset (i.e., `NULL`) to enable
#' checks but `FALSE` can also be used to explicitly enable them. The available
#' options are:
#'
#'   * `build-output-file`: file containing `R CMD build` output, for
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
#'   * `no-check-namespace`:  disable NAMESPACE file checks
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
#'   * `install`: if `FALSE`, the package is not installed; otherwise, if not
#'     specified, the package is installed by default. Optionally, a
#'     `check:<file>` key-value pair is provided to identify the name of the
#'     installation output file which will be copied to the
#'     `<packageName>.BiocCheck` directory.
#'   * `libloc`: when `install` is specified, the library location where the
#'     package is installed. By default, this is `.libPaths()[1]`.
#'
#' @param package `character(1)` or `.BiocPackage` Either a path to the R
#'   package source directory or its tarball (`.tar.gz`) or an object of class
#'   `BiocPackage`. The `BiocCheck` function is designed to run from within the
#'   base source package directory by default (`getwd()`).
#'
#' @param checkDir `character(1)` The directory where the `BiocCheck` output
#'   directory `'<packageName>.BiocCheck'` should be placed. By default, it will
#'   be created in the same directory as the package directory i.e.,
#'   `dirname(package)`.
#'
#' @param debug `logical(1)` Whether to append the names of functions that
#'   correspond to each condition raised by `BiocCheck` in the written log
#'   (i.e., in the `'<packageName>.BiocCheck'` folder). This option is only
#'   relevant to developers and contributors to `BiocCheck`.
#'
#' @param callr `logical(1)` Whether to use the `callr` package to run
#'   `BiocCheck` in an isolated R session to prevent namespace collisions.
#'
#' @param \dots See the `dot-options` details section for available options.
#'
#' @return `BiocCheck()` is chiefly called for the side effect of the check
#'   reporting. The function also creates a `<packageName>.BiocCheck` folder
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
#' @references <https://contributions.bioconductor.org>
#' @seealso [BiocCheck-class], [Message-class]
#'
#' @usage
#' BiocCheck(
#'    package = getwd(),
#'    checkDir = dirname(package),
#'    debug = FALSE,
#'    callr = FALSE,
#'    ...
#' )
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
    if (is.character(package)) {
        packagePath <- normalizePath(package)
        package <- .BiocPackage$initialize(
            packageDir = packagePath,
            checkDir = dirname(packagePath)
        )
    } else if (!is(package, "BiocPackage")) {
        .stop(
            "Invalid 'package' argument: must be a character path or ",
            "an object of class 'BiocPackage'."
        )
    }

    .BiocCheck$zero()

    if (callr) {
        callr::r(
            function(...) {
                BiocCheck:::BiocCheckRun(...)
            },
            args = list(
                package = package,
                debug = debug,
                ...
            ),
            cmdargs = c("--no-echo", "--no-save", "--no-restore"),
            show = TRUE
        )
    } else {
        BiocCheckRun(
            package = package,
            debug = debug,
            ...
        )
    }
}

#' @importFrom BiocBaseUtils isScalarCharacter
BiocCheckRun <-
    function(package, debug, ...)
{
    if (!is(package, "BiocPackage"))
        .BiocPackage <- .BiocPackage$initialize(
            packageDir = package,
            checkDir = dirname(package)
        )
    else
        .BiocPackage <- package

    cli::cli_div(
        theme = list(
            .pkg = list(color = "green", `font-weight` = "bold"),
            .error = list(color = "red"),
            .warning = list(color = "orange"),
            .note = list(color = "blue"),
            .version = list(color = "yellow", `font-style` = "italic")
        )
    )
    cli::cli_rule("Installing {.pkg { .BiocPackage$packageName }}")

    dots <- list(...)
    if (length(dots) == 1L && is.list(dots[[1]]))
        dots <- dots[[1]]               # command line args come as list

    install_param <- dots[["install"]]
    should_install <- is.null(install_param) || isTRUE(install_param)

    if (should_install) {
        .BiocPackage$inst_setup()
        cli::cli_alert_success("Package installed successfully")
        dots[["libloc"]] <- .BiocPackage$installDir
    } else {
        if (is.character(install_param)) {
            split_log <- strsplit(install_param, ":")[[1L]]
            stopifnot(
                identical(
                    "check",
                    utils::head(split_log, n = 1L)
                )
            )
            inst_log <- utils::tail(split_log, n = 1L)
            on.exit({
                if (file.exists(inst_log))
                    file.copy(
                        from = inst_log,
                        to = file.path(
                            .BiocPackage$BiocCheckDir,
                            basename(inst_log)
                        )
                    )
            }, add = TRUE)
        }
        dots[["libloc"]] <- dots[["libloc"]] %||% .libPaths()[1L]
        .BiocPackage$installDir <- .libPaths()[1L]
        .BiocPackage$isInstalled <-  system.file(
            package = .BiocPackage$packageName,
            lib.loc = .libPaths()[1L]
        ) |>
            nzchar()
    }

    oldwarn <- getOption("warn")
    oldwidth <- getOption("cli.width")
    on.exit({
        options(warn = oldwarn, cli.width = oldwidth)
    })
    options(warn = 1, cli.width = 80)

    .BiocCheck$addMetadata(
        BiocPackage = .BiocPackage,
        installDir = .BiocPackage$installDir
    )
    cli::cli_rule("{.pkg { .BiocPackage$packageName }} session metadata")
    .BiocCheck$show_meta()

    if (.BiocPackage$isGitClone)
        .BiocCheck <- BiocCheckGitClone(.BiocPackage, dots)

    .BiocCheck <- BiocCheckSource(.BiocPackage, debug, dots)

    BiocCheckResults(.BiocCheck, dots)
}

BiocCheckSource <- function(.BiocPackage, debug, dots) {
    isBBS <- Sys.getenv("IS_BIOC_BUILD_MACHINE")
    onBBS <- nzchar(isBBS) && identical(tolower(isBBS), "true")
    hasAdmin <- nzchar(Sys.getenv("BIOC_DEVEL_PASSWORD"))

    cli::cli_rule(
        "Running {.pkg BiocCheck} on {.pkg { .BiocPackage$packageName }}"
    )
    # BiocCheck checks --------------------------------------------------------
    if (.isNULLorFALSE(dots[["no-check-deprecated"]])) {
        handleCheck("Checking for deprecated package usage...")
        checkDeprecatedPackages(.BiocPackage)
    }

    if (.isNULLorFALSE(dots[["no-check-remotes"]])) {
        handleCheck("Checking for remote package usage...")
        checkRemotesUsage(.BiocPackage)
    }

    handleCheck("Checking for 'LazyData: true' usage...")
    checkLazyDataUsage(.BiocPackage)

    if (.isNULLorFALSE(dots[["no-check-version-num"]])) {
        handleCheck("Checking version number...")
        checkForVersionNumberMismatch(.BiocPackage)

        if (isTRUE(dots[["new-package"]])) {
            handleCheck("Checking new package version number...")
            checkNewPackageVersionNumber(.BiocPackage)
        } else if (.isNULLorFALSE(dots[["new-package"]])) {
            handleCheck("Checking version number validity...")
            checkVersionNumber(.BiocPackage)
        }
    }

    if (.isNULLorFALSE(dots[["no-check-R-ver"]])) {
        handleCheck("Checking R version dependency...")
        checkRVersionDependency(.BiocPackage)
    }

    if (.isNULLorFALSE(dots[["no-check-pkg-size"]])) {
        handleCheck("Checking package size...")
        if (.BiocPackage$isTar) {
            checkPackageSize(.BiocPackage)
        } else {
            handleMessage(
                "Skipped... only checked on source tarball",
                indent = 4
            )
        }
    }

    if (.isNULLorFALSE(dots[["no-check-file-size"]])) {
        handleCheck("Checking individual file sizes...")
        checkIndivFileSizes(.BiocPackage)
        checkDataFileSizes(.BiocPackage)
    }

    if (.isNULLorFALSE(dots[["no-check-bioc-views"]])) {
        handleCheck("Checking biocViews...")
        result <- checkBiocViews(.BiocPackage)
        if (result) {
            cli::cli_alert_info(
                "Search 'biocViews' at https://contributions.bioconductor.org"
            )
        }
    }

    if (.isNULLorFALSE(dots[["no-check-bbs"]])) {
        handleCheck("Checking build system compatibility...")
        checkBBScompatibility(.BiocPackage)
    }

    if (.isNULLorFALSE(dots[["no-check-description"]])) {
        checkDESCRIPTIONFile(.BiocPackage)
    }

    handleCheck("Checking CITATION...")
    checkForCitationFile(.BiocPackage)

    if (.isNULLorFALSE(dots[["no-check-namespace"]]))
        checkNAMESPACE(.BiocPackage)

    handleCheck("Checking .Rbuildignore...")
    checkRbuildignore(.BiocPackage)

    handleCheck("Checking for stray BiocCheck output folders...")
    checkBiocCheckOutputFolder(.BiocPackage)

    if (.isNULLorFALSE(dots[["no-check-vignettes"]])) {
        handleCheck("Checking vignette directory...")
        checkVignetteDir(.BiocPackage)
        if ("build-output-file" %in% names(dots)) {
            handleCheck(
                "Checking whether vignette is built with 'R CMD build'..."
            )
            checkIsVignetteBuilt(dots[["build-output-file"]])
        }
    }

    if (.isNULLorFALSE(dots[["no-check-library-calls"]])) {
        handleCheck("Checking package installation calls in R code...")
        checkPkgInstallCalls(.BiocPackage)
    }

    package_dir <- .BiocPackage$sourceDir
    package_name <- .BiocPackage$packageName
    parsedCode <- parseFiles(.BiocPackage)

    if (.isNULLorFALSE(dots[["no-check-install-self"]])) {
        handleCheck(sprintf(
            "Checking for library/require of %s...",
            package_name
        ))
        checkForLibraryRequire(.BiocPackage)
    }

    if (.isNULLorFALSE(dots[["no-check-coding-practices"]])) {
        handleCheck("Checking coding practice...")
        checkCodingPractice(.BiocPackage, parsedCode)
    }

    if (.isNULLorFALSE(dots[["no-check-function-len"]])) {
        handleCheck("Checking function lengths...")
        checkFunctionLengths(parsedCode, package_name)
    }

    if (.isNULLorFALSE(dots[["no-check-man-doc"]])) {
        handleCheck("Checking man page documentation...")
        checkManDocumentation(.BiocPackage, dots[["libloc"]])
    }

    if (.isNULLorFALSE(dots[["no-check-news"]])) {
        handleCheck("Checking package NEWS...")
        checkNEWS(package_dir)
    }

    if (.isNULLorFALSE(dots[["no-check-unit-tests"]])) {
        handleCheck("Checking unit tests...")
        checkUnitTests(package_dir)
    }

    if (.isNULLorFALSE(dots[["no-check-skip-bioc-tests"]])) {
        handleCheck("Checking skip_on_bioc() in tests...")
        checkSkipOnBioc(package_dir)
    }

    if (.isNULLorFALSE(dots[["no-check-formatting"]])) {
        handleCheck(
            "Checking formatting of DESCRIPTION, NAMESPACE, ",
            "man pages, R source, and vignette source..."
        )
        checkFormatting(.BiocPackage)
    }

    if (.isNULLorFALSE(dots[["no-check-CRAN"]])) {
        handleCheck("Checking if package already exists in CRAN...")
        checkIsPackageNameAlreadyInUse(package_name, "CRAN")
    }

    if (isTRUE(dots[["new-package"]])) {
        handleCheck(
            "Checking if new package already exists in Bioconductor..."
        )
        checkIsPackageNameAlreadyInUse(package_name, "BioCsoft")
        checkIsPackageNameAlreadyInUse(package_name, "BioCann")
        checkIsPackageNameAlreadyInUse(package_name, "BioCexp")
        checkIsPackageNameAlreadyInUse(package_name, "BioCworkflows")
        # TODO: add VIEWS files for books
        # checkIsPackageNameAlreadyInUse(package_name, "BioCbooks")
    }

    if (.isNULLorFALSE(dots[["no-check-bioc-help"]])) {
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

    .BiocCheck$report(debug, onBBS)

    return(.BiocCheck)
}

BiocCheckResults <- function(.BiocCheck, dots) {

    # BiocCheck results -------------------------------------------------------
    cli::cli_rule(
        left = paste(
            "{.pkg BiocCheck}",
            "{.version v{ packageVersion('BiocCheck') }}",
            "results"
        )
    )
    cli::cli_text(
        paste(
            "{.error {symbol$cross}}",
                "{ .BiocCheck$getNum('error') }",
                "{.error ERRORS } |",
            "{.warning {symbol$warning}}",
                "{ .BiocCheck$getNum('warning') }",
                "{.warning WARNINGS } |",
            "{.note {symbol$info}}",
                "{ .BiocCheck$getNum('note') }",
                "{.note NOTES }\n"
        )
    )
    cli::cli_alert_info(
        paste(
            "\nSee the { .BiocPackage$packageName }.BiocCheck folder and run\n",
            " {.code browseVignettes(package = 'BiocCheck')}\n",
            " for details."
        )
    )

    if (isTRUE(dots[["quit-with-status"]])) {
        errcode <- as.integer(.BiocCheck$getNum("error") > 0)
        q("no", errcode)
    }

    return(.BiocCheck)
}
