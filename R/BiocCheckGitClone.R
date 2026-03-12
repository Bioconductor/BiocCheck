.HIDDEN_FILE_EXTS <- c(
    ".renviron", ".rprofile", ".rproj", ".rproj.user", ".rhistory",
    ".rapp.history", ".o", ".sl", ".so", ".dylib", ".a", ".dll", ".def",
    ".ds_store", "unsrturl.bst", ".log", ".aux", ".backups", ".cproject",
    ".directory", ".dropbox", ".exrc", ".gdb.history", ".gitattributes",
    ".gitmodules", ".hgtags", ".project", ".seed", ".settings",
    ".tm_properties", ".rdata"
)

# taken from
# https://github.com/wch/r-source/blob/trunk/src/library/tools/R/build.R#L462
# https://github.com/wch/r-source/blob/trunk/src/library/tools/R/check.R#L4025
hidden_file_data <- data.frame(
    file_ext = .HIDDEN_FILE_EXTS,
    hidden_only = c(TRUE, TRUE, FALSE, TRUE, TRUE,
        TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
        TRUE, TRUE, FALSE, FALSE, FALSE, FALSE,
        FALSE, FALSE, FALSE, FALSE, TRUE,
        TRUE, FALSE, TRUE, FALSE, FALSE,
        FALSE, TRUE)
)

#' Checks specific to a Git clone of a package repository
#'
#' Analyzes an R package for adherence with Bioconductor package guidelines and
#' best practices. The check outputs are categorized into ERROR, WARNING, and
#' NOTE. This function is typically used in the Bioconductor Build System (BBS)
#' and not intended for general use.
#'
#' `BiocCheckGitClone()` reviews R packages for adherence with
#' Bioconductor package guidelines and best practices. See
#' \url{https://contributions.bioconductor.org} for the latest guidance for
#' writing Bioconductor software. This function should only be run on a source
#' directory and not on a tarball.
#'
#' `BiocCheckGitClone` is called within R with, as \preformatted{
#' BiocCheckGitClone(<package>) } where `package` is the source directory
#' containing the `R` package.
#'
#' @param package A directory containing an R source package. Not a package tar
#' ball.
#'
#' @param \dots Currently, only `quit-with-status` is available.  See
#' `BiocCheck`
#'
#'
#' @return `BiocCheckGitClone()` is chiefly called for the side effect of the
#'   check reporting. The function returns a `BiocCheck` reference class with
#'   three main list elements:
#'
#' * error: Items to address before the package can be accepted
#'
#' * warning: Strongly suggested items that may require attention
#'
#' * note: Items to consider, though not required, before acceptance
#'
#' @author Lori Shepherd
#'
#' @references \url{https://contributions.bioconductor.org}
#' @seealso [BiocCheck-class]
#'
#' @examples
#'
#' packageDir <- system.file("testpackages", "testpkg0", package="BiocCheck")
#' BiocCheckGitClone(packageDir, `quit-with-status`=FALSE)
#'
#' @export BiocCheckGitClone
BiocCheckGitClone <- function(package=".", ...)
{
    .BiocCheck$zero()
    package <- normalizePath(package)
    if (!dir.exists(package))
        .stop("Package directory does not exist")
    .BiocPackage <- .BiocPackage$initialize(package)
    if (.BiocPackage$isTar)
        .stop("Run 'BiocCheckGitClone' on the Git-cloned package directory.")
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

    .BiocCheck$addMetadata(.BiocPackage)
    .BiocCheck$show_meta()

    # BiocCheck checks --------------------------------------------------------
    handleCheck("Checking valid files...")
    checkBadFiles(.BiocPackage)

    handleCheck("Checking individual file sizes...")
    checkIndivFileSizes(.BiocPackage)
    checkDataFileSizes(.BiocPackage)

    handleCheck("Checking for stray BiocCheck output folders...")
    checkBiocCheckOutputFolder(.BiocPackage)

    handleCheck("Checking for inst/doc folders...")
    checkInstDocFolder(.BiocPackage)

    checkDESCRIPTION(.BiocPackage)
    checkNAMESPACE(.BiocPackage)
    validMaintainer(.BiocPackage)

    handleCheck("Checking CITATION...")
    checkForCitationFile(.BiocPackage)

    # BiocCheck results -------------------------------------------------------
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
        "\nFor more details, run\n  browseVignettes(package = 'BiocCheck')"
    )

    if (isTRUE(dots[["quit-with-status"]])) {
        errcode <- as.integer(.BiocCheck$getNum("error") > 0)
        q("no", errcode)
    }

    return(.BiocCheck)
}

# Checks for BiocCheckGitClone --------------------------------------------

checkBadFiles <- function(.BiocPackage) {
    package_dir <- .BiocPackage$sourceDir
    swith <- ifelse(hidden_file_data[["hidden_only"]], .Platform$file.sep, "")
    ext_expr <- paste0(
        swith, "\\", hidden_file_data[["file_ext"]], "$", collapse = "|"
    )

    fls <- dir(package_dir, recursive=TRUE, all.files=TRUE)
    flist <- split(fls, startsWith(fls, "inst"))
    warns <- grep(ext_expr, ignore.case = TRUE, flist[['TRUE']], value = TRUE)
    errs <- grep(ext_expr, ignore.case = TRUE, flist[['FALSE']], value = TRUE)

    ## use gitignore to filter out false positives
    gitignore <- file.path(package_dir, ".gitignore")
    if (file.exists(gitignore)) {
        gitignore <- readLines(gitignore)
        filter_expr <- paste0(utils::glob2rx(gitignore), collapse = "|")
        ignored <- grep(
            filter_expr, ignore.case = TRUE, flist[["FALSE"]], value = TRUE
        )
        errs <- errs[!errs %in% ignored]
    }
    if (length(warns)) {
        handleWarning(
            "System files in '/inst' should not be Git tracked.",
            messages = warns
        )
    }

    if (length(errs)) {
        handleError(
            "System files found that should not be Git tracked.",
            messages = errs
        )
    }
}

checkForCitationFile <- function(.BiocPackage) {
    package_dir <- .BiocPackage$sourceDir
    citfile_location <- file.path(package_dir, "inst", "CITATION")
    if (file.exists(citfile_location)) {
        handleCheck(
            "Checking that provided CITATION file is correctly formatted..."
        )
        cit <- try(readCitationFile(citfile_location), silent = TRUE)
        if (is(cit, "try-error"))
            handleWarning(
                "Unable to read CITATION file with 'utils::readCitationFile()'"
            )
        else if (is.null(cit$doi))
            handleWarning(
                "The 'doi' argument is missing or empty in the CITATION's ",
                "'bibentry()'. Only include a CITATION file if there is a ",
                "preprint or publication associated with this Bioconductor ",
                "package."
            )
    } else {
        handleNote(
            "(Optional) CITATION file not found. Only include a CITATION ",
            "file if there is a preprint or publication for this Bioconductor ",
            "package. Note that Bioconductor packages are not required to ",
            "have a CITATION file but it is useful both for users and for ",
            "tracking Bioconductor project-wide metrics. When including a ",
            "CITATION file, add the publication using the  'doi' argument ",
            "of 'bibentry()'."
        )
    }
}
