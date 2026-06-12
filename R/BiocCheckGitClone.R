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
#' <https://contributions.bioconductor.org> for the latest guidance for
#' writing Bioconductor software. This function should only be run on a source
#' directory and not on a tarball.
#'
#' `BiocCheckGitClone` is called within R with, as
#' `BiocCheckGitClone("package")` where `package` is the source directory
#' containing the `R` package.
#'
#' @param package A directory containing an R source package. Not a package tar
#' ball.
#'
#' @param \dots Additional arguments, not currently used.
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
#' @references <https://contributions.bioconductor.org>
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
    if (!is(package, "BiocPackage"))
        .BiocPackage <- .BiocPackage$initialize(
            packageDir = package,
            checkDir = dirname(package)
        )
    else
        .BiocPackage <- package

    cli::cli_rule(
        "Running Git clone checks on {.pkg { .BiocPackage$packageName }}"
    )
    # BiocCheck checks --------------------------------------------------------
    handleCheck("Checking valid files...")
    checkBadFiles(.BiocPackage)

    handleCheck("Checking for inst/doc folders...")
    checkInstDocFolder(.BiocPackage)

    handleCheck("Checking if DESCRIPTION is well formatted...")
    checkDESCRIPTION(.BiocPackage)

    validMaintainer(.BiocPackage)

    handleCheck("Checking CITATION...")
    checkForCitationFile(.BiocPackage)

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
