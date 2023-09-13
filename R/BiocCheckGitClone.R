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
#' \item{error}{Items to address before the package can be accepted}
#'
#' \item{warning}{Strongly suggested items that may require attention}
#'
#' \item{note}{Items to consider, though not required, before acceptance}
#'
#' @author Lori Shepherd
#'
#' @references \url{https://contributions.bioconductor.org}
#' @seealso \link{BiocCheck-class}
#'
#' @md
#'
#' @examples
#'
#' packageDir <- system.file("testpackages", "testpkg0", package="BiocCheck")
#' BiocCheckGitClone(packageDir, `quit-with-status`=FALSE)
#'
#' @export BiocCheckGitClone
BiocCheckGitClone <- function(package=".", ...)
{
    .zeroCounters()
    package <- normalizePath(package)
    isTar <- grepl("\\.tar\\.[gx]z$", package)
    if (isTar)
        .stop("Run 'BiocCheckGitClone' on the Git-cloned package directory.")
    if (!dir.exists(package))
        .stop("Package directory does not exist")
    # be careful here:
    if (.Platform$OS.type=="windows")
        package <- gsub("\\\\", "/", package)

    dots <- list(...)
    if (length(dots) == 1L && is.list(dots[[1]]))
        dots <- dots[[1]]               # command line args come as list

    oldwarn <- getOption("warn")
    on.exit(options(warn=oldwarn))
    options(warn=1)

    package_dir <- .getPackageDir(package, isTar)
    package_name <- .getPackageName(package)
    pkgver <- .getPackageVersion(package_dir)
    bioccheckver <- as.character(packageVersion("BiocCheck"))
    biocver <- as.character(BiocManager::version())

    .BiocCheck$metadata <- list(
        BiocCheckVersion = bioccheckver,
        BiocVersion = biocver,
        Package = package_name, PackageVersion = pkgver,
        sourceDir = package_dir,
        platform = .Platform$OS.type, isTarBall = isTar
    )
    .BiocCheck$verbose <- TRUE
    .BiocCheck$show_meta()

    # BiocCheck checks --------------------------------------------------------
    handleCheck("Checking valid files...")
    checkBadFiles(package)

    handleCheck("Checking for stray BiocCheck output folders...")
    checkBiocCheckOutputFolder(package_dir, package_name)
    
    handleCheck("Checking for inst/doc folders...")
    checkInstDocFolder(package_dir, package_name)

    handleCheck("Checking DESCRIPTION...")
    checkDescription(package)

    handleCheck("Checking CITATION...")
    checkForCitationFile(package)

    # BiocCheck results -------------------------------------------------------
    message("\n\U2500 BiocCheck results \U2500\U2500")
    .msg(
        "%d ERRORS | %d WARNINGS | %d NOTES",
        .BiocCheck$getNum("error"),
        .BiocCheck$getNum("warning"),
        .BiocCheck$getNum("note")
    )
    message(
        "\nFor more details, run\n",
        "    browseVignettes(package = 'BiocCheck')"
    )

    if (isTRUE(dots[["quit-with-status"]])) {
        errcode <- as.integer(.BiocCheck$getNum("error") > 0)
        q("no", errcode)
    }

    return(.BiocCheck)
}
