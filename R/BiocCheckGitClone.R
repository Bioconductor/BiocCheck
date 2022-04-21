BiocCheckGitClone <- function(package=".", ...)
{
    .zeroCounters()
    package <- normalizePath(package)
    isTar <- grepl("\\.tar\\.gz$", package)
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
    .BiocCheck$show_meta()

    ## checks
    handleCheck("Checking valid files...")
    checkBadFiles(package)

    handleCheck("Checking for stray BiocCheck output folders...")
    checkBiocCheckOutputFolder(package_dir, package_name)

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
