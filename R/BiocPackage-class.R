# BiocPackage-class -------------------------------------------------------

#' @name BiocPackage-class
#'
#' @docType class
#'
#' @aliases BiocPackage
#'
#' @title A class for representing files in a Bioconductor package
#'
#' @description The BiocPackage class is used to represent a Bioconductor
#'   package. It is used by BiocCheck to store information about the package
#'   being checked. The class has several methods to identify the type of
#'   package, check for common issues, and store metadata about the package.
#'
#' @field isValid `logical` indicating whether the package's `DESCRIPTION` file
#'   was able to be read without any errors
#'
#' @field isTar `logical` indicating whether the package is a tarball
#'
#' @field isSourceDir `logical` indicating whether the package being checked is
#'   from a source directory
#'
#' @field isInfrastructure `logical` indicating whether the package is an
#'   Bioconductor infrastructure package based on the `biocViews` field
#'
#' @field usesRoxygen `logical` indicating whether the package uses `roxygen2`
#'   documentation
#'
#' @field usesRdpack `logical` indicating whether the package uses `Rdpack`
#'   package
#'
#' @field DESCRIPTION `matrix` containing the DCF contents of the `DESCRIPTION`
#'   file
#'
#' @field dependencies `character` vector of package dependencies
#'
#' @field readError `character` error message if the `DESCRIPTION` file could
#'   not be read
#'
#' @field packageVersion `character` version of the package
#'
#' @field packageType `character` indicating the type of package based on the
#'   `biocViews` field; can be `NA_character_` there are invalid `biocViews`
#'   terms
#'
#' @field sourceDir `character` path to the source directory
#'
#' @field vignettesDir `character` path to the vignettes directory
#'
#' @field RSources `character` vector of R source files
#'
#' @field VigSources `character` vector of vignette source files
#'
#' @field manSources `character` vector of Rd source files
#'
#' @field BiocCheckDir `character` path to the directory where the package
#'   BiocCheck logs are written
#'
#' @field packageName `character` name of the package
#'
#' @field tarFilename `character` filename of the tarball
#'
#' @field metadata `list` containing metadata about the package
#'
#' @return An object of class `BiocPackage`
#'
#' @keywords internal
#'
#' @section methods:
#'
#' * `initialize`: Initialize a `BiocPackage` object
#' * `getPackageDir`: Get the package directory
#' * `getRSources`: Get the R source files
#' * `getVigSources`: Get the vignette source files
#' * `getManSources`: Get the Rd source files
#' * `getBiocCheckDir`: Get the directory where the BiocCheck logs are written
#' * `getBiocViews`: Get the `biocViews` field from the `DESCRIPTION` file
#' * `getPackageType`: Get the package type based on the `biocViews` field
#' * `readDESCRIPTION`: Read the `DESCRIPTION` file
#' * `getVigBuilder`: Get the vignette builder
#' * `getAllDependencies`: Get all dependencies from the `DESCRIPTION` file
#' * `findInfrastructure`: Is the package an infrastructure package?
#' * `findRoxygen`: Does the package use `roxygen2`?
#' * `getPackageVersion`: Get the package version
#' * `untarTarball`: Untar the source tarball
#'
#' @seealso [BiocCheck-class], [Message-class]
#' @examples
#'
#' # Create a BiocPackage object
#' packageDirectory <- "path/to/package"
#' if (dir.exists(packageDirectory))
#'     .bioctest <- .BiocPackage$initialize(packageDirectory)
#'
#' .bioctest <- BiocCheck:::.BiocPackage
#'
#' .bioctest$DESCRIPTION
#'
#' @importFrom utils untar
#' @exportClass BiocPackage
.BiocPackage <- setRefClass(
    "BiocPackage",
    fields = list(
        isValid = "logical",
        isTar = "logical",
        isSourceDir = "logical",
        isGitClone = "logical",
        isInfrastructure = "logical",
        isInstalled = "logical",
        installDir = "character",
        usesRoxygen = "logical",
        usesRdpack = "logical",
        DESCRIPTION = "matrix",
        dependencies = "character",
        readError = "character",
        packageVersion = "character",
        packageType = "character",
        sourceDir = "character",
        vignettesDir = "character",
        RSources = "character",
        VigSources = "character",
        VigBuilder = "character",
        manSources = "character",
        BiocCheckDir = "character",
        packageName = "character",
        tarFilename = "character",
        metadata = "list"
    ),
    methods = list(
        initialize = function(packageDir, checkDir = dirname(packageDir), ...) {
            if (!missing(packageDir)) {
                packageDir <- normalizePath(packageDir)
                .self[["isTar"]] <- grepl("\\.tar\\.[gx]z$", packageDir)
                .self[["isSourceDir"]] <-
                    !.self$isTar && .isSourceDir(packageDir)
                .self[["isGitClone"]] <- .isGitClone(packageDir)
                .self[["isInstalled"]] <- FALSE
                .self$getPackageDir(packageDir)
                .self$getRSources()
                .self$getVigSources()
                .self$getManSources()
                .self$readDESCRIPTION()
                .self$getVigBuilder()
                .self$getAllDependencies()
                .self$getBiocCheckDir(checkDir)
                .self$getPackageType()
                .self$findInfrastructure()
                .self$findRoxygen()
            }
            callSuper(...)
        },
        install = function(install_dir = tempfile()) {
            pkgpath <- .self[["sourceDir"]]
            pkgname <- .self[["packageName"]]

            if (!dir.exists(install_dir))
                dir.create(install_dir)

            dir.create(libdir <- file.path(install_dir, "lib"))
            file.create(stderr <- file.path(install_dir, "install.stderr"))

            r_libs_user <-
                paste(c(libdir, .libPaths()), collapse = .Platform$path.sep)

            res <- callr::rcmd_safe(
                "INSTALL",
                c(
                    "--use-vanilla",
                    paste0("--library=", libdir),
                    pkgpath
                ),
                env = c(callr::rcmd_safe_env(), R_LIBS_USER = r_libs_user)
            )

            if (!identical(res[["status"]], 0L))
                warning(pkgpath, " must be installable and loadable.")

            .self[["isInstalled"]] <- TRUE
            .self[["installDir"]] <- libdir
        },
        getPackageDir = function(packageDir) {
            if (.self[["isTar"]]) {
                .self[["tarFilename"]] <- packageDir
                packageDir <- .self$untarTarball(packageDir)
            } else if (!file.info(packageDir)[["isdir"]]) {
                .stop(
                    "'%s' is not a directory or package source tarball.",
                    packageDir
                )
            }
            if (identical(.Platform$OS.type, "windows"))
                packageDir <- gsub("\\\\", "/", packageDir)
            .self[["sourceDir"]] <- packageDir
        },
        getRSources = function() {
            Rdir <- file.path(.self[["sourceDir"]], "R")
            rfiles <-
                list.files(Rdir, pattern = "\\.[Rr]$", full.names = TRUE)
            if (length(rfiles))
                names(rfiles) <- paste0("R/", basename(rfiles))
            .self[["RSources"]] <- rfiles
        },
        getVigSources = function() {
            .self[["vignettesDir"]] <-
                file.path(.self[["sourceDir"]], "vignettes")
            vigfiles <- list.files(
                .self[["vignettesDir"]],
                pattern="\\.Rmd$|\\.qmd$|\\.Rnw$|\\.Rrst$|\\.Rhtml$|\\.Rtex$",
                ignore.case=TRUE, full.names=TRUE
            )
            if (length(vigfiles))
                names(vigfiles) <- paste0("vignettes/", basename(vigfiles))
            .self[["VigSources"]] <- vigfiles
        },
        getManSources = function() {
            mandir <- file.path(.self[["sourceDir"]], "man")
            manfiles <- list.files(
                mandir, pattern="\\.Rd$", full.names = TRUE, ignore.case = TRUE
            )
            if (length(manfiles))
                names(manfiles) <- paste0("man/", basename(manfiles))
            .self[["manSources"]] <- manfiles
        },
        getBiocCheckDir = function(checkDir) {
            checkDir <- normalizePath(checkDir, winslash = "/")
            .self[["BiocCheckDir"]] <- file.path(
                checkDir, paste(.self[["packageName"]], "BiocCheck", sep = ".")
            )
        },
        getBiocViews = function() {
            views <- ""
            dcf <- .self[["DESCRIPTION"]]
            if ("biocViews" %in% colnames(dcf))
                views <- strsplit(dcf[, "biocViews"], "\\s*,\\s*")[[1]]
            views
        },
        getPackageType = function() {
            views <- .self$getBiocViews()
            if (identical(length(views), 1L) && !nzchar(views))
                type <- NA
            biocViewsVocab <- .load_data("biocViewsVocab", "biocViews")
            if (any(!views %in% nodes(biocViewsVocab)))
                type <- NA
            parents <- vapply(views, getParent, character(1L), biocViewsVocab)
            u <- unique(parents)
            type <- if (identical(length(u), 1L)) u else NA_character_
            .self[["packageType"]] <- type
        },
        readDESCRIPTION = function() {
            desc <- file.path(.self[["sourceDir"]], "DESCRIPTION")
            dcf <- try({ read.dcf(desc) }, silent=TRUE)
            .self[["isValid"]] <- !inherits(dcf, "try-error")
            if (.self[["isValid"]]) {
                .self[["DESCRIPTION"]] <- dcf
                .self[["packageName"]] <- as.character(dcf[, "Package"])
                .self[["packageVersion"]] <- as.character(dcf[, "Version"])
            } else {
                .self[["readError"]] <- conditionMessage(attr(dcf, "condition"))
            }
        },
        getVigBuilder = function() {
            dcf <- .self[["DESCRIPTION"]]
            if (.self[["isValid"]] && "VignetteBuilder" %in% colnames(dcf))
                .self[["VigBuilder"]] <- unlist(
                    strsplit(dcf[, "VignetteBuilder"], ",\\s*\\n?"),
                    use.names = FALSE
                )
        },
        getAllDependencies = function() {
            dcf <- .self[["DESCRIPTION"]]
            fields <-
                c("Depends", "Imports", "Suggests", "Enhances", "LinkingTo")
            afields <- intersect(fields, colnames(dcf))
            out <- lapply(afields, function(field) {
                cleanupDependency(dcf[, field])
            })
            .self[["dependencies"]] <- as.character(unlist(out))
            .self[["usesRdpack"]] <- "Rdpack" %in% out
        },
        findInfrastructure = function() {
            dcf <- .self[["DESCRIPTION"]]
            if (!nrow(dcf) || !"biocViews" %in% colnames(dcf))
                return()
            biocViews <- dcf[, "biocViews"]
            views <- strsplit(gsub("\\s", "", biocViews), ",")[[1]]
            .self[["isInfrastructure"]] <- "Infrastructure" %in% views
        },
        findRoxygen = function() {
            .self[["usesRoxygen"]] <-
                "RoxygenNote" %in% colnames(.self[["DESCRIPTION"]])
        },
        getPackageVersion = function() {
            .self[["packageVersion"]]
        },
        untarTarball = function(pkgTarball, tempDir = tempfile()) {
            if (!dir.exists(tempDir))
                dir.create(tempDir)
            suppressMessages({ untar(pkgTarball, exdir = tempDir) })
            .self[["packageName"]] <-
                list.dirs(tempDir, recursive = FALSE, full.names = FALSE)
            file.path(tempDir, .self[["packageName"]])
        }
    )
)

#' @export
.BiocPackage <- .BiocPackage()
