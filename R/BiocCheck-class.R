# BiocCheck-class ---------------------------------------------------------

#' @name BiocCheck-class
#'
#' @docType class
#'
#' @title An internal class for composing BiocCheck reports
#'
#' @description The `BiocCheck` class provides a framework for reporting checks
#'   based on Bioconductor guidelines. The class has several methods for working
#'   with the provided checks that handle and display messages and the display
#'   of the metadata. These methods also record the output of the `BiocCheck()`
#'   report in both plain text and JSON formats.
#'
#'   **Note** that currently, multiple `BiocCheck` runs will interfere with
#'   each other given that they are implemented via a reference class semantic.
#'   When running multiple checks in the same session, you can separate these
#'   instances by running them in separate processes (e.g., via `BiocParallel`).
#'
#' @details The metadata includes a number of standard fields to allow easier
#'   troubleshooting and display of potentially relevant information. Currently,
#'   the fields included are:
#'
#'   * BiocCheckVersion:  The version of the BiocCheck package
#'   * BiocVersion:  The version of Bioconductor
#'   * Package:  The name of the package in check
#'   * PackageVersion:  The version of the package in check
#'   * sourceDir:  The directory of the package source or tarball in check
#'   * installDir: The directory where the package is installed for
#'     testing, a temporary location by default
#'   * BiocCheckDir: The directory where the `<package>.BiocCheck` folder
#'     is saved. Usually the same folder as the package in check
#'   * platform:  The platform/OS where the check is taking place
#'   * isTarBall: Whether the package in check is a source directory or a
#'     tarball
#'
#' @field log `list()` A running list of all conditions raised (i.e., notes,
#'   warnings, errors)
#'
#' @field check `character(1)` The title of the last check used for logging
#'   purposes.
#'
#' @field error,warning,note `list()` Finer extraction of each condition type
#'
#' @field metadata `list()` A list of additional information relevant to the
#'   package and its state. See details.
#'
#' @param ... `character()` A vector that makes up the `BiocCheck` exception
#'   message (e.g., 'Vignette must be built by R CMD build'). The character
#'   vector is handled with `paste0` and made into a list and appended with
#'   `help_text` and `messages`.
#'
#' @param help_text `character(1)` Additional text prompting a list of files
#'   (e.g,. "Found in files:")
#'
#' @param condition `character(1)` One of the three conditions handled: `error`,
#'   `warning`, or `note`
#'
#' @param messages `character()` Often a vector of file names where the check
#'   was triggered.
#'
#'
#' @param debug `logical(1)` Whether to append the name of the originating check
#'   name into for trace-ability
#'
#' @param checkName `character(1)` The title of the current group of checks. It
#'   can be set with `handleCheck`, e.g.,
#'   `handleCheck("Checking for version number mismatch...")`. Internally, it
#''  is saved with `setCheck` and obtained with `getLastCheck`.
#'
#' @param isOnBBS `logical(1)` Indicates whether the checks are being run on the
#'   Bioconductor Build System (BBS). This is helpful for avoiding the creation
#'   of folders in the BBS.
#'
#' @param file `character(1)` A path to a JSON file for writing or reading as
#'   created by `toJSON` and `fromJSON` `BiocCheck` methods.
#'
#' @importFrom BiocBaseUtils checkInstalled
#' @importFrom utils tail
#'
#' @section methods:
#'   * `add`: Include a condition to the `BiocCheck` report
#'   * `addMetadata`: Add metadata to the `BiocCheck` object from a
#'     `BiocPackage` object
#'   * `getLastCheck`: Obtain the name of the last check run
#'   * `setCheck`: Create a new element in the internal list for a check
#'   * `get`: Extract the list of conditions raised by `BiocCheck`
#'   * `getNum`: Tally the number of condition provided by the input
#'   * `zero`: Reset the internal log of the condition provided
#'   * `getBiocCheckDir`: Report and create the `<package>.BiocCheck`
#'     directory as obtained from the metadata
#'   * `composeReport`: Simplify the list structure from the `log` and
#'     provide a character vector of conditions raised
#'   * `report`: Write the `00BiocCheck.log` report into the `BiocCheck`
#'     folder
#'   * `toJSON`: Write a JSON file to the location indicated with the
#'     conditions raised
#'   * `fromJSON`: Read a JSON file from the location indicated with the
#'     output of previous conditions raised in the check
#'   * `show`: Display the information in the class. Currently empty.
#'   * `show_meta`: Display the metadata information stored in the `metadata`
#'     field
#'
#' @return An internal `BiocCheck` R5 Reference Class used to document
#'   conditions such as errors, warnings, and notes
#'
#' @seealso [Message-class], [BiocPackage-class]
#'
#' @examples
#'
#' bc <- BiocCheck:::.BiocCheck
#'
#' @exportClass BiocCheck
NULL

.BiocCheck <- setRefClass("BiocCheck",
    fields = list(
        log = "list",
        # checkName
        check = "character",
        # conditions
        error = "list",
        warning = "list",
        note = "list",
        metadata = "list"
    ),
    methods = list(
        initialize = function(...) {
            callSuper(...)
        },
        add = function(
            ..., condition, help_text, messages
        ) {
            if (missing(condition))
                stop(
                    "<Internal> 'condition' should be:",
                    " 'error', 'warning', or 'note'"
                )
            checkName <- .self$getLastCheck()
            mlist <- list(...)[[1]]
            stopifnot(
                "<Internal> Input to '$add' must be a list" = is.list(mlist)
            )
            ins <- Filter(length, list(mlist, help_text, messages))
            nist <- structure(list(ins), .Names = names(mlist))
            .messages$setMessage(nist, condition = condition)
            .self[[condition]] <- append(.self[[condition]], nist)
            .self$log[[checkName]] <- append(.self$log[[checkName]], nist)
        },
        addMetadata = function(BiocPackage, ...) {
            args <- list(...)
            .self$metadata <- list(
                sourceDir = BiocPackage$sourceDir,
                BiocVersion = as.character(BiocManager::version()),
                Package = BiocPackage$packageName,
                PackageVersion = BiocPackage$packageVersion,
                BiocCheckDir = BiocPackage$BiocCheckDir,
                BiocCheckVersion = as.character(packageVersion("BiocCheck")),
                sourceDir = BiocPackage$sourceDir,
                installDir = args[["installDir"]],
                isTarBall = BiocPackage$isTar,
                platform = .Platform$OS.type
            )
        },
        getLastCheck = function() {
            checkName <- .self$check
            if (!length(checkName))
                "undefined"
            else
                checkName
        },
        setCheck = function(checkName) {
            .self$check <- checkName
            ## create a list for appending
            .self$log[[checkName]] <- list()
        },
        get = function(condition) {
            cond <- .self[[condition]]
            if (length(cond)) {
                length_elements <- vapply(
                    cond,
                    function(x) length(unlist(x, use.names = FALSE)),
                    integer(1L)
                )
                split(
                    unlist(cond, use.names = FALSE),
                    rep(names(cond), length_elements)
                )
            } else {
                cond
            }
        },
        getNum = function(conditions = c("error", "warning", "note")) {
            vapply(
                conditions,
                function(condition) {
                    length(.self[[condition]])
                },
                integer(1L)
            )
        },
        zero = function(conditions = c("error", "warning", "note")) {
            for (condition in conditions) {
                .self[[condition]] <- list()
            }
        },
        getBiocCheckDir = function() {
            bioccheck_dir <- .self$metadata$BiocCheckDir
            if (!dir.exists(bioccheck_dir))
                dir.create(bioccheck_dir, recursive = TRUE)
            bioccheck_dir
        },
        composeReport = function(debug = FALSE) {
            unlist(Map(
                    f = function(...) {
                        .composeReport(..., debug = debug)
                    },
                    checkName = names(.self$log),
                    lowerElements = lapply(.self$log, .flattenElement)
            ), use.names = FALSE)
        },
        report = function(debug, isOnBBS) {
            if (isOnBBS)
                return()
            bioccheck_dir <- .self$getBiocCheckDir()
            outputs <- .self$composeReport(debug = debug)
            writeLines(
                outputs, con = file.path(bioccheck_dir, "00BiocCheck.log")
            )
        },
        toJSON = function(file) {
            out <- Filter(length, .self$log)
            checkInstalled("jsonlite")
            jlog <- jsonlite::toJSON(out, auto_unbox = FALSE)
            jsonlite::write_json(jlog, file)
        },
        fromJSON = function(file) {
            checkInstalled("jsonlite")
            infile <- jsonlite::read_json(file)[[1]]
            .self[["log"]] <- jsonlite::fromJSON(infile, simplifyVector = FALSE)
        },
        show = function() {
            invisible()
        },
        show_meta = function() {
            meta <- .self$metadata
            if (!length(meta))
                stop("<Internal> No metadata to show.")
            lapply(
                paste(names(meta), meta, sep = ": "),
                cli::cli_alert
            )
        }
    )
)

.flattenElement <- function(listElem) {
    debugFun <- names(listElem)
    lowerElem <- unlist(listElem, use.names = FALSE)
    attributes(lowerElem) <- list(debugNames = debugFun)
    lowerElem
}

.composeReport <- function(checkName, lowerElements, debug = FALSE) {
    if (!length(lowerElements))
        checkName <- paste(checkName, "OK")
    else if (debug)
        lowerElements <-
            c(lowerElements, paste("DEBUG:", attr(lowerElements, "debugNames")))
    c(checkName, lowerElements)
}

## singletons. Exported but 'hidden' from ls() by the '.'

#' @export
.BiocCheck <- .BiocCheck()
