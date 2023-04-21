# BiocCheck-class ---------------------------------------------------------

#' @name BiocCheck-class
#'
#' @title A class for composing BiocCheck reports.
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
#'   the fields included are
#'
#' \describe{
#'   \item{BiocCheckVersion}{ The version of the BiocCheck package }
#'   \item{BiocVersion}{ The version of Bioconductor }
#'   \item{Package}{ The name of the package in check }
#'   \item{PackageVersion}{ The version of the package in check }
#'   \item{sourceDir}{ The directory of the package source or tarball in check }
#'   \item{installDir}{ The directory where the package is installed for
#'     testing, a temporary location by default
#'   }
#'   \item{BiocCheckDir}{ The directory where the `<package>.BiocCheck` folder
#'     is saved. Usually the same folder as the package in check }
#'   \item{platform}{ The platform/OS where the check is taking place }
#'   \item{isTarBall}{ Whether the package in check is a source directory or a
#'     tarball }
#' }
#'
#' @field log `list()` A running list of all conditions raised (i.e., notes,
#'   warnings, errors)
#'
#' @field check `character(1)` The title of the last check used for logging
#'   purposes.
#'
#' @field error, warning, note `list()` Finer extraction of each condition type
#'
#' @field metadata `list()` A list of additional information relevant to the
#'   package and its state. See details.
#'
#' @field verbose `logical(1)` Whether to show the full information pertaining
#'   to the checks. A `FALSE` value will only show the condition messages
#'   and not any relevant files or additional information. The defaults are
#'   `FALSE` and `TRUE` for `BiocCheck` and `BiocCheckGitClone`, respectively.
#'
#' @return A `BiocCheck` instance
#'
#' @md
#'
#' @keywords internal
#' @seealso \link{Message-class}
#'
#' @importFrom utils tail
#'
#' @examples
#'
#' bc <- BiocCheck:::.BiocCheck
#'
#' bc$verbose
#'
NULL

# BiocCheck-methods -------------------------------------------------------

#' @name BiocCheck-methods
#'
#' @title A list of methods for the BiocCheck reference class
#'
#' @aliases add,BiocCheck-method
#'
#' @param ... character() A vector that makes up the `BiocCheck` exception
#'   message (e.g., 'Vignette must be built by R CMD build'). The character
#'   vector is handled with `paste0` and made into a list and appended with
#'   `help_text` and `messages`.
#'
#' @param help_text character(1) Additional text prompting a list of files
#'   (e.g,. "Found in files:")
#'
#' @param condition character(1) One of the three conditions handled: `error`,
#'   `warning`, or `note`
#'
#' @param messages character() Often a vector of file names where the check
#'   was triggered.
#'
#' @param verbose logical(1) Whether or not to output both the `help_text` and
#'   `messages` as part of the report
#'
#' @param debug logical(1) Whether to append the name of the originating check
#'   name into for traceability
#'
#' @param checkName character(1) The title of the current group of checks. It
#'   can be set with `handleCheck`, e.g., `handleCheck("Checking for version
#'   number mismatch...")`. Internally, it is saved with `setCheck` and obtained
#'   with `getLastCheck`.
#'
#' @param isOnBBS logical(1) Indicates whether the checks are being run on the
#'   Bioconductor Build System (BBS). This is helpful for avoiding the creation
#'   of folders in the BBS.
#'
#' @param file character(1) A path to a JSON file for writing or reading as
#'   created by `toJSON` and `fromJSON` `BiocCheck` methods.
#'
#' @section methods:
#' \describe{
#'   \item{add}{ Include a condition to the `BiocCheck` report }
#'   \item{getLastCheck}{ Obtain the name of the last check run }
#'   \item{setCheck}{ Create a new element in the internal list for a check }
#'   \item{get}{ Extract the list of conditions raised by `BiocCheck` }
#'   \item{getNum}{ Tally the number of condition provided by the input }
#'   \item{zero}{ Reset the internal log of the condition provided }
#'   \item{getBiocCheckDir}{ Report and create the `<package>.BiocCheck`
#'     directory as obtained from the metadata }
#'   \item{composeReport}{ Simplify the list structure from the `log` and
#'     provide a character vector of conditions raised }
#'   \item{report}{ Write the `00BiocCheck.log` report into the `BiocCheck`
#'     folder }
#'   \item{toJSON}{ Write a JSON file to the location indicated with the
#'     conditions raised }
#'   \item{fromJSON}{ Read a JSON file from the location indicated with the
#'     output of previous conditions raised in the check }
#'   \item{show}{ Display the information in the class. Currently empty. }
#'   \item{show_meta}{ Display the metadata information stored in the `metadata`
#'     field }
#' }
#'
#' @md
#'
NULL

#' @exportClass BiocCheck
.BiocCheck <- setRefClass("BiocCheck",
    fields = list(
        log = "list",
        # checkName
        check = "character",
        # conditions
        error = "list",
        warning = "list",
        note = "list",
        metadata = "list",
        verbose = "logical"
    ),
    methods = list(
        initialize =
            function(verbose = FALSE, ...) {
            callSuper(verbose = verbose, ...)
        },
        add = function(
            ..., condition, help_text, messages, verbose = .self$verbose
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
            dots <- sprintf(paste0("* ", toupper(condition), ": %s"), mlist)
            ins <- Filter(length, list(dots, help_text, messages))
            nist <- structure(list(ins), .Names = names(mlist))
            .messages$setMessage(nist, verbose = verbose, condition = condition)
            .self[[condition]] <- append(.self[[condition]], nist)
            .self$log[[checkName]] <- append(.self$log[[checkName]], nist)
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
        getNum = function(conditions) {
            vapply(
                conditions,
                function(condition) {
                    length(.self[[condition]])
                },
                integer(1L)
            )
        },
        zero = function(conditions) {
            for (cond in conditions) {
                .self[[cond]] <- list()
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
            bioccheck_dir <- getBiocCheckDir()
            outputs <- .self$composeReport(debug = debug)
            writeLines(
                outputs, con = file.path(bioccheck_dir, "00BiocCheck.log")
            )
        },
        toJSON = function(file) {
            out <- Filter(length, .self$log)
            jlog <- jsonlite::toJSON(out, auto_unbox = FALSE)
            if (!requireNamespace("jsonlite", quietly = TRUE))
                stop("Install 'jsonlite' to use the write method.")
            jsonlite::write_json(jlog, file)
        },
        fromJSON = function(file) {
            if (!requireNamespace("jsonlite", quietly = TRUE))
                stop("Install 'jsonlite' to use the read method.")
            infile <- jsonlite::read_json(file)[[1]]
            .self[["log"]] <- jsonlite::fromJSON(infile, simplifyVector = FALSE)
        },
        show = function() {
            invisible()
        },
        show_meta = function() {
            meta <- .self$metadata
            if (!length(meta))
                stop("No metadata to show.")
            message(
                paste0(
                    "\U2500", " ", paste(names(meta), meta, sep = ": "), "\n"
                )
            )
        }
    )
)

# Message-class -----------------------------------------------------------

#' @name Message-class
#'
#' @title A lower level Message helper class for BiocCheck
#'
#' @field msg `list()` A list of character messages usually grown with `append`
#'   with conditions raised by a check
#'
#' @field condition character(1) One of the three conditions handled: `error`,
#'   `warning`, or `note`
#'
#' @seealso \link{BiocCheck-class}
#'
#' @return A `Message` class instance
#'
#' @md
#'
NULL

# Message-methods ---------------------------------------------------------

#' @name Message-methods
#'
#' @title A list of methods for the Message reference class
#'
#' @aliases setMessage,Message-method
#' @aliases setCondition,Message-method
#' @aliases getCondition,Message-method
#'
#' @param condition character(1) One of the three conditions handled: `error`,
#'   `warning`, or `note`
#'
#' @param verbose logical(1) Whether to output the full text in the check or
#'   only the check name itself in the report
#'
#' @param \dots `list()` A nested list with the check name as the top level
#'   layer. Second level lists include any `help_text` and `messages` that are
#'   part of the check.
#'
#' @md
#'
NULL

.MessageCondition <- setRefClass("Message",
    fields = list(
        msg = "list",
        condition = "character"
    ),
    methods = list(
        setMessage = function(..., verbose = FALSE, condition) {
            text <- list(...)[[1L]]
            .self$setCondition(condition)
            .self$msg <- append(.self$msg, text)
            if (!verbose) {
                handleMessage(
                    head(unlist(text, use.names = FALSE), 1L), indent = 4
                )
            } else {
                lens <- lengths(text)
                indents <- seq(4, by = 2, length = lens)
                text[[1]][[lens]] <- selectSome(unlist(tail(text[[1]], 1L)))
                mapply(
                    handleMessage,
                    unlist(text, recursive = FALSE, use.names = FALSE),
                    indent = indents
                )
            }
            .self$msg
        },
        setCondition = function(condition) {
            stopifnot(
                "'condition' must be one of 'error', 'warning', 'note'" =
                condition %in% c("error", "warning", "note")
            )
            .self$condition <- append(.self$condition, condition)
        },
        getCondition = function() {
            .self$condition
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
.messages <- .MessageCondition()

.zeroCounters <- function()
{
    .BiocCheck$zero(c("error", "warning", "note"))
}
