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
        isVerbose = function() {
            .self$verbose
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
        composeReport = function(debug) {
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
        writeNSsuggests = function(isOnBBS) {
            if (isOnBBS)
                return()
            bioccheck_dir <- getBiocCheckDir()
            pkgName <- getElement(.self$metadata, "Package")
            require(pkgName,
                lib.loc = file.path(.self$metadata$installDir, "lib"),
                quietly = TRUE, character.only = TRUE)
            suggestions <- try(
                suppressMessages(suppressWarnings(
                    capture.output(codetoolsBioC::writeNamespaceImports(pkgName))
                )), silent = TRUE
            )
            if (inherits(suggestions, "try-error")) {
                msg <- "Could not get namespace suggestions."
            } else {
                msg <- c("Namespace import suggestions are:", suggestions)
            }
            writeLines(
                msg, con = file.path(bioccheck_dir, "00NAMESPACE.log")
            )
        },
        toJSON = function(file) {
            out <- Filter(length, .self$log)
            jlog <- toJSON(out, auto_unbox = FALSE)
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

.BiocCheck <- .BiocCheck()
.messages <- .MessageCondition()

.zeroCounters <- function()
{
    .BiocCheck$zero(c("error", "warning", "note"))
}
