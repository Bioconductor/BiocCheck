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
        add = function(..., condition, help_text, messages, verbose = FALSE) {
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

## singletons. Exported but 'hidden' from ls() by the '.'

.BiocCheck <- .BiocCheck()
.messages <- .MessageCondition()

.zeroCounters <- function()
{
    .BiocCheck$zero(c("error", "warning", "note"))
}
