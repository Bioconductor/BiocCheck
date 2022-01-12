.BiocCheck <- setRefClass("BiocCheck",
    fields = list(
        log = "list",
        # checkName
        check = "character",
        # conditions
        error = "list",
        warning = "list",
        note = "list"
    ),
    methods = list(
        add = function(..., condition, help_text, messages) {
            if (missing(condition))
                stop(
                    "<Internal> 'condition' should be:",
                    " 'error', 'warning', or 'note'"
                )
            checkName <- .self$getCheck()
            mlist <- list(...)[[1]]
            stopifnot(
                "<Internal> Input to '$add' must be a list" = is.list(mlist)
            )
            dots <- sprintf(paste0("* ", toupper(condition), ": %s"), mlist)
            ins <- Filter(length, list(dots, help_text, messages))
            nist <- structure(list(ins), .Names = names(mlist))
            .messages$setMessage(nist, condition = condition)
            .self[[condition]] <- append(.self[[condition]], nist)
            .self$log[[checkName]] <- append(.self$log[[checkName]], nist)
        },
        getCheck = function() {
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
        }
    )
)

.MessageCondition <- setRefClass("Message",
    fields = list(
        msg = "list",
        condition = "character"
    ),
    methods = list(
        setMessage = function(..., condition) {
            text <- list(...)[[1L]]
            .self$setCondition(condition)
            indents <- seq(4, by = 2, length = lengths(text))
            .self$msg <- append(.self$msg, text)
            mapply(
                handleMessage,
                unlist(text, recursive = FALSE, use.names = FALSE),
                indent = indents
            )
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

.event <- .BiocCheck()
.messages <- .MessageCondition()

.zeroCounters <- function()
{
    .event$zero(c("error", "warning", "note"))
}
