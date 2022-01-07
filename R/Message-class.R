.Message <- setRefClass("Message",
    fields=list(
        msg="character",
        ml = "list"
    ),
    methods=list(
        get = function() .self$msg,
        getNum = function() length(.self$msg),
        zero = function() .self$msg <- character(0),
        add = function(...) {
            m <- paste0(...)
            .self$msg <- append(.self$msg, m)
            m
        },
        ## identify and combine elements with the same name
        getList = function()
            split(unlist(.self$ml, use.names = FALSE), names(.self$ml)),
        addList = function(...) {
            mlist <- list(...)[[1]]
            stopifnot(is.list(mlist))
            .self$ml <- c(.self$ml, mlist)
            mlist
        }
    )
)

## singletons. Exported but 'hidden' from ls() by the '.'

.error <- .Message()

.warning <- .Message()

.note <- .Message()

.zeroCounters <- function()
{
    .error$zero()
    .warning$zero()
    .note$zero()
}
