.Message <- setRefClass("Message",
    fields=list(
        msg="character"
    ),
    methods=list(
        get = function() .self$msg,
        getNum = function() length(.self$msg),
        zero = function() .self$msg <- character(0),
        add = function(...) {
            m <- paste0(...)
            .self$msg <- append(.self$msg, m)
            m
        })
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
