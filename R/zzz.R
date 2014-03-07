setRefClass("MsgClass",
    fields=list(
        msg="character"
    ),
    methods=list(
        get = function() .self$msg,
        getNum = function() length(.self$msg),
        zero = function() .self$msg <- character(0),
        add = function(m) .self$msg <- append(.self$msg, m)
        )
)

