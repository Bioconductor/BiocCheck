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

.errors <- new("MsgClass", msg=character(0))
.warnings <- new("MsgClass", msg=character(0))
.notes <- new("MsgClass", msg=character(0))
