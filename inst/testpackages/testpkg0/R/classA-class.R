.classA <- setClass(
    "classA",
    slots = c(
        slot1 = "numeric",
        slot2 = "character"
    )
)

setMethod(
    "show",
    "classA",
    function(object) {
        cat("An object of class 'classA'\n")
        cat("slot1:", object@slot1, "\n")
        cat("slot2:", object@slot2, "\n")
    }
)

.classB <- setClass(
    "classB",
    slots = c(
        slot3 = "logical",
        slot4 = "list"
    )
)
