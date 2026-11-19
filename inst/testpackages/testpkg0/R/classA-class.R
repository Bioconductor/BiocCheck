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

## S4 classes extending S3 / basic classes do not require a show() method
.classC <- setClass("classC", contains = "array")

.classD <- setClass("classD", contains = "classC")

## guard: relies on the methods package's show,oldClass method
setOldClass("someS3class")
.classE <- setClass("classE", contains = "someS3class")
