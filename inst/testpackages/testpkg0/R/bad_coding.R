bad_fun <- function(){

    update.packages("base")

    for (i in 1:10) {
        print("test")
    }

    sapply(letters, function(x) x)
}

pasteTest <- function() {

    message(paste0("A", "B"))
    message ( paste("A", "B"))

    warning(paste("A", "B"), "C")
    warning(paste0("A", "B"), "C")

    stop("A", paste0("B", "C"))
    stop("A", paste("B", "C"))
}

invalid_ref <- function() {

    bcheck <- BiocCheck:BiocCheck
    red <- 1
    xbluex <- 3
    bcheck <- xbluex:red

}
