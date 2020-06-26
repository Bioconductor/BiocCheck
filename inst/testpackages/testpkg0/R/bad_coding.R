bad_fun <- function(){

    update.packages("base")

    for (i in 1:10) {
        print("test")
    }

    sapply(letters, function(x) x)
}

invalid_ref <- function() {

    bcheck <- BiocCheck:BiocCheck
    red <- 1
    xbluex <- 3
    bcheck <- xbluex:red

}
