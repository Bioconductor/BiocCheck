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

bad_dl <- function() {

    dataurl <- "https://raw.githubusercontent.com/file"
    githurl <- "https://github.com/tree/master/"
    githurl <- "https://dropbox.com/data?dl=1"
    laburl <- "https://gitlab.com/raw/master/data"
    bucketurl <- "https://bitbucket.org/test/raw/file"
    download.file(dataurl, destfile = tempfile())

}
