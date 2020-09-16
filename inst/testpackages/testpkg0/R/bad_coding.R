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

    dataurl <- paste0("https://", "raw", ".githubusercontent", ".com")
    laburl <- paste0("https://", "gitlab.com/", "raw", "/master")
    bucketurl <- paste0("https://", "bitbucket.org", "/test/", "raw", "/file")
    download.file(dataurl, destfile = tempfile())

}
