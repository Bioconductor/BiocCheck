.onLoad <- function(libname, pkgname) {
    ## download.file should not be used here
    if (FALSE) {
        download.file("https://httpbin.org/get", destfile = tempfile())
    }
}

.onAttach <- function(libname, pkgname) {
    if (FALSE) {
        downloader::download("https://httpbin.org/get", destfile = tempfile())
    }
}
