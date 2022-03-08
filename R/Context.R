#' @rdname Context
#'
#' @title Report context of events to user with a data.frame of events and
#' locations
#'
#' @param pkg character(1) name of the package
#'
#' @param file character(1) full path (including package name) of file
#'     being summarized.
#'
#' @param lines character() vector of text lines in file
#'
#' @param idx logical() same length as \code{lines} indicating lines
#'     in which event occurs
#'
#' @md
#'
#' @return Context: a data.frame() with columns File, Line, and Context
Context <- function(pkg="", file="", lines=character(), idx=logical()) {
    data.frame(
        File=rep(getDirFile(file), sum(idx)), Line=which(idx),
        Context=lines[idx], stringsAsFactors=FALSE)
}
