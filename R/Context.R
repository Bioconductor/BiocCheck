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
#' @param nlines numeric(1) The number of lines to provide context for.
#'
#' @param width numeric(1) The width of the console; defaults to the value
#'     given by `getOption("width")`
#'
#' @param idx logical() same length as \code{lines} indicating lines
#'     in which event occurs
#'
#' @md
#'
#' @return Context: a data.frame() with columns File, Line, and Context
Context <- function(pkg="", file="", lines=character(), idx=logical()) {
    data.frame(
        File=rep(mungeName(file, pkg), sum(idx)), Line=which(idx),
        Context=lines[idx], stringsAsFactors=FALSE)
}

#' @rdname Context
#'
#' @param ctxt Object derived from Context()
#'
#' @return handleContext: side effect is output on the message stream
handleContext <- function(ctxt, nlines=6, width=getOption("width")) {
    ctxt <- head(ctxt, nlines)
    txt <- c(
        sprintf("First %d lines:", nrow(ctxt)),
        sprintf("%s:%d %s", ctxt$File, ctxt$Line, ctxt$Context))
    handleVerbatim(txt, width=width)
}
