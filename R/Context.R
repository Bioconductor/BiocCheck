#' Report context of events to user with a data.frame of events and locations
#'
#' Report context of events to user with a data.frame of events and locations
#'
#'
#' @param file character(1) full path (including package name) of file being
#'   summarized.
#'
#' @param lines character() vector of text lines in file
#'
#' @param idx logical() same length as \code{lines} indicating lines in which
#'   event occurs
#'
#' @param offset `integer(1)` The number of lines to add to the 'Line' column
#'   calculation. It is mainly used to account for the number of lines that the
#'   YAML header occupies.
#'
#' @return Context: a data.frame() with columns File, Line, and Context
Context <- function(file="", lines=character(), idx=logical(), offset = 0L) {
    stopifnot(
        BiocBaseUtils::isScalarCharacter(file, zchar = TRUE),
        BiocBaseUtils::isScalarInteger(offset)
    )
    data.frame(
        File = rep(.getDirFiles(file), sum(idx)),
        Line = which(idx) + offset,
        Context = lines[idx],
        stringsAsFactors=FALSE
    )
}
