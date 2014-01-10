.printf <- function(...) cat(noquote(sprintf(...)), "\n")

.msg <- function(...) message(noquote(sprintf(...)))
.stop <- function(...) stop(noquote(sprintf(...)))


handleError <- function(msg)
{
    num_errors$bump()
    .stop("* ERROR: %s", msg)
}

handleWarning <- function(msg)
{
    num_warnings$bump()
    .msg("* WARNING: %s", msg)
}

handleNote <- function(msg)
{
    num_notes$bump()
    sprintf("* NOTE: %s", msg)
}