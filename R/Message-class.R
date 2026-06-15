cli_error <- function(...) {
    cli::cli_div(theme = list(.error = list(color = "red")))
    cli::cli_alert_danger(paste0("{.error ERROR: ", ..., "}"), wrap = TRUE)
}

cli_warning <- function(...) {
    cli::cli_div(theme = list(.warning = list(color = "orange")))
    cli::cli_alert_warning(paste0("{.warning WARNING: ", ..., "}"), wrap = TRUE)
}

cli_note <- function(...) {
    cli::cli_div(theme = list(.note = list(color = "blue")))
    cli::cli_alert_info(paste0("{.note NOTE: ", ..., "}"), wrap = TRUE)
}

# Message-class and methods------------------------------------------------

#' @name Message-class
#'
#' @docType class
#'
#' @title A lower level Message helper class for BiocCheck
#'
#' @aliases setMessage,Message-method setCondition,Message-method
#' @aliases getCondition,Message-method
#'
#' @field msg `list()` A list of character messages usually grown with `append`
#'   with conditions raised by a check
#'
#' @field condition `character(1)` One of the three conditions handled: `error`,
#'   `warning`, or `note`
#'
#' @param condition `character(1)` One of the three conditions handled: `error`,
#'   `warning`, or `note`
#'
#' @param \dots `list()` A nested list with the check name as the top level
#'   layer. Second level lists include any `help_text` and `messages` that are
#'   part of the check.
#'
#' @section methods:
#'   * `setMessage`: Set the message and condition (`error`, `warning`, or
#'     `note`) for the check
#'   * `setCondition`: Change the condition for the check
#'   * `getCondition`: Get the condition from the `Message` class
#'
#' @importFrom BiocBaseUtils selectSome
#'
#' @seealso [BiocCheck-class], [BiocPackage-class]
#'
#' @return `.MessageCondition`: An internal `R5` Reference Class to handle
#'   messages and their conditions, e.g., for errors, warnings, or notes.
#'
#' @keywords internal
.MessageCondition <- setRefClass("Message",
    fields = list(
        msg = "list",
        condition = "character"
    ),
    methods = list(
        setMessage = function(..., condition) {
            text <- list(...)[[1L]]
            .self$setCondition(condition)
            clifun <- switch(
                condition,
                error = cli_error,
                warning = cli_warning,
                note = cli_note
            )
            .self$msg <- append(.self$msg, text)
            comps <- unlist(text, recursive = FALSE, use.names = FALSE)
            if (identical(length(comps), 1L)) {
                clifun(unlist(comps, use.names = FALSE))
            } else {
                dotlist <- selectSome(tail(comps, 1L)[[1L]], maxToShow = 3L)
                names(dotlist) <- rep("*", length(dotlist))
                dotlist <- gsub("{", "{{", dotlist, fixed = TRUE) |>
                    gsub("}", "}}", x = _, fixed = TRUE)
                alerttitle <- head(
                    unlist(text, recursive = FALSE, use.names = FALSE), -1L
                )
                clifun(
                    head(unlist(alerttitle, use.names = FALSE), 1L)
                )
                id <- cli::cli_div(theme = list(div = list("margin-left" = 2L)))
                if (identical(length(comps), 3L)) {
                    cli::cli_text(unlist(alerttitle, use.names = FALSE)[2L])
                    id2 <- cli::cli_div(theme = list(div = list("margin-left" = 2L)))
                }
                for (item in dotlist)
                    cli::cli_li(item)
                if (identical(length(comps), 3L))
                    cli::cli_end(id2)
                cli::cli_end(id)
            }
            .self$msg
        },
        setCondition = function(condition) {
            stopifnot(
                "'condition' must be one of 'error', 'warning', 'note'" =
                condition %in% c("error", "warning", "note")
            )
            .self$condition <- append(.self$condition, condition)
        },
        getCondition = function() {
            .self$condition
        }
    )
)

.messages <- .MessageCondition()
