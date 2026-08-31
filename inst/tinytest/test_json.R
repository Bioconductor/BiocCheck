source("helpers.R")

# machine-readable report -------------------------------------------------
cli::cli_h3("JSON report")

## conditions are raised from within a check function; 'checkFun' is taken
## from the calling function's name
noteCheck <- function() {
    BiocCheck:::handleNote("A note")
}
warnCheck <- function() {
    BiocCheck:::handleWarningFiles(
        "A warning", messages = "R/foo.R (line 3, column 5)"
    )
}
errorCheck <- function() {
    BiocCheck:::handleErrorFiles(
        "An error", messages = "R/bar.R#L42 x <- 1 ..."
    )
}

.BiocCheck$zero()
BiocCheck:::handleCheck("Checking JSON output...")
noteCheck()
warnCheck()
errorCheck()

json <- .BiocCheck$toJSON()
expect_true(jsonlite::validate(json))

payload <- jsonlite::fromJSON(json, simplifyVector = FALSE)
expect_identical(
    names(payload), c("metadata", "summary", "status", "entries", "text")
)

## the summary is the tally of the conditions raised
expect_equal(
    unlist(payload[["summary"]]), c(error = 1, warning = 1, note = 1)
)
expect_identical(payload[["status"]], "error")
expect_identical(
    length(payload[["entries"]]), sum(.BiocCheck$getNum())
)

entries <- payload[["entries"]]
expect_identical(
    vapply(entries, `[[`, character(1L), "severity"),
    c("note", "warning", "error")
)
expect_identical(
    vapply(entries, `[[`, character(1L), "checkFun"),
    c("noteCheck", "warnCheck", "errorCheck")
)
expect_identical(
    vapply(entries, `[[`, character(1L), "check"),
    rep("Checking JSON output...", 3L)
)
expect_identical(entries[[1L]][["message"]], "A note")

## 'help_text' is absent as 'null', not as an empty array
expect_null(entries[[1L]][["help_text"]])
expect_identical(entries[[2L]][["help_text"]], "Found in files:")

## 'details' is always an array, even when a single message is reported
expect_true(is.list(entries[[1L]][["details"]]))
expect_identical(length(entries[[1L]][["details"]]), 0L)
expect_identical(
    entries[[3L]][["details"]], list("R/bar.R#L42 x <- 1 ...")
)

## locations are parsed from both formats emitted by the checks
expect_null(entries[[1L]][["locations"]])
expect_identical(
    entries[[2L]][["locations"]],
    list(list(file = "R/foo.R", line = 3L, column = 5L))
)
expect_identical(
    entries[[3L]][["locations"]], list(list(file = "R/bar.R", line = 42L))
)

# .parseLocations ---------------------------------------------------------
cli::cli_h3(".parseLocations")

expect_null(BiocCheck:::.parseLocations(character(0L)))
expect_null(BiocCheck:::.parseLocations("no location here"))
expect_identical(
    BiocCheck:::.parseLocations(
        c("R/a.R#L1 code ...", "vignettes/b.Rmd#L20 more code ...")
    ),
    data.frame(file = c("R/a.R", "vignettes/b.Rmd"), line = c(1L, 20L))
)
expect_identical(
    BiocCheck:::.parseLocations(
        c("a.R (line 1, column 2)", "b.R (line 30, column 4)")
    ),
    data.frame(
        file = c("a.R", "b.R"), line = c(1L, 30L), column = c(2L, 4L)
    )
)
## the symbol found is not part of the file name
expect_identical(
    BiocCheck:::.parseLocations(
        c(
            "sapply() in R/a.R (line 1, column 2)",
            "update.packages() in R/b.R (line 3, column 4)"
        )
    ),
    data.frame(
        file = c("R/a.R", "R/b.R"), line = c(1L, 3L), column = c(2L, 4L)
    )
)
## chunk-relative lines are not reported as file locations
expect_null(
    BiocCheck:::.parseLocations("a.Rmd (chunk no. 2, line 1, column 2)")
)

# round trip --------------------------------------------------------------
cli::cli_h3("toJSON / fromJSON")

jsonfile <- tempfile(fileext = ".json")
.BiocCheck$toJSON(file = jsonfile)
expect_true(file.exists(jsonfile))

messages <- vapply(.BiocCheck$entries, `[[`, character(1L), "message")
.BiocCheck$zero()
expect_identical(.BiocCheck$entries, list())

roundtrip <- .BiocCheck$fromJSON(jsonfile)
expect_identical(roundtrip[["status"]], "error")
expect_identical(
    vapply(.BiocCheck$entries, `[[`, character(1L), "message"), messages
)

## an empty report is still valid and reports an 'ok' status
.BiocCheck$entries <- list()
payload <- jsonlite::fromJSON(.BiocCheck$toJSON(), simplifyVector = FALSE)
expect_identical(payload[["status"]], "ok")
expect_identical(payload[["entries"]], list())
expect_equal(
    unlist(payload[["summary"]]), c(error = 0, warning = 0, note = 0)
)

# report ------------------------------------------------------------------
cli::cli_h3("report")

bioccheck_dir <- file.path(tempfile(), "test.BiocCheck")
.BiocCheck$metadata <- list(Package = "test", BiocCheckDir = bioccheck_dir)
BiocCheck:::handleCheck("Checking report output...")
noteCheck()

.BiocCheck$report(debug = FALSE, isOnBBS = TRUE)
expect_false(dir.exists(bioccheck_dir))

.BiocCheck$report(debug = FALSE, isOnBBS = FALSE)
expect_true(file.exists(file.path(bioccheck_dir, "00BiocCheck.log")))
expect_true(file.exists(file.path(bioccheck_dir, "00BiocCheck.json")))

payload <- jsonlite::read_json(file.path(bioccheck_dir, "00BiocCheck.json"))
expect_identical(payload[["status"]], "note")
expect_identical(payload[["metadata"]][["Package"]], "test")
## the JSON is a superset of the plain text report
expect_identical(
    unlist(payload[["text"]]),
    readLines(file.path(bioccheck_dir, "00BiocCheck.log"))
)

unlink(dirname(bioccheck_dir), recursive = TRUE)
.BiocCheck$zero()
