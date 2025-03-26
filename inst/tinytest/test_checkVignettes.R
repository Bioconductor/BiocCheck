source("helpers.R")

library(devtools)
library(tinytest)

# vignettes0 --------------------------------------------------------------
.BiocCheck$zero()
temp_dir <- tempfile()
.bioctest <- create_test_package(
    test_dir = temp_dir,
    extraActions = function(path) {
        vigdir <- file.path(path, "vignettes")
        dir.create(vigdir, recursive = TRUE)
    }
)
BiocCheck:::checkVignetteDir(.bioctest)
checkCounter("No vignette sources in vignettes/ directory.", "error")
.BiocCheck$zero()

.bioctest <- create_test_package(
    test_dir = temp_dir, description = list(Title = "unitTestTempDir"),
    extraActions = function(path) {
        vigdir <- file.path(path, "vignettes")
        dir.create(vigdir, recursive = TRUE)
        cat("nothing", file = file.path(vigdir, "test.Rnw"))
    }
)
BiocCheck:::checkVignetteDir(.bioctest)
expect_identical(
    .BiocCheck$getNum(c("error", "warning", "note")),
    c(error = 0L, warning = 3L, note = 1L),
    "Rmd recommend WARNING"
)
.BiocCheck$zero()

.bioctest <- create_test_package(
    test_dir = temp_dir,
    description = list(Title = "unitTestTempDir", Suggests = "knitr"),
    extraActions = function(path) {
        vigdir <- file.path(path, "vignettes")
        dir.create(vigdir, recursive = TRUE)
        cat(c(
            "% \\VignetteIndexEntry{header} \n",
            "% \\VignetteEngine{knitr} \nnothing"
        ), file = file.path(vigdir, "test.Rnw"))
    }
)
BiocCheck:::checkVignetteDir(.bioctest)
expect_identical(
    .BiocCheck$getNum(c("error", "warning", "note")),
    c(error = 0L, warning = 1L, note = 1L),
    "test OK"
)
.BiocCheck$zero()

.bioctest <- create_test_package(
    test_dir = temp_dir,
    extraActions = function(path) {
        instdoc <- file.path(path, "inst", "doc")
        dir.create(instdoc, recursive = TRUE)
        cat("nothing", file = file.path(instdoc, "test.rnw"))
    }
)
## check rnw file in inst/doc  WARNING
BiocCheck:::checkInstContents(.bioctest)
checkCounter(
    "Remove vignette sources from inst/doc", "warning"
)
.BiocCheck$zero()

.bioctest <- create_test_package(
    test_dir = temp_dir,
    extraActions = function(path) {
        instdoc <- file.path(path, "inst", "doc")
        dir.create(instdoc, recursive = TRUE)
        cat("nothing", file = file.path(instdoc, "test.Rmd"))
    }
)
## check rmd file in inst/doc  WARNING
BiocCheck:::checkInstContents(.bioctest)
checkCounter(
    "Rmd file in inst/doc not seen as valid vignette source", "warning"
)
.BiocCheck$zero()

## check for Rnw vignettes, warn if any
.bioctest <- read_test_package("testpkg0")
BiocCheck:::checkVigTypeRNW(.bioctest)
expect_equivalent(
    .BiocCheck$getNum("warning"), 1L,
    info = "check for Rnw vignettes, warn if any"
)
.BiocCheck$zero()

## check for duplicate chunk labels
BiocCheck:::checkDupChunkLabels(
    .bioctest$VigSources["vignettes/dupChunks.Rmd"]
)
expect_true(
    any(
        grepl(
        pattern="duplicate chunk labels",
        .BiocCheck$get("error")[["checkDupChunkLabels"]]
        )
    )
)
.BiocCheck$zero()

.bioctest <- create_test_package(
    test_dir = temp_dir, description = list(VignetteBuilder = "knitr"),
    extraActions = function(path) {
        vigdir <- file.path(path, "vignettes")
        dir.create(vigdir, recursive = TRUE)
        cat(
            "% \\VignetteIndexEntry{header} \nnnothing",
            file = file.path(vigdir, "test.Rnw")
        )
    }
)
expect_silent(
    BiocCheck:::checkVigBuilder(.bioctest)
)
BiocCheck:::checkVigTypeRNW(.bioctest)
expect_equivalent(
    .BiocCheck$getNum("warning"), 1L
)
.BiocCheck$zero()

## check 'SystemRequirements' in DESCRIPTION for qmd
.bioctest <- read_test_package("testpkg0")
BiocCheck:::checkVigTypeQMD(.bioctest)
expect_true(
    grepl(
        pattern = "'SystemRequirements' field not in DESCRIPTION",
        .BiocCheck$get("warning")[["checkVigTypeQMD"]]
    )
)
.BiocCheck$zero()

BiocCheck:::checkVigSessionInfo(.bioctest)
expect_equivalent(
    .BiocCheck$getNum("note"), 1L
)
.BiocCheck$zero()

.bioctest <- create_test_package(
    test_dir = temp_dir, description = list(Title = "something"),
    extraActions = function(path) {
        vigdir <- file.path(path, "vignettes")
        dir.create(vigdir, recursive = TRUE)
        cat(
            "% \\VignetteIndexEntry{header} \nnnothing",
            file = file.path(vigdir, "test.Rnw")
        )
    }
)
BiocCheck:::checkVignetteDir(.bioctest)
expect_identical(
    .BiocCheck$getNum(c("error", "warning", "note")),
    c(error = 0L, warning = 2L, note = 1L),
    "Rmd recommend, no builder in DESCRIPTION"
)
.BiocCheck$zero()

.bioctest <- create_test_package(
    test_dir = temp_dir, description = list(VignetteBuilder = "Sweave"),
    extraActions = function(path) {
        vigdir <- file.path(path, "vignettes")
        dir.create(vigdir, recursive = TRUE)
        cat(
            "% \\VignetteIndexEntry{header} \nnnothing",
            file = file.path(vigdir, "test.Rnw")
        )
    }
)
BiocCheck:::checkVignetteDir(.bioctest)
expect_identical(
    .BiocCheck$getNum(c("error", "warning", "note")),
    c(error = 0L, warning = 2L, note = 1L),
    "Rmd recommend, no builder in DESCRIPTION"
)
.BiocCheck$zero()

.bioctest <- read_test_package("testpkg0")
BiocCheck:::checkVignetteDir(.bioctest)
expect_identical(
    .BiocCheck$getNum(c("error", "warning", "note")),
    c(error = 5L, warning = 10L, note = 1L),
    "check vignette style of example pkg; test multiple errors, warnings"
)
expect_true(
    any(grepl(
        pattern="VignetteIndex",
        .BiocCheck$get("warning")[["checkVigTemplate"]]
    ))
)
.BiocCheck$zero()

BiocCheck:::checkVigEvalAllFalse(.bioctest)
expect_equivalent(
    .BiocCheck$getNum("warning"), 1L
)
expect_true(
    any(
        grepl(
            "evalfalse.Rmd",
            .BiocCheck$get("warning")[["checkVigEvalAllFalse"]]
        )
    )
)
.BiocCheck$zero()


.bioctest <- read_test_package("testpkg2")
BiocCheck:::checkVignetteDir(.bioctest)
expect_identical(
    .BiocCheck$getNum(c("error", "warning", "note")),
    c(error = 2L, warning = 5L, note = 2L),
    "check vignette style of example pkg; vignette metadata"
)
expect_true(
    any(grepl(
        pattern="VignetteBuilder",
        .BiocCheck$get("warning")[["checkVigSuggests"]]
    ))
)
expect_true(
    any(grepl(pattern="VignetteEngine",
          .BiocCheck$get("error")[["checkVigEngine"]]
    ))
)
expect_true(
    any(grepl(
        pattern="missing vignette metadata", ignore.case = TRUE,
        .BiocCheck$get("warning")[["checkVigMetadata"]]
    ))
)
expect_true(
    any(grepl(
        pattern="not currently Suggested",
        .BiocCheck$get("warning")[["checkVigSuggests"]]
    ))
)
expect_true(
    grepl(
        pattern="Evaluate more vignette chunks",
        .BiocCheck$get("warning")[["checkVigChunkEval"]]
    )
)
.BiocCheck$zero()

.bioctest <- read_test_package("testpkg2")
BiocCheck:::checkVigFiles(.bioctest)
expect_identical(
    .BiocCheck$getNum(c("error", "warning", "note")),
    c(error = 0L, warning = 0L, note = 1L),
    "check vignette intermediate files"
)
.BiocCheck$zero()

BiocCheck:::checkVigEvalAllFalse(.bioctest)
expect_equivalent(
    .BiocCheck$getNum("warning"), 1L
)
expect_true(
    any(
        grepl(
            "vignettes/testpkg0.Rmd",
            .BiocCheck$get("warning")[["checkVigEvalAllFalse"]],
            fixed = TRUE,
        )
    )
)
.BiocCheck$zero()

unlink(temp_dir, recursive = TRUE)

