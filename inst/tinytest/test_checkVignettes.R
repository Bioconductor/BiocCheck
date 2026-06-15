source("helpers.R")

library(devtools)
library(tinytest)

# vignettes0 --------------------------------------------------------------
cli::cli_h3("vignettes0")

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
        cat(
            "<<echo=FALSE, results=tex>>=\n## some code\n@\n",
            file = file.path(vigdir, "test.Rnw")
        )
    }
)
BiocCheck:::checkVignetteDir(.bioctest)
expect_true(
    any(
        grepl(
            pattern="VignetteIndexEntry",
            .BiocCheck$get("warning")[["checkVigTemplate"]]
        )
    )
)
expect_true(
    any(
        grepl(
            pattern="RMarkdown instead of Sweave",
            .BiocCheck$get("warning")[["checkVigTypeRNW"]]
        )
    )
)
expect_true(
    any(
        grepl(
            pattern="'sessionInfo' not found",
            .BiocCheck$get("note")[["checkVigSessionInfo"]]
        )
    )
)
expect_true(
    any(
        grepl(
            pattern="missing chunk labels",
            .BiocCheck$get("note")[["checkChunkLabels"]]
        )
    )
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

## check for missing chunk labels in Rmd
BiocCheck:::checkChunkLabels(
    .bioctest$VigSources["vignettes/testpkg0.Rmd"]
)
expect_true(
    any(
        grepl(
            pattern="missing chunk labels",
            .BiocCheck$get("note")[["checkChunkLabels"]]
        )
    )
)
.BiocCheck$zero()

## check for missing chunk labels in qmd
BiocCheck:::checkChunkLabels(
    .bioctest$VigSources["vignettes/testpkg0.qmd"]
)
expect_true(
    any(
        grepl(
            pattern="missing chunk labels",
            .BiocCheck$get("note")[["checkChunkLabels"]]
        )
    )
)
.BiocCheck$zero()

## check for missing chunk labels in Rnw
BiocCheck:::checkChunkLabels(
    .bioctest$VigSources["vignettes/testpkg0.Rnw"]
)
expect_true(
    any(
        grepl(
            pattern="missing chunk labels",
            .BiocCheck$get("note")[["checkChunkLabels"]]
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

## check for quarto in 'SystemRequirements' in DESCRIPTION
.bioctest <- create_test_package(
    test_dir = temp_dir,
    description = list(
        VignetteBuilder = "quarto",
        SystemRequirements = "azcopy"
    ),
    extraActions = function(path) {
        vigdir <- file.path(path, "vignettes")
        dir.create(vigdir, recursive = TRUE)
        cat(
            "---\n",
            "%\\VignetteIndexEntry{A Quarto Vignette}\n",
            "%\\VignetteEngine{quarto}\n",
            "%\\VignetteEncoding{UTF-8}\n",
            "---\n\n",
            "# Quarto Vignette\n\n",
            "```{r}\n",
            "print('This is a Quarto vignette')\n",
            "```\n",
            sep = "",
            file = file.path(vigdir, "test.qmd")
        )
    }
)
BiocCheck:::checkVigTypeQMD(.bioctest)
expect_true(
    grepl(
        pattern = "'SystemRequirements' does not list 'quarto'",
        .BiocCheck$get("warning")[["checkVigTypeQMD"]]
    )
)

BiocCheck:::checkVigSessionInfo(.bioctest)
expect_true(
    any(
        grepl(
            pattern = "'sessionInfo' not found",
            .BiocCheck$get("note")[["checkVigSessionInfo"]]
        )
    )
)
.BiocCheck$zero()

.bioctest <- read_test_package("testpkg0")
BiocCheck:::checkVigTemplate(.bioctest$VigSources["vignettes/testpkg0.Rmd"])
expect_true(
    any(
        grepl(
            pattern="VignetteIndex",
            .BiocCheck$get("warning")[["checkVigTemplate"]]
        )
    )
)
.BiocCheck$zero()

BiocCheck:::checkVigEvalAllFalse(.bioctest)
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
BiocCheck:::checkVigSuggests(.bioctest)
expect_true(
    any(
        grepl(
            pattern="VignetteBuilder",
            .BiocCheck$get("warning")[["checkVigSuggests"]]
        )
    )
)
.BiocCheck$zero()

BiocCheck:::checkVigEngine(.bioctest)
expect_true(
    any(
        grepl(
            pattern="VignetteEngine",
            .BiocCheck$get("error")[["checkVigEngine"]]
        )
    )
)
.BiocCheck$zero()

BiocCheck:::checkVigMetadata(.bioctest$VigSources["vignettes/testpkg0.Rmd"])
expect_true(
    any(
        grepl(
            pattern="missing vignette metadata", ignore.case = TRUE,
            .BiocCheck$get("warning")[["checkVigMetadata"]]
        )
    )
)
.BiocCheck$zero()

BiocCheck:::checkVigSuggests(.bioctest)
expect_true(
    any(
        grepl(
            pattern="not currently Suggested",
            .BiocCheck$get("warning")[["checkVigSuggests"]]
        )
    )
)
.BiocCheck$zero()

BiocCheck:::checkVigChunkEval(.bioctest$VigSources["vignettes/testpkg0.Rmd"])
expect_true(
    grepl(
        pattern="Evaluate more vignette chunks",
        .BiocCheck$get("warning")[["checkVigChunkEval"]]
    )
)
.BiocCheck$zero()

.bioctest <- read_test_package("testpkg2")
BiocCheck:::checkVigFiles(.bioctest)
expect_true(
    any(
        grepl(
            pattern="intermediate files found",
            .BiocCheck$get("note")[["checkVigFiles"]]
        )
    )
)
.BiocCheck$zero()

BiocCheck:::checkVigEvalAllFalse(.bioctest)
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

