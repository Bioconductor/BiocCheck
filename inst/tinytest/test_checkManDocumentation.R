## source("inst/tinytest/helpers.R")
source("helpers.R")

library(devtools)
library(tinytest)

# checkForPromptComments --------------------------------------------------
cli::cli_h3("checkForPromptComments")

.BiocCheck$zero()
temp_dir <- tempfile()

# Case 1: Auto-generated '%% ~' comment present
.bioctest <- create_test_package(
    test_dir = temp_dir,
    extraActions = function(path) {
        mandir <- file.path(path, "man")
        dir.create(mandir, recursive = TRUE)
        cat(
            "\\name{test1}
            \\alias{test1}
            \\title{test1}
            \\description{test1}\n%% ~auto-generated comment
            ",
            file = file.path(mandir, "test1.Rd")
        )
    }
)
BiocCheck:::checkForPromptComments(.bioctest)
expect_equivalent(.BiocCheck$getNum("note"), 1L)
notes <- .BiocCheck$get("note")
expect_true(
    any(
        grepl(
            pattern = "Auto-generated '%% ~' comments",
            notes[["checkForPromptComments"]]
        )
    )
)

# Case 2: No prompt comments
.BiocCheck$zero()
.bioctest <- create_test_package(
    test_dir = temp_dir,
    extraActions = function(path) {
        mandir <- file.path(path, "man")
        dir.create(mandir, recursive = TRUE)
        cat(
            "\\name{test2}
            \\alias{test2}
            \\title{test2}
            \\description{test2}
            ",
            file = file.path(mandir, "test2.Rd")
        )
    }
)
BiocCheck:::checkForPromptComments(.bioctest)
expect_true(stillZero())

unlink(temp_dir, recursive = TRUE)


# checkForValueSection ----------------------------------------------------
cli::cli_h3("checkForValueSection")

.BiocCheck$zero()
temp_dir <- tempfile()

# Case 1: Missing value and format sections
.bioctest <- create_test_package(
    test_dir = temp_dir,
    extraActions = function(path) {
        mandir <- file.path(path, "man")
        dir.create(mandir, recursive = TRUE)
        # Function Rd page with missing value section
        cat(
            "\\name{fun1}
            \\alias{fun1}
            \\title{fun1}
            \\description{fun1}
            \\usage{fun1()}
            ",
            file = file.path(mandir, "fun1.Rd")
        )
        # Data Rd page with missing format section
        cat(
            "\\name{data1}
            \\alias{data1}
            \\docType{data}
            \\title{data1}
            \\description{data1}
            ",
            file = file.path(mandir, "data1.Rd")
        )
    }
)
BiocCheck:::checkForValueSection(.bioctest)
expect_equivalent(.BiocCheck$getNum("warning"), 2L)
warnings <- .BiocCheck$get("warning")
expect_true(
    any(
        grepl(
            pattern = "Empty or missing \\\\value sections",
            warnings[["checkForValueSection"]]
        )
    )
)
expect_true(
    any(
        grepl(
            pattern = "Empty or missing \\\\format sections",
            warnings[["checkForValueSection"]]
        )
    )
)

# Case 2: Correct value and format sections, and internal page skipped
.BiocCheck$zero()
.bioctest <- create_test_package(
    test_dir = temp_dir,
    extraActions = function(path) {
        mandir <- file.path(path, "man")
        dir.create(mandir, recursive = TRUE)
        # Function Rd page with value section
        cat(
            "\\name{fun1}
            \\alias{fun1}
            \\title{fun1}
            \\description{fun1}
            \\usage{fun1()}
            \\value{A list of results}
            ",
            file = file.path(mandir, "fun1.Rd")
        )
        # Data Rd page with format section
        cat(
            "\\name{data1}
            \\alias{data1}
            \\docType{data}
            \\title{data1}
            \\description{data1}
            \\format{A data frame}
            ",
            file = file.path(mandir, "data1.Rd")
        )
        # Internal Rd page which does not need value
        cat(
            "\\name{internal1}
            \\alias{internal1}
            \\title{internal1}
            \\description{internal1}
            \\keyword{internal}
            ",
            file = file.path(mandir, "internal1.Rd")
        )
    }
)
BiocCheck:::checkForValueSection(.bioctest)
expect_true(stillZero())

unlink(temp_dir, recursive = TRUE)


# checkExportsAreDocumented -----------------------------------------------
cli::cli_h3("checkExportsAreDocumented")

# Case 1: Under 80% have runnable examples (0/2 have examples) -> ERROR
.BiocCheck$zero()
temp_dir <- tempfile()
.bioctest <- create_test_package(
    test_dir = temp_dir,
    extraActions = function(path) {
        # Export fun1 and fun2 in NAMESPACE
        cat("export(fun1)\nexport(fun2)\n", file = file.path(path, "NAMESPACE"))

        # Create R folder and code
        rdir <- file.path(path, "R")
        dir.create(rdir, recursive = TRUE)
        cat("fun1 <- function() { 1 }\n", file = file.path(rdir, "fun1.R"))
        cat("fun2 <- function() { 2 }\n", file = file.path(rdir, "fun2.R"))

        # Create man folder and Rd files without examples
        mandir <- file.path(path, "man")
        dir.create(mandir, recursive = TRUE)
        cat(
            "\\name{fun1}
            \\alias{fun1}
            \\title{fun1}
            \\description{fun1}
            \\usage{fun1()}
            \\value{1}
            ",
            file = file.path(mandir, "fun1.Rd")
        )
        cat(
            "\\name{fun2}
            \\alias{fun2}
            \\title{fun2}
            \\description{fun2}
            \\usage{fun2()}
            \\value{2}
            ",
            file = file.path(mandir, "fun2.Rd")
        )
    }
)
.bioctest$inst_setup()
BiocCheck:::checkExportsAreDocumented(.bioctest, lib.loc = .bioctest$installDir)
expect_equivalent(.BiocCheck$getNum("error"), 1L)
errors <- .BiocCheck$get("error")
expect_true(
    any(
        grepl(
            pattern = "At least 80% of man pages",
            errors[["checkExportsAreDocumented"]]
        )
    )
)
# Cleanup installation
.bioctest$installDir |> dirname() |> unlink(recursive = TRUE)
unlink(temp_dir, recursive = TRUE)

# Case 2: >= 80% have runnable examples but not all (4/5 have examples) -> NOTE
.BiocCheck$zero()
temp_dir <- tempfile()
.bioctest <- create_test_package(
    test_dir = temp_dir,
    extraActions = function(path) {
        # Export fun1..5 in NAMESPACE
        cat(
            "export(fun1)
            export(fun2)
            export(fun3)
            export(fun4)
            export(fun5)
            ",
            file = file.path(path, "NAMESPACE")
        )

        # Create R folder and code
        rdir <- file.path(path, "R")
        for (i in 1:5) {
            cat(
                paste0("fun", i, " <- function() { ", i, " }\n"),
                file = file.path(rdir, paste0("fun", i, ".R"))
            )
        }

        # Create man folder and Rd files
        mandir <- file.path(path, "man")
        dir.create(mandir, recursive = TRUE)
        for (i in 1:4) {
            cat(
                paste0(
                    "\\name{fun", i, "}
                    \\alias{fun", i, "}
                    \\title{fun", i, "}
                    \\description{fun", i, "}
                    \\usage{fun", i, "()}
                    \\value{", i, "}
                    ",
                    "\\examples{\nfun", i, "()\n}\n"
                ),
                file = file.path(mandir, paste0("fun", i, ".Rd"))
            )
        }
        # fun5 has no examples
        cat(
            "\\name{fun5}
            \\alias{fun5}
            \\title{fun5}
            \\description{fun5}
            \\usage{fun5()}
            \\value{5}
            ",
            file = file.path(mandir, "fun5.Rd")
        )
    }
)
.bioctest$inst_setup()
BiocCheck:::checkExportsAreDocumented(.bioctest, lib.loc = .bioctest$installDir)
expect_equivalent(.BiocCheck$getNum("error"), 0L)
expect_equivalent(.BiocCheck$getNum("note"), 1L)
notes <- .BiocCheck$get("note")
expect_true(
    any(
        grepl(
            pattern = "Consider adding runnable examples",
            notes[["checkExportsAreDocumented"]]
        )
    )
)
# Cleanup installation
.bioctest$installDir |> dirname() |> unlink(recursive = TRUE)
unlink(temp_dir, recursive = TRUE)

# Case 3: 100% have runnable examples -> Zero conditions
.BiocCheck$zero()
temp_dir <- tempfile()
.bioctest <- create_test_package(
    test_dir = temp_dir,
    extraActions = function(path) {
        # Export fun1 in NAMESPACE
        cat("export(fun1)\n", file = file.path(path, "NAMESPACE"))

        # Create R folder and code
        rdir <- file.path(path, "R")
        cat("fun1 <- function() { 1 }\n", file = file.path(rdir, "fun1.R"))

        # Create man folder and Rd files
        mandir <- file.path(path, "man")
        dir.create(mandir, recursive = TRUE)
        cat(
            "\\name{fun1}
            \\alias{fun1}
            \\title{fun1}
            \\description{fun1}
            \\usage{fun1()}
            \\value{1}
            \\examples{
            fun1()
            }
            ",
            file = file.path(mandir, "fun1.Rd")
        )
    }
)
.bioctest$inst_setup()
BiocCheck:::checkExportsAreDocumented(.bioctest, lib.loc = .bioctest$installDir)
expect_true(stillZero())
# Cleanup installation
.bioctest$installDir |> dirname() |> unlink(recursive = TRUE)
unlink(temp_dir, recursive = TRUE)


# checkUsageOfDont --------------------------------------------------------
cli::cli_h3("checkUsageOfDont")

.BiocCheck$zero()
temp_dir <- tempfile()

# Case 1: Usage of donttest and dontrun in non-internal page
.bioctest <- create_test_package(
    test_dir = temp_dir,
    extraActions = function(path) {
        mandir <- file.path(path, "man")
        dir.create(mandir, recursive = TRUE)
        # Rd page using donttest
        cat(
            "\\name{fun1}
            \\alias{fun1}
            \\title{fun1}
            \\description{fun1}
            \\value{1}
            ",
            "\\examples{\n\\donttest{\n1 + 1\n}\n}\n",
            file = file.path(mandir, "fun1.Rd")
        )
        # Rd page using dontrun
        cat(
            "\\name{fun2}
            \\alias{fun2}
            \\title{fun2}
            \\description{fun2}
            \\value{2}
            ",
            "\\examples{\n\\dontrun{\n2 + 2\n}\n}\n",
            file = file.path(mandir, "fun2.Rd")
        )
    }
)
BiocCheck:::checkUsageOfDont(.bioctest)
expect_equivalent(.BiocCheck$getNum("note"), 2L)
notes <- .BiocCheck$get("note")
expect_true(
    any(
        grepl(
            pattern = "Usage of dontrun\\{\\} / donttest\\{\\} tags found",
            notes[["checkUsageOfDont"]]
        )
    )
)
expect_true(
    any(
        grepl(
            pattern = "Use donttest\\{\\} instead of dontrun\\{\\}",
            notes[["checkUsageOfDont"]]
        )
    )
)

# Case 2: Usage of donttest / dontrun in INTERNAL page (should be allowed)
.BiocCheck$zero()
.bioctest <- create_test_package(
    test_dir = temp_dir,
    extraActions = function(path) {
        mandir <- file.path(path, "man")
        dir.create(mandir, recursive = TRUE)
        cat(
            "\\name{fun1}
            \\alias{fun1}
            \\title{fun1}
            \\description{fun1}
            \\value{1}
            \\keyword{internal}
            ",
            "\\examples{
            \\dontrun{
            1 + 1
            }
            }
            ",
            file = file.path(mandir, "fun1.Rd")
        )
    }
)
BiocCheck:::checkUsageOfDont(.bioctest)
expect_true(stillZero())

unlink(temp_dir, recursive = TRUE)

# checkManDocumentation ----------------------------------------------------
cli::cli_h3("checkManDocumentation")

.BiocCheck$zero()
temp_dir <- tempfile()

# A perfectly clean package should pass checkManDocumentation with no conditions
.bioctest <- create_test_package(
    test_dir = temp_dir,
    extraActions = function(path) {
        # Export fun1 in NAMESPACE
        cat("export(fun1)\n", file = file.path(path, "NAMESPACE"))

        # Create R folder and code
        rdir <- file.path(path, "R")
        cat("fun1 <- function() { 1 }\n", file = file.path(rdir, "fun1.R"))

        # Create man folder and Rd files
        mandir <- file.path(path, "man")
        dir.create(mandir, recursive = TRUE)
        cat(
            "\\name{fun1}
            \\alias{fun1}
            \\title{fun1}
            \\description{fun1}
            \\usage{fun1()}
            \\value{1}
            \\examples{
            fun1()
            }
            ",
            file = file.path(mandir, "fun1.Rd")
        )
    }
)
.bioctest$inst_setup()
BiocCheck:::checkManDocumentation(.bioctest, libloc = .bioctest$installDir)
expect_true(stillZero())

# Cleanup installation
.bioctest$installDir |> dirname() |> unlink(recursive = TRUE)
unlink(temp_dir, recursive = TRUE)
