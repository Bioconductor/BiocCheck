source("helpers.R")

.BiocCheck$zero()
temp_dir <- tempfile()

## exportPattern with [[:alpha:]]
.BiocPackage <- create_test_package(
    test_dir = temp_dir,
    extraActions = function(pkgdir) {
        cat("exportPattern('[[:alpha:]]')",
            file=file.path(pkgdir, "NAMESPACE"))
    }
)
BiocCheck:::checkNAMESPACE(.BiocPackage)
checkCounter("has exportPattern", "error")

## valid NAMESPACE
.BiocPackage <- create_test_package(
    test_dir = temp_dir,
    extraActions = function(pkgdir) {
        cat("export(a, b, c)",
            file=file.path(pkgdir, "NAMESPACE"))
    }
)
BiocCheck:::checkNAMESPACE(.BiocPackage)
expect_true(stillZero())

unlink(temp_dir, recursive = TRUE)
