source("helpers.R")

library(devtools)
library(tinytest)

.bioctest <- read_test_package("testpkg1")
dcf <- .bioctest$DESCRIPTION
BiocCheck:::checkFndPerson(dcf)
checkCounter(
    "No 'fnd' role", "note"
)


temp_dir <- tempfile()
.bioctest <- create_test_package(
    test_dir = temp_dir,
    extraActions = function(pkgdir) {
        badfile <- file.path(pkgdir, "DESCRIPTION")
        writeLines(c(
            "Package: foo",
            "Version: 0.99.0",
            "Authors@R: person('Foo', 'Bar', email = 'foo.bar@email.com')",
            "License: GPL-2",
            "Depends: R (>= 4.0.0)"
        ), badfile)
    }
)
dcf <- .bioctest$DESCRIPTION
expect_identical(
    BiocCheck:::.PersonsFromDCF(dcf),
    person('Foo', 'Bar', email = 'foo.bar@email.com')
)
BiocCheck:::checkFndPerson(dcf)
checkCounter(
    "No 'fnd' role", "note"
)

.bioctest <- create_test_package(
    test_dir = temp_dir,
    extraActions = function(pkgdir) {
        badfile <- file.path(pkgdir, "DESCRIPTION")
        writeLines(c(
            "Package: foo",
            "Version: 0.99.0",
            paste0(
                "Authors@R: person(",
                "'Foo', 'Bar', email = 'foo.bar@email.com', ",
                "role = c('aut', 'cre', 'fnd'))"
            ),
            "License: GPL-2",
            "Depends: R (>= 4.0.0)"
        ), badfile)
    }
)
dcf <- .bioctest$DESCRIPTION
BiocCheck:::checkFndPerson(dcf)
expect_true(
    stillZero()
)

# test checkValidDESCfields -----------------------------------------------
.bioctest <- create_test_package(
    test_dir = temp_dir,
    extraActions = function(pkgdir) {
        desc <- file.path(pkgdir, "DESCRIPTION")
        writeLines(c(
            "Package: foo",
            "Version: 0.99.0",
            "License: GPL-2",
            "Config/test/val: yes"
        ), desc)
    }
)
BiocCheck:::checkValidDESCfields(.bioctest)
expect_true(stillZero())

.bioctest <- create_test_package(
    test_dir = temp_dir,
    extraActions = function(pkgdir) {
        desc <- file.path(pkgdir, "DESCRIPTION")
        writeLines(c(
            "Package: foo",
            "Version: 0.99.0",
            "License: GPL-2",
            "Bugreport: http://bugs",
            "biocviews: Software"
        ), desc)
    }
)
BiocCheck:::checkValidDESCfields(.bioctest)
checkCounter("Unknown or non-standard DESCRIPTION field(s)", "warning")

# test checkBiocType ------------------------------------------------------
dcf <- cbind(Package = "foo", BiocType = "Software")
BiocCheck:::checkBiocType(dcf)
expect_true(stillZero())

dcf <- cbind(Package = "foo")
BiocCheck:::checkBiocType(dcf)
expect_true(stillZero())

for (type in c("ExperimentData", "Annotation", "Workflow", "Book", "Docker")) {
    BiocCheck:::checkBiocType(cbind(Package = "foo", BiocType = type))
    expect_true(stillZero())
}

dcf <- cbind(Package = "foo", BiocType = "Package")
BiocCheck:::checkBiocType(dcf)
checkCounter("Invalid 'BiocType' field", "error")

dcf <- cbind(Package = "foo", BiocType = "workflow")
BiocCheck:::checkBiocType(dcf)
expect_true(
    any(grepl(
        "Did you mean 'Workflow'",
        unlist(.BiocCheck$get("error"), use.names = FALSE),
        fixed = TRUE
    ))
)
checkCounter("BiocType with incorrect case", "error")

unlink(temp_dir, recursive = TRUE)
