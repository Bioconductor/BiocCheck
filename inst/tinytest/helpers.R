UNIT_TEST_PKG <- "unitTestTempDir"
UNIT_TEST_TEMPDIR <- file.path(tempdir(), UNIT_TEST_PKG)

create_test_package <-
    function(
        test_dir = tempfile(),
        description = list(),
        pkgpath = tempfile(tmpdir = test_dir),
        extraActions = function(path = NULL) {},
        use.canned = TRUE
    )
{
    canned <- list()
    if (use.canned) {
        canned <- list(
            Author = "Test Author",
            Maintainer = "Test Maintainer <test@test.com>",
            "Authors@R" = NULL
        )
    }
    for (name in names(description))
        canned[[name]] <- description[[name]]

    if (!dir.exists(pkgpath))
        dir.create(pkgpath, recursive = TRUE)
    capture.output({
        suppressWarnings(
            suppressMessages(
                usethis::create_package(
                    path = pkgpath,
                    fields = canned,
                    rstudio = FALSE,
                    open = FALSE
                )
            )
        )
    })
    cat("#", file = file.path(pkgpath, "NAMESPACE"))
    extraActions(pkgpath)
    .BiocPackage$initialize(pkgpath)
}

checkCounter <- function(msg, type = "error") {
    if (missing(msg))
        stop("<internal> Provide message input")
    conds <- c("note", "warning", "error")
    res <- as.integer(conds %in% type)
    names(res) <- conds
    expect_identical(
        .BiocCheck$getNum(c("note", "warning", "error")), res,
        msg
    )
    .BiocCheck$zero()
}

read_test_package <- function(pkgname) {
    pkgdir <- system.file(
        "testpackages", pkgname, package="BiocCheck", mustWork = TRUE
    )
    .BiocPackage$initialize(pkgdir)
}

stillZero <- function()
{
    identical(
        .BiocCheck$getNum(c("note", "warning", "error")),
        c(note = 0L, warning = 0L, error = 0L)
    )
}
