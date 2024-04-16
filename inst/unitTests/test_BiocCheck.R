UNIT_TEST_PKG <- "unitTestTempDir"
UNIT_TEST_TEMPDIR <- file.path(tempdir(), UNIT_TEST_PKG)

library(devtools)

.zeroCounters <- BiocCheck:::.zeroCounters

parsedCode <- NULL

create_test_package <-
    function(
        test_dir = tempfile(),
        description=list(), extraActions=function(path=NULL){}
    )
{
    canned <- list(Author="Test Author",
        Maintainer="Test Maintainer <test@test.com>", "Authors@R"=NULL)
    for (name in names(description))
    {
        canned[[name]] <- description[[name]]
    }
    pkgpath <- tempfile(tmpdir = test_dir)
    if (!dir.exists(pkgpath))
        dir.create(pkgpath, recursive = TRUE)
    capture.output({
        suppressMessages(
            usethis::create_package(pkgpath, canned, rstudio=FALSE, open = FALSE)
        )
    })

    cat("#", file=file.path(pkgpath, "NAMESPACE"))
    extraActions(pkgpath)
    pkgpath
}

checkError <- function(msg)
{
    if (missing(msg)) msg = ""
    checkTrue(
        identical(
            .BiocCheck$getNum(c("note", "warning", "error")),
            c(note = 0L, warning = 0L, error = 1L)
        ),
        msg
    )
    .zeroCounters()
}

checkCounter <- function(msg, type = "error") {
    if (missing(msg))
        stop("<internal> Provide message input")
    conds <- c("note", "warning", "error")
    res <- as.integer(conds %in% type)
    names(res) <- conds
    checkTrue(
        all(
            mapply(
                all.equal,
                .BiocCheck$getNum(c("note", "warning", "error")), res
            )
        ),
        msg
    )
    .zeroCounters()
}

stillZero <- function()
{
    identical(
        .BiocCheck$getNum(c("note", "warning", "error")),
        c(note = 0L, warning = 0L, error = 0L)
    )
}

.setUp <- function()
{
    .zeroCounters()
    dir.create(UNIT_TEST_TEMPDIR)
}

.tearDown <- function()
{
    unlink(UNIT_TEST_TEMPDIR, TRUE)
}

setVersion <- function(version)
{
    cat(paste("Version:", version),
        file=file.path(UNIT_TEST_TEMPDIR, "DESCRIPTION"))
}


test_vignettes0 <- function()
{
    pkgdir <- UNIT_TEST_TEMPDIR
    vigdir <- file.path(pkgdir, "vignettes")
    if (!dir.exists(pkgdir))
        dir.create(pkgdir)

    ## no vignettes dir ERROR
    if (!dir.exists(vigdir)) {
        vde <- BiocCheck:::checkVigDirExists(vigdir)
        checkTrue(!vde)
        .zeroCounters()
        # empty vignette dir ERROR
        dir.create(vigdir, recursive=TRUE)
    }

    BiocCheck:::checkVignetteDir(pkgdir, TRUE)
    checkCounter("No vignette sources in vignettes/ directory.", "error")
    .zeroCounters()

    # vig dir w/ source file  WARNING
    cat("nothing", file=file.path(vigdir, "test.Rnw"))
    cat("Title: unitTestTempDir", file=file.path(pkgdir, "DESCRIPTION"))
    BiocCheck:::checkVignetteDir(pkgdir, TRUE)
    checkIdentical(
        .BiocCheck$getNum(c("error", "warning", "note")),
        c(error = 0L, warning = 2L, note = 1L)
    )
    .zeroCounters()

    # test OK
    cat(
        "% \\VignetteIndexEntry{header} \n% \\VignetteEngine{knitr} \nnothing",
        file=file.path(vigdir, "test.Rnw")
    )
    cat(
        "Title: unitTestTempDir\nSuggests: knitr",
        file=file.path(pkgdir, "DESCRIPTION")
    )
    BiocCheck:::checkVignetteDir(pkgdir, TRUE)
    checkIdentical(
        .BiocCheck$getNum(c("error", "warning", "note")),
        c(error = 0L, warning = 1L, note = 1L)
    )
    .zeroCounters()


    # check rnw file in inst/doc  WARNING
    instdoc <- file.path(pkgdir, "inst", "doc")
    dir.create(instdoc, recursive=TRUE)
    cat("nothing", file=file.path(instdoc, "test.rnw"))
    BiocCheck:::checkInstContents(pkgdir, TRUE)
    checkCounter(
        "Remove vignette sources from inst/doc", "warning"
    )
    .zeroCounters()

    # check rmd file in inst/doc  WARNING
    unlink(instdoc, TRUE)
    dir.create(instdoc, recursive=TRUE)
    cat("nothing", file=file.path(instdoc, "test.Rmd"))
    BiocCheck:::checkInstContents(pkgdir, TRUE)
    checkCounter(
        "Rmd file in inst/doc not seen as valid vignette source", "warning"
    )
    unlink(file.path(instdoc, "test.Rmd"))
    .zeroCounters()

    # check for Rnw vignettes, warn if any
    vigdircontents <- BiocCheck:::getVigSources(
        system.file(
            "testpackages", "testpkg0", "vignettes", package="BiocCheck"
        )
    )
    BiocCheck:::checkVigTypeRNW(vigdircontents)
    checkEqualsNumeric(1, .BiocCheck$getNum("warning"))
    .zeroCounters()

    # check vigbuilder ERROR
    # in DESCRIPTION but not any vignette
    # also checks if builder listed but not in DESCRIPTION import,depend,suggest
    cat("VignetteBuilder: knitr", file=file.path(pkgdir, "DESCRIPTION"))
    cat(
        "% \\VignetteIndexEntry{header} \nnnothing",
        file=file.path(vigdir, "test.Rnw")
    )
    BiocCheck:::checkVignetteDir(pkgdir, TRUE)
    checkIdentical(
        .BiocCheck$getNum(c("error", "warning", "note")),
        c(error = 1L, warning = 2L, note = 1L)
    )
    .zeroCounters()

    # check vignette builder default
    # vignette directory currently contains rnw
    unlink(file.path(pkgdir, "DESCRIPTION"))
    cat("Title: something", file=file.path(pkgdir, "DESCRIPTION"))
    BiocCheck:::checkVignetteDir(pkgdir, TRUE)
    # no error if RNW
    checkIdentical(
        .BiocCheck$getNum(c("error", "warning", "note")),
        c(error = 0L, warning = 1L, note = 1L)
    )
    .zeroCounters()

    # check defined in desc but default vig
    unlink(file.path(pkgdir, "DESCRIPTION"))
    cat("VignetteBuilder: Sweave", file=file.path(pkgdir, "DESCRIPTION"))
    BiocCheck:::checkVignetteDir(pkgdir, TRUE)
    checkIdentical(
        .BiocCheck$getNum(c("error", "warning", "note")),
        c(error = 0L, warning = 2L, note = 1L)
    )
    .zeroCounters()

    # check vignette style of example package
    # 2 WARNINGS - vignette template and evaluate more chunks
    pkgdir <- system.file("testpackages", "testpkg0", package="BiocCheck")
    BiocCheck:::checkVignetteDir(pkgdir, TRUE)
    checkIdentical(
        .BiocCheck$getNum(c("error", "warning", "note")),
        c(error = 4L, warning = 9L, note = 1L)
    )
    checkTrue(
        any(grepl(
            pattern="VignetteIndex",
            .BiocCheck$get("warning")[["checkVigTemplate"]]
        ))
    )
    .zeroCounters()

    # check vignette style of example package
    BiocCheck:::checkVignetteDir(system.file("testpackages",
        "testpkg2", package="BiocCheck"), TRUE)
    checkIdentical(
        .BiocCheck$getNum(c("error", "warning", "note")),
        c(error = 2L, warning = 4L, note = 2L)
    )
    checkTrue(
        any(grepl(
            pattern="VignetteBuilder",
            .BiocCheck$get("warning")[["checkVigSuggests"]]
        ))
    )
    checkTrue(
        any(grepl(pattern="VignetteEngine",
              .BiocCheck$get("error")[["checkVigEngine"]]
        ))
    )
    checkTrue(
        any(grepl(
            pattern="missing vignette metadata", ignore.case = TRUE,
            .BiocCheck$get("warning")[["checkVigMetadata"]]
        ))
    )
    checkTrue(
        any(grepl(
            pattern="not currently Suggested",
            .BiocCheck$get("warning")[["checkVigSuggests"]]
        ))
    )
    checkTrue(
        grepl(
            pattern="Evaluate more vignette chunks",
            .BiocCheck$get("warning")[["checkVigChunkEval"]]
        )
    )
    .zeroCounters()

    # check vignette 'intermediate' files
    vigdir <- system.file("testpackages", "testpkg2", "vignettes",
                          package="BiocCheck")
    vigdircontents <- BiocCheck:::getVigSources(vigdir)
    BiocCheck:::checkVigFiles(vigdir, vigdircontents)
    checkIdentical(
        .BiocCheck$getNum(c("error", "warning", "note")),
        c(error = 0L, warning = 0L, note = 1L)
    )
    .zeroCounters()

}

test_checkVersionNumber <- function()
{
    setVersion("lkjgfhfdlkgjhdflkgj")
    BiocCheck:::checkVersionNumber(UNIT_TEST_TEMPDIR)
    checkError("Garbage version doesn't cause error!")
    setVersion("1.2.3.4")
    BiocCheck:::checkVersionNumber(UNIT_TEST_TEMPDIR)
    checkError("Version 1.2.3.4 doesn't cause error!")
    isDevel <- (BiocManager:::.version_bioc("devel") == BiocManager::version())
    .zeroCounters()
    if (isDevel)
    {
        setVersion("1.2.3")
    } else {
        setVersion("1.3.3")
    }
    BiocCheck:::checkVersionNumber(UNIT_TEST_TEMPDIR)
    checkTrue(.BiocCheck$getNum("warning") ==1)
    .zeroCounters()
}

test_checkNewPackageVersionNumber <- function()
{
    setVersion("1.99.3")
    BiocCheck:::checkNewPackageVersionNumber(UNIT_TEST_TEMPDIR)
    checkCounter(
        "New package x version non-zero",
        "warning"
    )

    setVersion("00.99.3")
    BiocCheck:::checkNewPackageVersionNumber(UNIT_TEST_TEMPDIR)
    checkTrue(stillZero())

    setVersion("0.2.3")
    BiocCheck:::checkNewPackageVersionNumber(UNIT_TEST_TEMPDIR)
    checkCounter(
        "New package y version not 99",
        "error"
    )

    setVersion("0.99.1")
    BiocCheck:::checkNewPackageVersionNumber(UNIT_TEST_TEMPDIR)
    checkTrue(stillZero())
}

test_checkRbuildignore <- function()
{
    rbuildfile <- file.path(UNIT_TEST_TEMPDIR, ".Rbuildignore")
    cat(
        "tests", "tests/", "^tests$", "^tests/", "^tests/$",
        ".*/testthat", "^tests/testthat/cache$", "^longtests/testthat/cache$",
        "longtests", "longtests/", "^longtests/", "^longtests$", "^longtests/$",
        "^nnntests$",
        sep = "\n", file = rbuildfile
    )
    checkIdentical(
        BiocCheck:::.testRbuildignore(readLines(rbuildfile)),
        c(
            TRUE, TRUE, TRUE, TRUE, FALSE,
            FALSE, FALSE, FALSE,
            TRUE, TRUE, TRUE, TRUE, FALSE,
            FALSE
        )
    )
}

test_checkBiocCheckOutputFolder <- function()
{
    check_folder <- file.path(UNIT_TEST_TEMPDIR, "testpkg.BiocCheck")
    dir.create(check_folder, recursive = TRUE)
    BiocCheck:::checkBiocCheckOutputFolder(dirname(check_folder), "testpkg")
    checkEqualsNumeric(.BiocCheck$getNum("error"), 1L)
    unlink(check_folder)
    .zeroCounters()
}

test_checkInstDocFolder <- function() {
    pkg_folder <- file.path(UNIT_TEST_TEMPDIR, "testPkg")
    inst_dir <- file.path(pkg_folder, "inst", "doc")
    dir.create(inst_dir, recursive = TRUE)
    BiocCheck:::checkInstDocFolder(pkg_folder, "testpkg")
    checkEqualsNumeric(.BiocCheck$getNum("error"), 0L)

    file.create(file.path(inst_dir, "index.html"))
    BiocCheck:::checkInstDocFolder(pkg_folder, "testpkg")
    checkEqualsNumeric(.BiocCheck$getNum("error"), 1L)

    unlink(pkg_folder, recursive = TRUE)
    .zeroCounters()
}

test_checkBiocViews <- function()
{
    if (!dir.exists(UNIT_TEST_TEMPDIR))
        dir.create(UNIT_TEST_TEMPDIR)
    .zeroCounters()
    cat("Foo: bar", file=file.path(UNIT_TEST_TEMPDIR, "DESCRIPTION"))
    BiocCheck:::checkBiocViews(UNIT_TEST_TEMPDIR)
    checkTrue(.BiocCheck$getNum("error") == 1,
        "missing biocViews doesn't produce error")

    .zeroCounters()
    cat(
        "biocViews: foo, Cancer, bar,\n    baz",
        file=file.path(UNIT_TEST_TEMPDIR, "DESCRIPTION")
    )
    BiocCheck:::checkBiocViews(UNIT_TEST_TEMPDIR)
    # Invalid views = 1 warning for 4 terms
    checkEqualsNumeric(.BiocCheck$getNum("warning"), 1)

    .zeroCounters()
    cat(
        "biocViews: GO, CellBasedAssays",
        file=file.path(UNIT_TEST_TEMPDIR, "DESCRIPTION")
    )
    BiocCheck:::checkBiocViews(UNIT_TEST_TEMPDIR)
    checkTrue(
        .BiocCheck$getNum("warning") == 0,
        "valid biocViews produce warning"
    )

    .zeroCounters()
    cat(
        "biocViews: aCGH, ChipName",
        file=file.path(UNIT_TEST_TEMPDIR, "DESCRIPTION")
    )
    BiocCheck:::checkBiocViews(UNIT_TEST_TEMPDIR)
    checkTrue(.BiocCheck$getNum("warning") == 1,
        "biocViews from multiple categories don't produce warning")
}

test_badFiles <- function(){
    pkgdir <- UNIT_TEST_TEMPDIR
    if (!dir.exists(pkgdir))
        dir.create(pkgdir)
    badfile <- file.path(pkgdir, "something.Rproj")
    file.create(badfile)
    BiocCheck:::checkBadFiles(pkgdir)
    checkEqualsNumeric(1, .BiocCheck$getNum("error"))
    .zeroCounters()
    unlink(pkgdir)
}

test_checkLicenseForRestrictiveUse <- function() {
    .zeroCounters()
    BiocCheck:::.checkLicenseForRestrictiveUse("GPL-3.0")
    stillZero()

    .zeroCounters()
    BiocCheck:::.checkLicenseForRestrictiveUse("CC BY-NC-ND 4.0")
    checkEqualsNumeric(1, .BiocCheck$getNum("error"))

    .zeroCounters()
    BiocCheck:::.checkLicenseForRestrictiveUse("CC BY-NC-ND 4.0 + file LICENSE")
    checkEqualsNumeric(1, .BiocCheck$getNum("error"))

    .zeroCounters()
    BiocCheck:::.checkLicenseForRestrictiveUse("UNKNOWN")
    checkEqualsNumeric(1, .BiocCheck$getNum("note"))

    .zeroCounters()
    BiocCheck:::.checkLicenseForRestrictiveUse(NA_character_)
    checkEqualsNumeric(1, .BiocCheck$getNum("note"))
}

test_analyze_licenses <- function() {
    licensedb <- gsub(
        "$R_HOME", Sys.getenv("R_HOME"),
        BiocCheck:::.LICENSE_DB_LOCATION, fixed = TRUE
    )
    result <- tools:::analyze_licenses("GPL-3.0", licensedb)
    checkTrue(!result$is_verified)
    BiocCheck:::.checkLicenseForRestrictiveUse("GPL-3.0")
    checkEqualsNumeric(1, .BiocCheck$getNum("note"))
    .zeroCounters()

    result <- tools:::analyze_licenses("GPL-3", licensedb)
    checkTrue(result$is_verified)
    BiocCheck:::.checkLicenseForRestrictiveUse("GPL-3")
    checkEqualsNumeric(0, .BiocCheck$getNum("note"))
}

test_checkIndivFileSizes <- function() {
    .zeroCounters()
    .findLargeFiles_org <- BiocCheck:::.findLargeFiles
    .findLargeFiles <- function(...) {
        c("fileA.rda", "fileB.rds")
    }
    assignInNamespace('.findLargeFiles', .findLargeFiles, "BiocCheck")
    on.exit({
        assignInNamespace('.findLargeFiles', .findLargeFiles_org, "BiocCheck")
    })

    BiocCheck:::checkIndivFileSizes(UNIT_TEST_TEMPDIR)
    checkEqualsNumeric(.BiocCheck$getNum("warning"), 1)
    .zeroCounters()

    BiocCheck:::checkDataFileSizes(UNIT_TEST_TEMPDIR)
    checkEqualsNumeric(.BiocCheck$getNum("warning"), 1)
    .zeroCounters()
}

test_checkBBScompatibility <- function()
{
    pkgdir <- UNIT_TEST_TEMPDIR
    if (!dir.exists(pkgdir))
        dir.create(pkgdir)


    cat("Package : foo",
        "License: GPL-2",
        sep = "\n",
        file=file.path(UNIT_TEST_TEMPDIR, "DESCRIPTION")
    )
    BiocCheck:::checkBBScompatibility(UNIT_TEST_TEMPDIR, FALSE)
    checkError("Space in DESCRIPTION field name doesn't cause error")
    .zeroCounters()


    cat("Package: foo",
        "",
        "Imports: bar",
        "License: GPL-2",
        sep = "\n",
        file=file.path(UNIT_TEST_TEMPDIR, "DESCRIPTION")
    )
    BiocCheck:::checkBBScompatibility(UNIT_TEST_TEMPDIR, FALSE)
    checkError("Blank line in DESCRIPTION doesn't cause error")
    .zeroCounters()

    desc <- system.file(
        "testpackages", "testpkg0", "DESCRIPTION", package="BiocCheck"
    )
    BiocCheck:::.checkDescription(desc)
    checkCounter(
        "Description field in the DESCRIPTION file is too concise",
        "warning"
    )


    cat("Package: Foo",
        "Description: This is a test description field in the Foo package.",
        "  The field should contain two sentences and it should trigger a",
        "  NOTE in the check.",
        "License: GPL-2",
        sep = "\n",
        file = file.path(UNIT_TEST_TEMPDIR, "DESCRIPTION")
    )
    BiocCheck:::.checkDescription(file.path(UNIT_TEST_TEMPDIR, "DESCRIPTION"))
    checkCounter(
        "The Description field in the DESCRIPTION is less than two sentences",
        "note"
    )

    cat("Package: Foo",
        "License: GPL-2",
        sep = "\n",
        file=file.path(UNIT_TEST_TEMPDIR, "DESCRIPTION")
    )
    BiocCheck:::checkBBScompatibility(UNIT_TEST_TEMPDIR, FALSE)
    checkError(
        "Package name which doesn't match dir name does not cause error!"
    )

    cat(paste("Package:", UNIT_TEST_PKG),
        "License: GPL-2",
        sep = "\n",
        file=file.path(UNIT_TEST_TEMPDIR, "DESCRIPTION"))
    BiocCheck:::checkBBScompatibility(UNIT_TEST_TEMPDIR, FALSE)
    checkError("Missing Version doesn't cause error!")


    cat(paste("Package: ", UNIT_TEST_PKG),
        "Version: 0.99.0",
        "Authors@R: 1",
        "License: GPL-2",
        sep = "\n",
        file=file.path(UNIT_TEST_TEMPDIR, "DESCRIPTION"))
    .zeroCounters()
    BiocCheck:::checkBBScompatibility(UNIT_TEST_TEMPDIR, FALSE)
    checkError("Wrong class in Authors@R doesn't cause error!")


    cat(paste("Package:", UNIT_TEST_PKG),
        "Version: 0.99.0",
        paste(
            "Authors@R: c(person('Bioconductor', 'Package Maintainer',",
            "email='maintainer@bioconductor.org', role=c('aut')))"
        ),
        "License: GPL-2",
        sep = "\n",
        file=file.path(UNIT_TEST_TEMPDIR, "DESCRIPTION"))
    .zeroCounters()
    BiocCheck:::checkBBScompatibility(UNIT_TEST_TEMPDIR, FALSE)
    checkError("Missing cre role in Authors@R doesn't cause error!")


    cat(sprintf("Package: %s", UNIT_TEST_PKG),
        "Version: 0.99.0",
        "License: GPL-2",
        sep = "\n",
        file=file.path(UNIT_TEST_TEMPDIR, "DESCRIPTION"))
    BiocCheck:::checkBBScompatibility(UNIT_TEST_TEMPDIR, FALSE)
    checkError("Missing Maintainer and Authors@R doesn't cause error!")

    cat(paste("Package: %s", UNIT_TEST_PKG),
        "Version: 0.99.0",
        "Maintainer: Joe Blow",
        "License: GPL-2",
        sep = "\n",
        file=file.path(UNIT_TEST_TEMPDIR, "DESCRIPTION"))
    BiocCheck:::checkBBScompatibility(UNIT_TEST_TEMPDIR, FALSE)
    checkTrue(
        .BiocCheck$getNum("error") > 0L,
        "Missing email in Maintainer doesn't cause error!"
    )

    .zeroCounters()
    cat(paste("Package: ", UNIT_TEST_PKG),
        "Version: 0.99.0",
        "Maintainer: Joe Blow <joe@blow.com>",
        "Authors@R: c(person('Bioconductor Package Maintainer',",
        "    email='maintainer@bioconductor.org', role=c('aut', 'cre')))",
        "License: GPL-2",
        sep = "\n",
        file=file.path(UNIT_TEST_TEMPDIR, "DESCRIPTION"))
    BiocCheck:::checkDescription(UNIT_TEST_TEMPDIR)
    checkEqualsNumeric(
        .BiocCheck$getNum("error"), 1L,
        "Using Maintainer and Author@R causes error"
    )

    .zeroCounters()
    cat(paste("Package:", UNIT_TEST_PKG),
        "Version: 0.99.0",
        "Authors@R: c(person('Bioconductor', 'Package Maintainer',",
        "    email='maintainer@bioconductor.org',",
        "    comment = c(ORCID = '0000-0000-000-0000'),",
        "    role=c('aut', 'cre')))",
        "License: GPL-2",
        sep = "\n",
        file=file.path(UNIT_TEST_TEMPDIR, "DESCRIPTION"))
    BiocCheck:::checkBBScompatibility(UNIT_TEST_TEMPDIR, FALSE)
    checkTrue(
        .BiocCheck$getNum("note") == 1L,
        "An invalid ORCID iD causes a note!"
    )

    .zeroCounters()
    cat(paste("Package:", UNIT_TEST_PKG),
        "Version: 0.99.0",
        "Maintainer: Joe Blow <joe@blow.com>",
        "License: GPL-2",
        sep = "\n",
        file=file.path(UNIT_TEST_TEMPDIR, "DESCRIPTION"))
    BiocCheck:::checkBBScompatibility(UNIT_TEST_TEMPDIR, FALSE)
    checkTrue(
        .BiocCheck$getNum("error") > 0L,
        "Utilize Maintainer instead of Authors@R doesn't cause error!"
    )

    .zeroCounters()
    cat(paste("Package:", UNIT_TEST_PKG),
        "Version: 0.99.0",
        "Authors@R: c(",
        "  person('Bioconductor Package Maintainer',",
        "    email='maintainer@bioconductor.org',",
        "    role=c('aut', 'cre')),",
        "  person('Joe', 'Blow', email='joe@blow.com', role='cre'))",
        "License: GPL-2",
        sep = "\n",
        file=file.path(UNIT_TEST_TEMPDIR, "DESCRIPTION"))
    BiocCheck:::checkBBScompatibility(UNIT_TEST_TEMPDIR, FALSE)
    # ERROR: More than one maintainer & NOTE: Include ORCID iD
    checkIdentical(
        .BiocCheck$getNum(c("error", "warning", "note")),
        c(error = 1L, warning = 0L, note = 1L)
    )

    .zeroCounters()
    cat(paste("Package:", UNIT_TEST_PKG),
        "Version: 0.99.0",
        "Authors@R: c(",
        "  person('Bioconductor Package Maintainer',",
        "    email='maintainer@bioconductor.org',",
        "    role=c('aut', 'cre')))",
        "License: GPL-2",
        sep = "\n",
        file=file.path(UNIT_TEST_TEMPDIR, "DESCRIPTION"))
    BiocCheck:::checkBBScompatibility(UNIT_TEST_TEMPDIR, FALSE)
    checkCounter("Include ORCID iD", "note")

    .zeroCounters()
    dir.create(file.path(UNIT_TEST_TEMPDIR,"inst"))
    cat(
        paste(
            "citEntry(entry='article', title='test',",
            "author=personList(as.person('Lori Kern')), year=2020,",
            "journal='Loris Best', volume='4', issue='12',",
            "textVersion='Shepherd, Lori (2020) test. Loris Best. 4(12)')"
        ),
        file=file.path(UNIT_TEST_TEMPDIR,"inst/CITATION")
    )
    BiocCheck:::checkForCitationFile(UNIT_TEST_TEMPDIR)
    checkEqualsNumeric(.BiocCheck$getNum("warning"), 1)
    .zeroCounters()

    cat(
        paste(
            "citEntry(entry='', title='test',",
            "author=personList(as.person('Lori Kern')), year=2020,",
            "journal='Loris Best', volume='4', issue='12')"
        ),
        file=file.path(UNIT_TEST_TEMPDIR,"inst/CITATION"),
        append=FALSE
    )
    BiocCheck:::checkForCitationFile(UNIT_TEST_TEMPDIR)
    checkEqualsNumeric(.BiocCheck$getNum("warning"), 1)
    .zeroCounters()
}

test_checkUnitTests <- function()
{
    .zeroCounters()
    BiocCheck:::checkUnitTests(UNIT_TEST_TEMPDIR)
    checkTrue(.BiocCheck$getNum("note") == 1)
    dir.create(file.path(UNIT_TEST_TEMPDIR, "tests"),recursive=TRUE)
    cat("nothing", file=file.path(UNIT_TEST_TEMPDIR, "tests",
        "foo.R"))
    .zeroCounters()
    BiocCheck:::checkUnitTests(UNIT_TEST_TEMPDIR)
    checkTrue(stillZero())
    .zeroCounters()
    BiocCheck:::checkSkipOnBioc(system.file("testpackages",
        "testpkg0", package="BiocCheck"))
    checkTrue(.BiocCheck$getNum("note") == 1)
    .zeroCounters()

    # check coding practice
    pkgdir <- system.file("testpackages", "testpkg0", package="BiocCheck")
    Rdir <- file.path(pkgdir, "R")
    msg_sapply <- BiocCheck:::checkSapply(Rdir)
    checkEqualsNumeric(length(msg_sapply), 1)
    msg_seq <- BiocCheck:::check1toN(Rdir)
    checkEqualsNumeric(length(msg_seq), 1)
    msg_eda <- BiocCheck:::checkExternalData(Rdir)
    checkEqualsNumeric(length(msg_eda), 4)
    msg_dl <- BiocCheck:::checkOnAttachLoadCalls(Rdir)
    checkEqualsNumeric(length(msg_dl), 2)
    avail_pkgs <- BiocManager::available()
    msg_sc <- BiocCheck:::checkSingleColon(Rdir, avail_pkgs)
    testval <- ifelse("BiocCheck" %in% avail_pkgs, 1, 0)
    checkTrue(length(msg_sc) == testval)
    pkgdir <- system.file("testpackages", "testpkg2", package="BiocCheck")
    res <- BiocCheck:::findSymbolsInVignettes(pkgdir, c("T", "F"), "SYMBOL")
    checkEqualsNumeric(length(res), 1)
}

test_findPasteInSignaler <- function() {
    rfile <- tempfile()
    writeLines(c(
        "message(paste('foo', 'bar'))",
        "message(paste('foo', x))",
        "message(paste(x, collapse = '\t'))",
        "message('foo', paste(x, collapse = 't'))"
    ), rfile)
    checkTrue(
        length(BiocCheck:::.findPasteInSignaler(rfile)) == 2L
    )
}

test_findSignalerInSignaler <- function() {
    .SIGNALERS_TXT <- c("message", "warning", "stop")
    rfile <- tempfile()
    writeLines(c(
        "message('warning: see here')",
        "warning('error here')",
        "stop('ErrOR: see here')",
        "stop('message here')"
    ), rfile)
    checkTrue(
        length(BiocCheck:::.findSignalerInSignaler(rfile, .SIGNALERS_TXT)) == 4L
    )
}

test_installAndLoad <- function()
{
    pkgdir <- create_test_package()
    instdir <- dirname(pkgdir)
    temppkg <- BiocCheck:::installAndLoad(
        pkgpath = pkgdir, install_dir = instdir
    )
    liblocation <- file.path(temppkg, "lib")
    checkTrue(dir.exists(liblocation))
    checkTrue(
        identical(
            readLines(file.path(instdir, "install.stderr")),
            character(0L)
        )
    )
    testloadEnv <- try(loadNamespace(basename(pkgdir), lib.loc = liblocation))
    checkTrue(is.environment(testloadEnv))
    unloadNamespace(testloadEnv)
    unlink(instdir, recursive = TRUE)
}

test_findPackageName <- function()
{
    pkgdir <- create_test_package()
    dirrename <- file.path(dirname(pkgdir), "test_package")
    file.rename(pkgdir, dirrename)
    pkgname <- BiocCheck:::.getPackageName(dirrename)
    checkTrue(identical(pkgname, basename(pkgdir)))
    unlink(dirname(pkgdir), recursive = TRUE)

    ## test tarball rename
    pkgdir <- create_test_package(description = list(Version = "0.99.0"))
    oldname <- devtools::build(pkgdir)
    newname <- file.path(tempdir(), "test.package_0.99.0.tar.gz")

    if (!file.rename(oldname, newname)) {
        file.remove(oldname)
        stop("'file.rename()' failed to rename package",
             "\n  oldname: ", oldname, " newname: ", newname,
             "\n  cmd: ", cmd,
             "\n  result:",
             "\n    ", paste(result, collapse="\n    "),
             "\n")
    }
    on.exit({
        file.remove(newname)
    })
    checkException(BiocCheck:::.getPackageName(newname))
}

test_checkDeprecatedPackages <- function()
{
    cat(sprintf("Depends: multicore, %s", UNIT_TEST_PKG),
        file=file.path(UNIT_TEST_TEMPDIR, "DESCRIPTION"))
    BiocCheck:::checkDeprecatedPackages(UNIT_TEST_TEMPDIR)
    checkError("Depending on multicore didn't cause error!")

}

test_parseFile <- function()
{
    testFile <- file.path(tempdir(), "testfile.R")
    cat("1 + 1", file=testFile)
    df <- BiocCheck:::parseFile(testFile, "BiocCheck")
    checkTrue(all(dim(df) == c(6,9)))
    ## test that  testpkg0_child.Rmd is read in using `child =` chunk
    ## in testpkg0.Rmd
    pkgdir <- system.file("testpackages", "testpkg0", package="BiocCheck")
    testFile <- file.path(pkgdir, "vignettes", "testpkg0.Rmd")
    incl <- BiocCheck:::parseFile(testFile, pkgdir)
    checkTrue(all(c("2", "+", "1") %in% incl[, "text"]))
}

test_checkForBrowser <- function()
{
    if (is.null(parsedCode))
        parsedCode <- BiocCheck:::parseFiles(system.file("testpackages",
            "testpkg0", package="BiocCheck"))
    res <- BiocCheck:::findSymbolsInParsedCode(
        parsedCodeList = parsedCode,
        symbolNames = "browser",
        tokenTypes = "SYMBOL_FUNCTION_CALL"
    )
    checkTrue(length(res) == 1)
}

test_checkVigSessionInfo <- function()
{
    pkgdir <- system.file("testpackages", "testpkg0", package="BiocCheck")
    checkVigSessionInfo(pkgdir)
    checkTrue(.BiocCheck$getNum("note") == 1)
}

test_findSymbolsInRFiles <- function()
{
    pkgdir <- system.file("testpackages", "testpkg0",
        package="BiocCheck", mustWork = TRUE)
    msg <- BiocCheck:::findSymbolsInRFiles(
        pkgdir, BiocCheck:::.BAD_INSTALL_CALLS, "SYMBOL_FUNCTION_CALL"
    )
    checkTrue(length(msg) == 2)
}

test_checkCatInRCode <- function()
{
    Rdir <- system.file("testpackages", "testpkg0", "R",
        package="BiocCheck", mustWork = TRUE)
    msg <- BiocCheck:::checkCatInRCode(
        Rdir, c("cat", "print")
    )
    checkTrue(length(msg) == 9)
}

test_checkDepDefInRCode <- function() {
    pkgdir <- system.file("testpackages", "testpkg0",
        package="BiocCheck", mustWork = TRUE)
    msg <- BiocCheck:::findSymbolsInRFiles(
        pkgdir, c(".Deprecated", ".Defunct"), "SYMBOL_FUNCTION_CALL"
    )
    checkTrue(
        identical(length(msg), 2L)
    )
}

test_checkEqInAssignment <- function()
{
    Rdir <- system.file("testpackages", "testpkg0", "R",
        package="BiocCheck", mustWork = TRUE)
    msg <- BiocCheck:::checkEqInAssignment(
        Rdir = Rdir, symbol = "=", tokenType = "EQ_ASSIGN"
    )
    checkTrue(length(msg) == 3)
}

test_checkVigInstalls <- function()
{
    BiocCheck:::checkVigInstalls(
        system.file(
            "testpackages", "testpkg0", package="BiocCheck", mustWork = TRUE
        )
    )
    checkEqualsNumeric(.BiocCheck$getNum("error"), 1)
    checkEqualsNumeric(length(.BiocCheck$get("error")[[1]]), 5)
    .zeroCounters()
}

test_checkDupChunkLabels <- function() {
    BiocCheck:::checkDupChunkLabels(
        system.file(
            "testpackages", "testpkg0", "vignettes", "dupChunks.Rmd",
            package="BiocCheck", mustWork = TRUE
        )
    )
    checkEqualsNumeric(.BiocCheck$getNum("error"), 1)
    ## check length of warning / 1 file affected plus 2 info messages
    checkEqualsNumeric(length(.BiocCheck$get("error")[[1]]), 3)
    .zeroCounters()
}

test_checkTFSymbolUsage <- function()
{
    BiocCheck:::checkTFSymbolUsage(
        system.file("testpackages", "testpkg0", package = "BiocCheck",
            mustWork = TRUE)
    )
    checkEqualsNumeric(.BiocCheck$getNum("warning"), 1)
}

test_checkVigSessionInfo <- function()
{
    BiocCheck:::checkVigSessionInfo(
        system.file("testpackages", "testpkg0", package = "BiocCheck",
            mustWork = TRUE)
    )
    checkEqualsNumeric(.BiocCheck$getNum("note"), 1)
}

test_checkForInstall <- function()
{
    if (is.null(parsedCode))
        parsedCode <- BiocCheck:::parseFiles(system.file("testpackages",
            "testpkg0", package="BiocCheck"))
    res <- BiocCheck:::findSymbolInParsedCode(parsedCode, "testpkg0", "install",
        "SYMBOL_FUNCTION_CALL")
    checkTrue(res == 2)
}

test_checkVigBiocInst <- function()
{
    pkgdir <- system.file("testpackages", "testpkg0", package = "BiocCheck")
    BiocCheck:::checkVigBiocInst(pkgdir)
    checkTrue(.BiocCheck$getNum("warning") == 1)
}

test_checkClassNEEQLookup <- function()
{
    dir.create(dir <- tempfile())
    dir.create(Rdir <- file.path(dir, "R"))
    fl <- tempfile(tmpdir=Rdir, fileext = ".R")
    cat(
        paste(
            ## bad
            "class(a)=='foo'", "class(a)!='foo'",
            "if (is(a) == 'character')", "is( a ) != 'numeric'",
            ## ok
            "is(a, 'List')", "is.numeric(a)",
            sep="\n"),
        "\n", file = fl
    )
    match <- BiocCheck:::checkClassNEEQLookup(dir)
    checkIdentical(4L, length(match))
}

test_checkDESCRIPTIONfile <- function()
{
    dcf <- matrix("https://example.com", dimnames = list(NULL, "URL"))
    BiocCheck:::.checkDESCfields(dcf)
    checkEqualsNumeric(.BiocCheck$getNum("note"), 1)
    .zeroCounters()

    dcf <- matrix("https://example.com", dimnames = list(NULL, "BugReports"))
    BiocCheck:::.checkDESCfields(dcf)
    checkEqualsNumeric(.BiocCheck$getNum("note"), 1)
    .zeroCounters()

    dcf <- matrix(
        c("https://example.com", "https://example.com"), nrow = 1,
        dimnames = list(NULL, c("BugReports", "URL"))
    )
    BiocCheck:::.checkDESCfields(dcf)
    checkEqualsNumeric(.BiocCheck$getNum("note"), 0)
    .zeroCounters()

    dcf <- matrix(
        c("https://example.com", "https://example.com"), nrow = 1,
        dimnames = list(NULL, c("BugReports", "URL"))
    )
    BiocCheck:::.checkBiocDepsDESC(dcf)
    checkEqualsNumeric(.BiocCheck$getNum("warning"), 1)
    .zeroCounters()

    dcf <- matrix(
        "S4Vectors (== 0.99.0)", nrow = 1,
        dimnames = list(NULL, "Depends")
    )
    BiocCheck:::.checkBiocDepsDESC(dcf)
    checkEqualsNumeric(.BiocCheck$getNum("warning"), 0)
    .zeroCounters()

    dcf <- matrix(
        "S4Vectors (== 0.99.0)", nrow = 1,
        dimnames = list(NULL, "Depends")
    )
    BiocCheck:::.checkPinnedDeps(dcf)
    checkEqualsNumeric(.BiocCheck$getNum("error"), 1)
    .zeroCounters()
}

test_remotesUsage <- function()
{
    pkg <- system.file("testpackages", "testpkg0", package="BiocCheck")
    .zeroCounters()
    BiocCheck:::checkRemotesUsage(pkg)
    checkEqualsNumeric(1, BiocCheck:::.BiocCheck$getNum("error"))
    checkTrue(grepl("Remotes:", BiocCheck:::.BiocCheck$get("error")[1]))
    .zeroCounters()
    pkg <- system.file("testpackages", "testpkg1", package="BiocCheck")
    BiocCheck:::checkRemotesUsage(pkg)
    checkEqualsNumeric(0, BiocCheck:::.BiocCheck$getNum("error"))
}

test_LazyDataUsage <- function()
{
    pkg <- system.file("testpackages", "testpkg0", package="BiocCheck")
    .zeroCounters()
    BiocCheck:::checkLazyDataUsage(pkg)
    checkEqualsNumeric(1, BiocCheck:::.BiocCheck$getNum("note"))
}

test_checkForLibraryRequire <- function()
{
    pkg_dir <- system.file(
        "testpackages", "testpkg0", package="BiocCheck", mustWork = TRUE
    )
    msg <- BiocCheck:::checkForLibraryRequire(pkg_dir)
    checkEqualsNumeric(1L, .BiocCheck$getNum("warning"))
    checkEqualsNumeric(length(msg), 14L)
    .zeroCounters()
}

test_getFunctionLengths <- function()
{
    file <- system.file("testpackages", "testpkg0", "R",
        "parseme.R", package="BiocCheck")
    df <- getParseData(parse(file, keep.source=TRUE))
    res <- BiocCheck:::getFunctionLengths(df)
    lsnames <- c("length", "startLine", "endLine")
    values <- c(2, 1, 2, 1, 3, 3, 1, 6, 6, 5, 9, 13, 4, 16,
        19, 6, 23, 28, 1, 31, 31, 1, 33, 33, 6, 35, 40)
    names(values) <- rep(lsnames, 9)
    expected <- split(values, rep(1:9, each = 3))
    names(expected) <- c("_anonymous_.1", "fa", "f2", "f3", "f4",
        "_anonymous_.23", "f5", "f6", "f7")

    checkTrue(all.equal(expected, res))
}

test_getFunctionLengths2 <- function()
{
    load(system.file("unitTests", "IRangesParsedCode.rda", package="BiocCheck"))
    BiocCheck:::checkFunctionLengths(IRangesParsedCode, "IRanges")
    .zeroCounters()

    ## we should find 1 function that is greater than 50 lines long
    parsedCode <- BiocCheck:::parseFiles(system.file("testpackages", "testpkg0",
                                                     package="BiocCheck"))
    res <- BiocCheck:::checkFunctionLengths(parsedCode, "testpkg0")
    checkEqualsNumeric(BiocCheck:::.BiocCheck$getNum("note"), 1)
    checkTrue(grepl(pattern = "There is 1 function greater than 50 lines",
                    x = BiocCheck:::.BiocCheck$note$checkFunctionLengths[[1]]),
              msg = "Checking we report functions > 50 lines long.")
    .zeroCounters()
}

test_checkExportsAreDocumented <- function()
{
    pkgdir <- system.file("testpackages", "testpkg0", package="BiocCheck")
    instdir <- BiocCheck:::installAndLoad(pkgdir)
    res <- BiocCheck:::checkExportsAreDocumented(
        pkgdir, "testpkg0", lib.loc = file.path(instdir, "lib")
    )
    checkEqualsNumeric(1, .BiocCheck$getNum("error"))
    .zeroCounters()
    res <- BiocCheck:::checkUsageOfDont(pkgdir)
    checkEqualsNumeric(2, .BiocCheck$getNum("note"))
    .zeroCounters()
}

test_checkNEWS <- function()
{
    BiocCheck:::checkNEWS(system.file("testpackages", "testpkg0",
        package="BiocCheck"))
    checkEqualsNumeric(1, .BiocCheck$getNum("note"))
    .zeroCounters()
    if (!dir.exists(UNIT_TEST_TEMPDIR))
        dir.create(UNIT_TEST_TEMPDIR)
    cat("lalala", file=file.path(UNIT_TEST_TEMPDIR, "NEWS"))
    BiocCheck:::checkNEWS(UNIT_TEST_TEMPDIR)
    stillZero()
    unlink(file.path(UNIT_TEST_TEMPDIR, "NEWS"))
    dir.create(file.path(UNIT_TEST_TEMPDIR, "inst"), FALSE)
    cat("lalala", file=file.path(UNIT_TEST_TEMPDIR, "inst", "NEWS.Rd"))
    BiocCheck:::checkNEWS(UNIT_TEST_TEMPDIR)
    checkEqualsNumeric(1, .BiocCheck$getNum("warning"))
    .zeroCounters()
    cat("lalala", file=file.path(UNIT_TEST_TEMPDIR, "NEWS.md"))
    BiocCheck:::checkNEWS(UNIT_TEST_TEMPDIR)
    checkEqualsNumeric(1, .BiocCheck$getNum("note"))
    checkEqualsNumeric(1, .BiocCheck$getNum("warning"))
    .zeroCounters()
}

test_checkFormatting <- function()
{
    BiocCheck:::checkFormatting(system.file("testpackages", "testpkg0",
        package="BiocCheck"))
    checkEqualsNumeric(3, .BiocCheck$getNum("note"))
}

test_checkForPromptComments <- function()
{
    BiocCheck:::checkForPromptComments(system.file("testpackages", "testpkg0",
        package="BiocCheck"))
    checkEqualsNumeric(1, .BiocCheck$getNum("note"))

}

test_getPkgType <- function()
{
   cat("Foo: bar", file=file.path(UNIT_TEST_TEMPDIR, "DESCRIPTION"))
   checkEquals(NA, BiocCheck:::getPkgType(UNIT_TEST_TEMPDIR))

   cat("biocViews: bad, Software",
       file=file.path(UNIT_TEST_TEMPDIR, "DESCRIPTION"))
   checkEquals(NA, BiocCheck:::getPkgType(UNIT_TEST_TEMPDIR))

   cat("biocViews: DifferentialExpression, CellBiology",
       file=file.path(UNIT_TEST_TEMPDIR, "DESCRIPTION"))
   checkEquals("Software", BiocCheck:::getPkgType(UNIT_TEST_TEMPDIR))

   cat("biocViews: DifferentialExpression, ChipManufacturer",
       file=file.path(UNIT_TEST_TEMPDIR, "DESCRIPTION"))
   checkEquals(NA, BiocCheck:::getPkgType(UNIT_TEST_TEMPDIR))

   cat("biocViews: GeneCardsCustomSchema, ChipManufacturer",
       file=file.path(UNIT_TEST_TEMPDIR, "DESCRIPTION"))
   checkEquals("AnnotationData", BiocCheck:::getPkgType(UNIT_TEST_TEMPDIR))

   # Cancer is not a valid biocView, so return NA
   cat("biocViews: Cancer, HapMap",
       file=file.path(UNIT_TEST_TEMPDIR, "DESCRIPTION"))
   checkEquals(NA, BiocCheck:::getPkgType(UNIT_TEST_TEMPDIR))
}

test_checkForBiocDevelSubscription <- function()
{
    if (nchar(Sys.getenv("BIOC_DEVEL_PASSWORD"))) {

        if (!dir.exists(UNIT_TEST_TEMPDIR))
            dir.create(UNIT_TEST_TEMPDIR)
        cat("Maintainer: Joe Blow <foo@bar.com>",
                file=file.path(UNIT_TEST_TEMPDIR, "DESCRIPTION"))
        BiocCheck:::checkForBiocDevelSubscription(UNIT_TEST_TEMPDIR)
        checkEqualsNumeric(.BiocCheck$getNum("error"), 1)
        .zeroCounters()

        cat("Maintainer: Bioconductor Maintainer <maintainer@bioconductor.org>",
                file=file.path(UNIT_TEST_TEMPDIR, "DESCRIPTION"))
        BiocCheck:::checkForBiocDevelSubscription(UNIT_TEST_TEMPDIR)
        checkTrue(stillZero())
        .zeroCounters()

        cat("Maintainer: Bioconductor Maintainer <MAINTAINER@bioconductor.ORG>",
                file=file.path(UNIT_TEST_TEMPDIR, "DESCRIPTION"))
        BiocCheck:::checkForBiocDevelSubscription(UNIT_TEST_TEMPDIR)
        checkTrue(stillZero())
        .zeroCounters()

        result_email <- BiocCheck:::getMaintainerEmail(UNIT_TEST_TEMPDIR)
        checkTrue(
            identical(result_email, "MAINTAINER@bioconductor.ORG")
        )

        writeLines(c(
            "Package: uniTestTempDir",
            "Version: 0.99.0",
            paste0(
                "Authors@R: person('BioC', 'Maintainer', ,",
                " email = 'Maintainer@bioconductor.org',",
                "  role = c('aut', 'cre'))"
            )
        ), con = file.path(UNIT_TEST_TEMPDIR, "DESCRIPTION"))
        result_email <- BiocCheck:::getMaintainerEmail(UNIT_TEST_TEMPDIR)
        checkTrue(
            identical(result_email, "Maintainer@bioconductor.org")
        )

        cat(sprintf(paste(
            "Package: %s\nVersion: 0.99.0\nAuthors@R:",
            "c(person('Joe', \n  'Blow', email='joe@blow.org',",
            "role=c('aut', 'cre')))"
        ), UNIT_TEST_PKG), file=file.path(UNIT_TEST_TEMPDIR, "DESCRIPTION"))
        BiocCheck:::checkForBiocDevelSubscription(UNIT_TEST_TEMPDIR)
        checkEqualsNumeric(.BiocCheck$getNum("error"), 1)
        .zeroCounters()

        cat(sprintf(paste(
            "Package: %s\nVersion: 0.99.0\nAuthors@R:",
            "c(person('BioC', \n  'Maintainer',",
            "email='maintainer@bioconductor.org',",
            "role=c('aut', 'cre')))"
        ), UNIT_TEST_PKG), file=file.path(UNIT_TEST_TEMPDIR, "DESCRIPTION"))
        BiocCheck:::checkForBiocDevelSubscription(UNIT_TEST_TEMPDIR)
        checkTrue(stillZero())
        .zeroCounters()

    }
}

test_checkForSupportSiteRegistration <- function()
{
    connect <- suppressWarnings(
        tryCatch({
            readBin("https://support.bioconductor.org", n=1L, what="raw")
            TRUE
        }, error = function(...) {
            FALSE
        })
    )

    if (connect) {

        # Email registration
        .zeroCounters()
        BiocCheck:::checkSupportReg("lori.shepherd@roswellpark.org")
        checkTrue(stillZero())
        BiocCheck:::checkSupportReg("foo@bar.com")
        checkEqualsNumeric(.BiocCheck$getNum("error"), 1)
        .zeroCounters()
        ## api is case insensitivy
        BiocCheck:::checkSupportReg("lori.shePhErd@roswellpark.org")
        checkTrue(stillZero())


        # tags
        BiocCheck:::checkWatchedTag(
            "lori.shepherd@roswellpark.org", "biocfilecache"
        )
        checkTrue(stillZero())
        BiocCheck:::checkWatchedTag(
            "lori.shepherd@roswellpark.org", "unwatchedpackage"
        )
        checkEqualsNumeric(.BiocCheck$getNum("error"), 1)
        .zeroCounters()
        ## email is case insensitive
        BiocCheck:::checkWatchedTag(
            "lori.shePherd@rosWellpark.org", "biocfilecache"
        )
        checkTrue(stillZero())
        ## check tag is case insenstive
        BiocCheck:::checkWatchedTag(
            "lori.shepherd@rosWellpark.org", "bioCfiLecache"
        )
        checkTrue(stillZero())

    }
}

test_checkForVersionNumberMismatch <- function()
{
    pkgpath <- create_test_package(description = list(Version="0.0.1"))
    pkgname <- basename(pkgpath)

    oldname <- devtools::build(pkgpath)

    newname <- file.path(dirname(oldname), paste0(pkgname, "_9.9.9.tar.gz"))
    if (!file.rename(oldname, newname)) {
        file.remove(oldname)
        stop("'file.rename()' failed to rename badkpgk",
             "\n  oldname: ", oldname, " newname: ", newname,
             "\n  cmd: ", cmd,
             "\n  result:",
             "\n    ", paste(result, collapse="\n    "),
             "\n")
    }
    on.exit({
        file.remove(newname)
    })
    inst_dir <- BiocCheck:::installAndLoad(newname)

    BiocCheck:::checkForVersionNumberMismatch(
        newname,
        BiocCheck:::.tempPackageDirTarball(newname))
    checkEqualsNumeric(.BiocCheck$getNum("error"), 1)
    .zeroCounters()
}

test_checkForDirectSlotAccess <- function()
{
    pkgpath <- create_test_package(description = list(VignetteBuilder="knitr"))
    parsedCode <- list(FooBar=BiocCheck:::parseFile(
        system.file("testfiles", "directSlotAccess.Rmd",
        package="BiocCheck"), pkgpath))
    res <- BiocCheck:::checkForDirectSlotAccess(parsedCode, pkgpath)
    checkEqualsNumeric(BiocCheck:::.BiocCheck$getNum("note"), 1)
    .zeroCounters()
    parsedCode <- list(FooBar=BiocCheck:::parseFile(
        system.file("testfiles", "noDirectSlotAccess.Rmd",
        package="BiocCheck"), pkgpath))
    res <- BiocCheck:::checkForDirectSlotAccess(parsedCode, pkgpath)
    checkEqualsNumeric(.BiocCheck$getNum("note"), 0)
    .zeroCounters()
}

test_checkRVersionDependency <- function()
{
    .zeroCounters()
    desc <- file.path(tempdir(), "DESCRIPTION")
    cat("Depends: R (>= 1.0.0)", file=desc)
    BiocCheck:::checkRVersionDependency(dirname(desc))
    checkEqualsNumeric(.BiocCheck$getNum("note"), 1)
    .zeroCounters()

    cat("Depends: R", file=desc)
    BiocCheck:::checkRVersionDependency(dirname(desc))
    checkEqualsNumeric(.BiocCheck$getNum("note"), 0)
    .zeroCounters()

    cat("Imports: foobar)", file=desc)
    BiocCheck:::checkRVersionDependency(dirname(desc))
    checkEqualsNumeric(.BiocCheck$getNum("warning"), 0)
    .zeroCounters()

    cat("Depends: R (>= 10000.0.0)", file=desc) # this test might fail some day!
    BiocCheck:::checkRVersionDependency(dirname(desc))
    checkEqualsNumeric(.BiocCheck$getNum("note"), 0)
    .zeroCounters()

    unlink(desc)
}

test_doesManPageHaveRunnableExample <- function()
{
    good <- tools::parse_Rd(system.file("testpackages", "testpkg0", "man",
            "has-devel.Rd", package = "BiocCheck"))

    bad <- tools::parse_Rd(system.file("testpackages", "testpkg0", "man",
            "baddep.Rd", package = "BiocCheck"))

    checkEquals(BiocCheck:::doesManPageHaveRunnableExample(good), TRUE)

    checkEquals(BiocCheck:::doesManPageHaveRunnableExample(bad), FALSE)
}

test_checkForValueSection <- function() {
    pkgdir <- system.file(
        "testpackages", "testpkg0", package = "BiocCheck"
    )
    mans <- BiocCheck:::.read_all_rds(pkgdir)
    tags <- lapply(mans, tools:::RdTags)
    checkTrue(!BiocCheck:::.valueInParsedRd(mans[[1]], tags[[1]]))
    checkTrue(!BiocCheck:::.valueInParsedRd(mans[[2]], tags[[2]]))
    checkTrue(BiocCheck:::.valueInParsedRd(mans[[3]], tags[[3]]))
    pkgdir <- system.file(
        "testpackages", "testpkg1", package = "BiocCheck"
    )
    mans <- BiocCheck:::.read_all_rds(pkgdir)
    tags <- lapply(mans, tools:::RdTags)
    checkTrue(!BiocCheck:::.valueInParsedRd(mans[[1]], tags[[1]]))
    checkTrue(!BiocCheck:::.valueInParsedRd(mans[[2]], tags[[2]]))
}

test_packageAlreadyExists <- function()
{
    .zeroCounters()
    nerrors <- 0L
    BiocCheck:::checkIsPackageNameAlreadyInUse("GenomicRanges", "CRAN")
    checkEqualsNumeric(.BiocCheck$getNum("error"), nerrors)
    BiocCheck:::checkIsPackageNameAlreadyInUse("devtools", "CRAN")
    checkEqualsNumeric(.BiocCheck$getNum("error"), nerrors)
    BiocCheck:::checkIsPackageNameAlreadyInUse("GenomicRanges", "BioCsoft")
    nerrors <- nerrors + 1L
    checkEqualsNumeric(.BiocCheck$getNum("error"), nerrors)
    BiocCheck:::checkIsPackageNameAlreadyInUse("gwascatData", "BioCann")
    nerrors <- nerrors + 1L
    checkEqualsNumeric(.BiocCheck$getNum("error"), nerrors)
    BiocCheck:::checkIsPackageNameAlreadyInUse("TENxBrainData", "BioCexp")
    nerrors <- nerrors + 1L
    checkEqualsNumeric(.BiocCheck$getNum("error"), nerrors)
    BiocCheck:::checkIsPackageNameAlreadyInUse("annotation", "BioCworkflows")
    nerrors <- nerrors + 1L
    checkEqualsNumeric(.BiocCheck$getNum("error"), nerrors)
    BiocCheck:::checkIsPackageNameAlreadyInUse("GenomicRanges", "BioCexp")
    checkEqualsNumeric(.BiocCheck$getNum("error"), nerrors)
    BiocCheck:::checkIsPackageNameAlreadyInUse("GenomicRanges", "BioCann")
    checkEqualsNumeric(.BiocCheck$getNum("error"), nerrors)
    BiocCheck:::checkIsPackageNameAlreadyInUse("GenomicRanges", "BioCworkflows")
    checkEqualsNumeric(.BiocCheck$getNum("error"), nerrors)
    BiocCheck:::checkIsPackageNameAlreadyInUse("ImNotFound", "BioCexp")
    checkEqualsNumeric(.BiocCheck$getNum("error"), nerrors)
    BiocCheck:::checkIsPackageNameAlreadyInUse("ImNotFound", "BioCann")
    checkEqualsNumeric(.BiocCheck$getNum("error"), nerrors)
    BiocCheck:::checkIsPackageNameAlreadyInUse("ImNotFound", "BioCworkflows")
    checkEqualsNumeric(.BiocCheck$getNum("error"), nerrors)
    BiocCheck:::checkIsPackageNameAlreadyInUse("ImNotFound", "CRAN")
    checkEqualsNumeric(.BiocCheck$getNum("error"), nerrors)
    BiocCheck:::checkIsPackageNameAlreadyInUse("ImNotFound", "BioCsoft")
    checkEqualsNumeric(.BiocCheck$getNum("error"), nerrors)
    .zeroCounters()
}

test_BiocCheckReporters <- function()
{
    pkgdir <- system.file("testpackages", package="BiocCheck")
    hypo_checkdir <- file.path(pkgdir, "hypoPkg.BiocCheck")
    oldCheckDir <- .BiocCheck$metadata$BiocCheckDir
    on.exit({
        .BiocCheck$metadata$BiocCheckDir <- oldCheckDir
    })
    .BiocCheck$metadata$BiocCheckDir <- hypo_checkdir
    .BiocCheck$report(debug = FALSE, isOnBBS = TRUE)
    checkTrue(
        !dir.exists(hypo_checkdir)
    )
    checkTrue(
        !dir.exists(hypo_checkdir)
    )
}

test_checkUsageOfDont <- function()
{
    ## testpkg0 should trigger this note for 2 out of 3 man pages
    pkgdir <- system.file("testpackages", "testpkg0", package="BiocCheck")
    BiocCheck:::installAndLoad(pkgdir)
    notemsg <- capture.output(BiocCheck:::checkUsageOfDont(pkgdir),
                              type = "message")
    checkEqualsNumeric(2, .BiocCheck$getNum("note"))
    # here we verify the correct number of pages were detected
    checkTrue( any(grepl("67%", notemsg)) )
    .zeroCounters()

    ## testpkg1 contains a man page with keyword 'internal'
    ## this shouldn't trigger the note
    pkgdir <- system.file("testpackages", "testpkg1", package="BiocCheck")
    BiocCheck:::installAndLoad(pkgdir)
    BiocCheck:::checkUsageOfDont(pkgdir)
    checkEqualsNumeric(0, .BiocCheck$getNum("note"))
    .zeroCounters()
}

test_IsOrcidIdValid <- function()
{
    orcid <- c(
        "0000-0001-6197-3471",
        "0000-0001-6197-347X",
        "0000-0001-6197-34XX",
        "0000-0001-6197-3471-0000",
        "",
        NA_character_
    )
    valid <- c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE)
    checkIdentical(valid, BiocCheck:::.checkORCID(orcid))
}

test_getBiocCheckDir <- function() {
    getBiocCheckDir <- BiocCheck:::.getBiocCheckDir
    pkg <- system.file(
        "testpackages", "testpkg0", package="BiocCheck"
    )
    checkIdentical(
        getBiocCheckDir(basename(pkg), dirname(pkg)),
        file.path(dirname(pkg), paste(basename(pkg), "BiocCheck", sep = "."))
    )
}

test_getDirFiles <- function() {
    vigfiles <- list.files(
        system.file(
            "testpackages", "testpkg0", "vignettes", package="BiocCheck"
        ),
        full.names = TRUE
    )
    targets <- file.path(basename(dirname(vigfiles)), basename(vigfiles))
    checkIdentical(
        targets[1], unname(BiocCheck:::.getDirFiles(vigfiles[1]))
    )
    checkIdentical(targets, unname(BiocCheck:::.getDirFiles(vigfiles)))
}


test_checkEnv <- function() {
    # Simple function in empty environment:
    env <- new.env(parent = emptyenv())
    env$f <- function() BiocCheck::BiocCheck
    dcolon <- BiocCheck:::.checkEnv(env, BiocCheck:::.colonWalker())$done()
    checkIdentical(dcolon, "BiocCheck")

    # Function inside environment inside environment
    # This happens on pkg ns environments with S4 methods
    env <- new.env(parent = emptyenv())
    env$another <- new.env(parent = emptyenv())
    env$another$f <- function() BiocCheck::BiocCheck
    dcolon <- BiocCheck:::.checkEnv(env, BiocCheck:::.colonWalker())$done()
    checkIdentical(dcolon, "BiocCheck")

    # Environments can contain themselves. Check that this does not
    # lead to an infinite loop
    env <- new.env(parent = emptyenv())
    env$another <- env
    env$another$f <- function() BiocCheck::BiocCheck
    dcolon <- BiocCheck:::.checkEnv(env, BiocCheck:::.colonWalker())$done()
    checkIdentical(dcolon, "BiocCheck")

    # Environments can contain other envs that can contain the first env.
    # Check that this does not lead to an infinite loop
    env <- new.env(parent = emptyenv())
    env$another <- new.env(parent = emptyenv())
    env$another$env2 <- env
    env$another$env2$f <- function() BiocCheck::BiocCheck
    dcolon <- BiocCheck:::.checkEnv(env, BiocCheck:::.colonWalker())$done()
    checkIdentical(dcolon, "BiocCheck")
}


