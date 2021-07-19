UNIT_TEST_PKG <- "unitTestTempDir"
UNIT_TEST_TEMPDIR <- file.path(tempdir(), UNIT_TEST_PKG)

library(devtools)

.printf <- BiocCheck:::.printf
.zeroCounters <- BiocCheck:::.zeroCounters

parsedCode <- NULL

inspect <- function()
{
    .printf("error: %s, warning: %s, note: %s",
        .error$getNum(),
        .warning$getNum(),
        .note$getNum())
    print("error:")
    print(.error$get())
    print("warning:")
    print(.warning$get())
    print("note:")
    print(.note$get())

}

create_test_package <- function(pkgpath, description=list(),
    extraActions=function(path=NULL){})
{
    canned <- list(Author="Test Author",
        Maintainer="Test Maintainer <test@test.com>", "Authors@R"=NULL)
    for (name in names(description))
    {
        canned[[name]] <- description[[name]]
    }
    path <- file.path(tempdir(), pkgpath)
    unlink(path, recursive=TRUE)
    capture.output({
        suppressMessages(usethis::create_package(path, canned, rstudio=FALSE, open = FALSE))
    })

    cat("#", file=file.path(path, "NAMESPACE"))
    extraActions(path)
    path
}

checkError <- function(msg)
{
    if (missing(msg)) msg = ""
    # .printf("Errors: %s, Warnings: %s, Notes: %s",
    #     .error$getNum(),
    #     .warning$getNum(),
    #     .note$getNum())
    checkTrue(
        .note$getNum() == 0 &&
        .warning$getNum() == 0 &&
        .error$getNum() == 1,
        msg
    )
    .zeroCounters()
}

checkCounter <- function(msg, type = "Error") {
    if (missing(msg))
        stop("<internal> Provide message input")
    res <- as.integer(c("Note", "Warning", "Error") %in% type)
    checkTrue(
        all(
            mapply(
                all.equal,
                c(.note$getNum(), .warning$getNum(), .error$getNum()), res
            )
        ),
        msg
    )
    .zeroCounters()
}

stillZero <- function()
{
    .note$getNum() == 0 &&
        .warning$getNum() == 0 &&
        .error$getNum() == 0
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
    BiocCheck:::checkVigDirExists(pkgdir, vigdir)
    checkError("Missing vignettes dir")

    # empty vignette dir ERROR
    dir.create(vigdir, recursive=TRUE)
    BiocCheck:::checkVignetteDir(pkgdir, TRUE)
    checkError("Empty vignettes")

    # vig dir w/ source file  WARNING
    cat("nothing", file=file.path(vigdir, "test.Rnw"))
    cat("Title: unitTestTempDir", file=file.path(pkgdir, "DESCRIPTION"))
    BiocCheck:::checkVignetteDir(pkgdir, TRUE)
    checkTrue(.error$getNum() == 0
        && .warning$getNum() == 1
        && .note$getNum() == 1,
        "expected 1 warning and 1 note")
    .zeroCounters()

    # test OK
    cat("% \\VignetteIndexEntry{header} \n% \\VignetteEngine{knitr} \nnothing", file=file.path(vigdir, "test.Rnw"))
    cat("Title: unitTestTempDir\nSuggests: knitr", file=file.path(pkgdir, "DESCRIPTION"))
    BiocCheck:::checkVignetteDir(pkgdir, TRUE)
    checkTrue(.error$getNum() == 0
        && .warning$getNum() == 0
        && .note$getNum() == 1,
        "expected 1 note and 0 errors/warnings")
    .zeroCounters()


    # check rnw file in inst/doc  WARNING
    instdoc <- file.path(pkgdir, "inst", "doc")
    dir.create(instdoc, recursive=TRUE)
    cat("nothing", file=file.path(instdoc, "test.rnw"))
    BiocCheck:::checkInstContents(pkgdir, TRUE)
    checkTrue(.error$getNum() == 0
        && .warning$getNum() == 1
        && .note$getNum() == 0,
        "expected 1 warning, no notes or errors")
    .zeroCounters()

    # check rmd file in inst/doc  WARNING
    unlink(instdoc, TRUE)
    dir.create(instdoc, recursive=TRUE)
    cat("nothing", file=file.path(instdoc, "test.Rmd"))
    BiocCheck:::checkInstContents(pkgdir, TRUE)
    checkTrue(.warning$getNum() == 1,
        "Rmd file not seen as valid vignette source")
    unlink(file.path(instdoc, "test.Rmd"))
    .zeroCounters()


    # check vigbuilder ERROR
    # in Description but not any vignette
    # also checks if builder listed but not in DESCRIPTION import,depend,suggest
    cat("VignetteBuilder: knitr", file=file.path(pkgdir, "DESCRIPTION"))
    cat("% \\VignetteIndexEntry{header} \nnnothing", file=file.path(vigdir, "test.Rnw"))
    BiocCheck:::checkVignetteDir(pkgdir, TRUE)
    checkTrue(.error$getNum() == 1)
    checkTrue(.warning$getNum() == 1)
    .zeroCounters()

    # check vignette builder default
    # vignette directory currently contains rnw
    unlink(file.path(pkgdir, "DESCRIPTION"))
    cat("Title: something", file=file.path(pkgdir, "DESCRIPTION"))
    BiocCheck:::checkVignetteDir(pkgdir, TRUE)
    checkTrue(.error$getNum() == 0)
    # check defined in desc but default vig
    unlink(file.path(pkgdir, "DESCRIPTION"))
    cat("VignetteBuilder: Sweave", file=file.path(pkgdir, "DESCRIPTION"))
    BiocCheck:::checkVignetteDir(pkgdir, TRUE)
    checkTrue(.error$getNum() == 0)
    .zeroCounters()

    # check vignette style of example package
    # 2 WARNINGS - vignette template and evaluate more chunks
    BiocCheck:::checkVignetteDir(system.file("testpackages",
        "testpkg0", package="BiocCheck"), TRUE)
    checkEquals(6, .warning$getNum())
    checkEquals("Evaluate more vignette chunks.",
        .warning$get()[5])
    checkTrue(grepl(pattern="VignetteIndex",  .warning$get()[3]))
    .zeroCounters()

    # check vignette style of example package
    BiocCheck:::checkVignetteDir(system.file("testpackages",
        "testpkg2", package="BiocCheck"), TRUE)
    checkEquals(2, .error$getNum())
    checkTrue(grepl(pattern="VignetteBuilder",  .error$get()[1]))
    checkTrue(grepl(pattern="VignetteEngine",  .error$get()[2]))
    checkEquals(3, .warning$getNum())
    checkTrue(grepl(pattern="missing Vignette metadata",  .warning$get()[1]))
    checkTrue(grepl(pattern="not currently Suggested",  .warning$get()[2]))
    checkTrue(grepl(pattern="Evaluate more vignette chunks",  .warning$get()[3]))
    .zeroCounters()

    # check vignette 'intermediate' files
    vigdir <- system.file("testpackages", "testpkg2", "vignettes",
                          package="BiocCheck")
    vigdircontents <- BiocCheck:::getVigSources(vigdir)
    BiocCheck:::checkVigFiles(vigdir, vigdircontents)
    checkEquals(1, .note$getNum())
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
    checkTrue(.warning$getNum() ==1)
    .zeroCounters()
    BiocCheck:::.compareVersions(
        as.package_version("4.0"), as.package_version("4.1")
    )
    checkTrue(.note$getNum() ==1)
}

test_checkNewPackageVersionNumber <- function()
{
    setVersion("1.99.3")
    BiocCheck:::checkNewPackageVersionNumber(UNIT_TEST_TEMPDIR)
    checkCounter(
        "New package x version non-zero",
        "Warning"
    )

    setVersion("00.99.3")
    BiocCheck:::checkNewPackageVersionNumber(UNIT_TEST_TEMPDIR)
    checkTrue(stillZero())

    setVersion("0.2.3")
    BiocCheck:::checkNewPackageVersionNumber(UNIT_TEST_TEMPDIR)
    checkCounter(
        "New package y version not 99",
        "Error"
    )

    setVersion("0.99.1")
    BiocCheck:::checkNewPackageVersionNumber(UNIT_TEST_TEMPDIR)
    checkTrue(stillZero())
}

test_checkRbuildignore <- function()
{
    rbuildfile <- file.path(UNIT_TEST_TEMPDIR, ".Rbuildignore")
    cat("tests/", "tests", "^tests$", ".*/testthat",
        sep = "\n", file = rbuildfile
    )
    checkTrue(
        identical(
            sum(BiocCheck:::.testRbuildignore(readLines(rbuildfile))),
            3L
        )
    )
}

test_checkBiocViews <- function()
{
    .zeroCounters()
    cat("Foo: bar", file=file.path(UNIT_TEST_TEMPDIR, "DESCRIPTION"))
    BiocCheck:::checkBiocViews(UNIT_TEST_TEMPDIR)
    checkTrue(.error$getNum() == 1,
        "missing biocViews doesn't produce error")

    .zeroCounters()
    cat(
        "biocViews: foo, Cancer, bar,\n    baz",
        file=file.path(UNIT_TEST_TEMPDIR, "DESCRIPTION")
    )
    BiocCheck:::checkBiocViews(UNIT_TEST_TEMPDIR)
    checkTrue(
        .warning$getNum() == 4, "invalid biocViews don't produce warning"
    )

    .zeroCounters()
    cat(
        "biocViews: GO, CellBasedAssays",
        file=file.path(UNIT_TEST_TEMPDIR, "DESCRIPTION")
    )
    BiocCheck:::checkBiocViews(UNIT_TEST_TEMPDIR)
    checkTrue(.warning$getNum() == 0, "valid biocViews produce warning")

    .zeroCounters()
    cat(
        "biocViews: aCGH, ChipName",
        file=file.path(UNIT_TEST_TEMPDIR, "DESCRIPTION")
    )
    BiocCheck:::checkBiocViews(UNIT_TEST_TEMPDIR)
    checkTrue(.warning$getNum() == 1,
        "biocViews from multiple categories don't produce warning")
}

test_badFiles <- function(){
    pkgdir <- UNIT_TEST_TEMPDIR
    if (!dir.exists(pkgdir))
        dir.create(pkgdir)
    badfile <- file.path(pkgdir, "something.Rproj")
    file.create(badfile)
    BiocCheck:::checkBadFiles(pkgdir)
    checkEquals(1, .error$getNum())
    .zeroCounters()
    unlink(pkgdir)
}

test_checkLicenseForRestrictiveUse <- function() {
    .zeroCounters()
    BiocCheck:::.checkLicenseForRestrictiveUse("GPL-3.0")
    stillZero()

    .zeroCounters()
    BiocCheck:::.checkLicenseForRestrictiveUse("CC BY-NC-ND 4.0")
    checkEquals(1, .error$getNum())

    .zeroCounters()
    BiocCheck:::.checkLicenseForRestrictiveUse("CC BY-NC-ND 4.0 + file LICENSE")
    checkEquals(1, .error$getNum())

    .zeroCounters()
    BiocCheck:::.checkLicenseForRestrictiveUse("UNKNOWN")
    checkEquals(1, .note$getNum())

    .zeroCounters()
    BiocCheck:::.checkLicenseForRestrictiveUse(NA_character_)
    checkEquals(1, .note$getNum())
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
        "Warning"
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
        "Note"
    )

    cat("Package: Foo",
        "License: GPL-2",
        sep = "\n",
        file=file.path(UNIT_TEST_TEMPDIR, "DESCRIPTION")
    )
    BiocCheck:::checkBBScompatibility(UNIT_TEST_TEMPDIR, FALSE)
    checkError("Package name which doesn't match dir name does not cause error!")


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
        "Authors@R: c(person('Bioconductor', 'Package Maintainer', email='maintainer@bioconductor.org', role=c('aut')))",
        "License: GPL-2",
        sep = "\n",
        file=file.path(UNIT_TEST_TEMPDIR, "DESCRIPTION"))
    .zeroCounters()
    BiocCheck:::checkBBScompatibility(UNIT_TEST_TEMPDIR, FALSE)
    checkError("Missing cre role in Authors@R doesn't cause error!")


    cat(sprintf("Package: ", UNIT_TEST_PKG),
        "Version: 0.99.0",
        "License: GPL-2",
        sep = "\n",
        file=file.path(UNIT_TEST_TEMPDIR, "DESCRIPTION"))
    BiocCheck:::checkBBScompatibility(UNIT_TEST_TEMPDIR, FALSE)
    checkError("Missing Maintainer and Authors@R doesn't cause error!")


    cat(paste("Package:", UNIT_TEST_PKG),
        "Version: 0.99.0",
        "Maintainer: Joe Blow",
        "License: GPL-2",
        sep = "\n",
        file=file.path(UNIT_TEST_TEMPDIR, "DESCRIPTION"))
    BiocCheck:::checkBBScompatibility(UNIT_TEST_TEMPDIR, FALSE)
    checkTrue(.error$getNum() > 0L, "Missing email in Maintainer doesn't cause error!")


    .zeroCounters()
    cat(paste("Package:", UNIT_TEST_PKG),
        "Version: 0.99.0",
        "Maintainer: Joe Blow <joe@blow.com>",
        "Authors@R: c(person('Bioconductor Package Maintainer',",
        "    email='maintainer@bioconductor.org', role=c('aut', 'cre')))",
        "License: GPL-2",
        sep = "\n",
        file=file.path(UNIT_TEST_TEMPDIR, "DESCRIPTION"))
    BiocCheck:::checkDescription(UNIT_TEST_TEMPDIR)
    checkTrue(.warning$getNum() == 0L, "Using Maintainer and Authors@R causes warning!")
    checkTrue(.error$getNum() == 1L, "Using Maintainer and Author@R doesn't cause error!")

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
    checkTrue(.note$getNum() == 1L, "An invalid ORCID ID causes a note!")


    .zeroCounters()
    cat(paste("Package:", UNIT_TEST_PKG),
        "Version: 0.99.0",
        "Maintainer: Joe Blow <joe@blow.com>",
        "License: GPL-2",
        sep = "\n",
        file=file.path(UNIT_TEST_TEMPDIR, "DESCRIPTION"))
    BiocCheck:::checkBBScompatibility(UNIT_TEST_TEMPDIR, FALSE)
    checkTrue(.error$getNum() > 0L,"Utilize Maintainer instead of Authors@R doesn't cause error!")


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
    checkError("More than one maintainer doesn't cause error!")

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
    checkTrue(stillZero())

    .zeroCounters()
    dir.create(file.path(UNIT_TEST_TEMPDIR,"inst"))
    cat("citEntry(entry='article', title='test', author=personList(as.person('Lori Kern')), year=2020, journal='Loris Best', volume='4', issue='12', textVersion='Shepherd, Lori (2020) test. Loris Best. 4(12)')",file=file.path(UNIT_TEST_TEMPDIR,"inst/CITATION"))
    BiocCheck:::checkForCitationFile(UNIT_TEST_TEMPDIR)
    checkTrue(stillZero())

    cat("citEntry(entry='article', title='test', author=personList(as.person('Lori Kern')), year=2020, journal='Loris Best', volume='4', issue='12')",file=file.path(UNIT_TEST_TEMPDIR,"inst/CITATION"),
    append=FALSE)
    BiocCheck:::checkForCitationFile(UNIT_TEST_TEMPDIR)
    checkTrue(.note$getNum()==1, "citation produces note")

    .zeroCounters()
}

test_checkUnitTests <- function()
{
    BiocCheck:::checkUnitTests(UNIT_TEST_TEMPDIR)
    checkTrue(.note$getNum() == 1)
    dir.create(file.path(UNIT_TEST_TEMPDIR, "tests"),recursive=TRUE)
    cat("nothing", file=file.path(UNIT_TEST_TEMPDIR, "tests",
        "foo.R"))
    .zeroCounters()
    BiocCheck:::checkUnitTests(UNIT_TEST_TEMPDIR)
    checkTrue(stillZero())
    .zeroCounters()
    BiocCheck:::checkSkipOnBioc(system.file("testpackages",
        "testpkg0", package="BiocCheck"))
    checkTrue(.note$getNum() == 1)
    .zeroCounters()

    # check coding practice
    pkgdir = system.file("testpackages", "testpkg0", package="BiocCheck")
    Rdir = file.path(pkgdir, "R")
    msg_sapply <- BiocCheck:::checkSapply(Rdir)
    checkTrue(length(msg_sapply) == 1)
    msg_seq <- BiocCheck:::check1toN(Rdir)
    checkTrue(length(msg_seq) == 1)
    msg_eda <- BiocCheck:::checkExternalData(Rdir)
    checkTrue(length(msg_eda) == 5)
    avail_pkgs <- BiocManager::available()
    msg_sc <- BiocCheck:::checkSingleColon(Rdir, avail_pkgs)
    testval <- ifelse("BiocCheck" %in% avail_pkgs, 1, 0)
    checkTrue(length(msg_sc) == testval)
    pkgdir = system.file("testpackages", "testpkg2", package="BiocCheck")
    res <- BiocCheck:::checkLogicalUseFiles(pkgdir)
    checkTrue(length(res) == 1)
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
    rfile <- tempfile()
    writeLines(c(
        "message('warning: see here')",
        "warning('error here')",
        "stop('ErrOR: see here')",
        "stop('message here')"
    ), rfile)
    checkTrue(
        length(BiocCheck:::.findSignalerInSignaler(rfile)) == 4L
    )
}

test_installAndLoad <- function()
{
    BiocCheck:::installAndLoad(create_test_package('testpkg'))
    checkTrue("package:testpkg" %in% search(),
        "testpkg is not installed!")
}

test_checkDeprecatedPackages <- function()
{
     cat(sprintf("Depends: multicore", UNIT_TEST_PKG),
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
    res <- BiocCheck:::findSymbolsInParsedCode(parsedCode, "browser",
        "SYMBOL_FUNCTION_CALL")
    checkTrue(length(res) == 1)
}

test_checkVigSessionInfo <- function()
{
    pkgdir <- system.file("testpackages", "testpkg0", package="BiocCheck")
    checkVigSessionInfo(pkgdir)
    checkTrue(.note$getNum() == 1)
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

test_checkVigInstalls <- function()
{
    BiocCheck:::checkVigInstalls(
        system.file("testpackages", "testpkg0", package="BiocCheck", mustWork = TRUE)
    )
    checkTrue(.error$getNum() == 1)
    .zeroCounters()
}

test_checkVigSessionInfo <- function()
{
    BiocCheck:::checkVigSessionInfo(
        system.file("testpackages", "testpkg0", package = "BiocCheck",
            mustWork = TRUE)
    )
    checkTrue(.note$getNum() == 1)
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
    checkTrue(.warning$getNum() == 1)
}

test_checkClassEqUsage <- function()
{
    dir.create(dir <- tempfile())
    dir.create(Rdir <- sprintf("%s%s%s", dir, .Platform$file.sep, "R"))
    fl <- tempfile(tmpdir=Rdir)
    cat(
        paste(
            ## good
            "class(a)=='foo'", "class(a)!='foo'", "class(a) ==",
            "if(class(a) ==", "class ( a ) ==",
            ## bad
            "aclass(a) ==", "classy(a) ==",
            sep="\n"),
        "\n", file = fl
    )
    # create directory that should not be checked
    # only R and vignettes checked
    dir.create(Instdir <- sprintf("%s%s%s", dir, .Platform$file.sep, "inst"))
    fl <- tempfile(tmpdir=Instdir)
    cat(
        paste(
            ## good
            "class(a)=='foo'", "class(a)!='foo'", "class(a) ==",
            "if(class(a) ==", "class ( a ) ==",
            ## bad
            "aclass(a) ==", "classy(a) ==",
            sep="\n"),
        "\n", file = fl
    )
    match <- BiocCheck:::checkClassEqUsage(dir)
    lines <- sub(".* (\\d)\\)$", "\\1", match)
    checkIdentical(1:5, as.integer(lines))
}

test_checkDescriptionNamespaceConsistency <- function()
{
    run_check <- function(pkg)
        suppressMessages({
            BiocCheck:::checkDescriptionNamespaceConsistency(pkg)
        })

    testpkg <- 'testpkg'

    .zeroCounters()

    pkgpath <- create_test_package(testpkg, list(Imports="devtools"))
    BiocCheck:::installAndLoad(pkgpath)
    run_check(testpkg)
    checkTrue(.warning$getNum() == 1)
    checkEquals("Import devtools in NAMESPACE as well as DESCRIPTION.",
        .warning$get()[1])

    .zeroCounters()

    pkgpath <- create_test_package(
        testpkg, list(Imports="devtools"),
        extraActions=function(path) {
            cat("import(devtools)\n", file=file.path(path, "NAMESPACE"))
        })
    BiocCheck:::installAndLoad(pkgpath)
    run_check(testpkg)
    checkTrue(.warning$getNum() == 0L)

    .zeroCounters()

    pkgpath <- create_test_package(
        testpkg, list(Imports="usethis"),
        extraActions=function(path) {
            cat("f = function() usethis::create_package()\n",
                file=file.path(path, "R", "f.R"))
        })
    BiocCheck:::installAndLoad(pkgpath)
    run_check(testpkg)
    checkTrue(.warning$getNum() == 0L)

    .zeroCounters()

    pkgpath <- create_test_package(
        testpkg, list(Imports="usethis, BiocCheck"),
        extraActions=function(path) {
            cat("f = function() usethis::create_package()\n",
                file=file.path(path, "R", "f.R"))
        })
    BiocCheck:::installAndLoad(pkgpath)
    run_check(testpkg)
    checkTrue(.warning$getNum() == 1L)
    checkIdentical("Import BiocCheck in NAMESPACE as well as DESCRIPTION.",
                   .warning$get())

    .zeroCounters()

    pkgpath <- create_test_package(testpkg, extraActions=function(path){
        cat("import(devtools)\n", file=file.path(path, "NAMESPACE"))
    })
    BiocCheck:::installAndLoad(pkgpath)
    checkTrue("devtools" %in% names(getNamespaceImports(testpkg)))

    run_check(testpkg)
    checkTrue(.warning$getNum() == 1)
    checkEquals("Import devtools in DESCRIPTION as well as NAMESPACE.",
        .warning$get()[1])
}

test_checkImportSuggestions <- function()
{
    if (suppressMessages(suppressWarnings(requireNamespace("codetoolsBioC",
        quietly=TRUE))))
    {
        suggestions <- BiocCheck:::checkImportSuggestions("RUnit")
        checkTrue(!is.null(suggestions)) # sometimes it works and sometimes it doesn't


        BiocCheck:::installAndLoad(create_test_package('testpkg'))
        suggestions <- BiocCheck:::checkImportSuggestions("testpkg")
        checkTrue(length(suggestions) == 0)
    }

}

test_checkForBadDepends <- function()
{
    pkg <- system.file("testpackages", "testpkg0", package="BiocCheck")
    pkg_path <- BiocCheck:::installAndLoad(pkg)
    BiocCheck:::.zeroCounters()
    BiocCheck:::checkForBadDepends(file.path(pkg_path, "lib", "testpkg0"))
    checkEquals(1, BiocCheck:::.error$getNum())
    checkEquals(1, BiocCheck:::.note$getNum())
    checkTrue(grepl("providing 1 object", BiocCheck:::.error$get()[1]))
    checkTrue(grepl("how 5 object", BiocCheck:::.note$get()[1]))
}

test_remotesUsage <- function()
{
    pkg <- system.file("testpackages", "testpkg0", package="BiocCheck")
    BiocCheck:::.zeroCounters()
    BiocCheck:::checkRemotesUsage(pkg)
    checkEquals(1, BiocCheck:::.error$getNum())
    checkTrue(grepl("Remotes:", BiocCheck:::.error$get()[1]))
    BiocCheck:::.zeroCounters()
    pkg <- system.file("testpackages", "testpkg1", package="BiocCheck")
    BiocCheck:::checkRemotesUsage(pkg)
    checkEquals(0, BiocCheck:::.error$getNum())
}

test_LazyDataUsage <- function()
{
    pkg <- system.file("testpackages", "testpkg0", package="BiocCheck")
    BiocCheck:::.zeroCounters()
    BiocCheck:::checkLazyDataUsage(pkg)
    checkEquals(1, BiocCheck:::.note$getNum())
}


test_doesFileLoadPackage <- function()
{
    df <- getParseData(parse(system.file("testpackages", "testpkg0",
        "R", "requireme.R", package="BiocCheck"), keep.source=TRUE))
    res <- BiocCheck:::doesFileLoadPackage(df, "testpkg0")
    checkEquals(c(3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, 12L, 13L, 14L), res)
}

test_checkForLibraryMe <- function()
{
    load(system.file("unitTests", "IRangesParsedCode.rda", package="BiocCheck"))
    BiocCheck:::checkForLibraryMe("IRanges", IRangesParsedCode)
}

test_getFunctionLengths <- function()
{
    file <- system.file("testpackages", "testpkg0", "R",
        "parseme.R", package="BiocCheck")
    df <- getParseData(parse(file, keep.source=TRUE))
    res <- BiocCheck:::getFunctionLengths(df)
    expected <-
        structure(list(`_anonymous_.1` = structure(c(2, 1, 2), .Names = c("length",
        "startLine", "endLine")), fa = structure(c(1, 3, 3), .Names = c("length",
        "startLine", "endLine")), f2 = structure(c(1, 6, 6), .Names = c("length",
        "startLine", "endLine")), f3 = structure(c(5, 9, 13), .Names = c("length",
        "startLine", "endLine")), f4 = structure(c(4, 16, 19), .Names = c("length",
        "startLine", "endLine")), `_anonymous_.23` = structure(c(6, 23,
        28), .Names = c("length", "startLine", "endLine")), f5 = structure(c(1,
        31, 31), .Names = c("length", "startLine", "endLine")), f6 = structure(c(1,
        33, 33), .Names = c("length", "startLine", "endLine")), f7 = structure(c(6,
        35, 40), .Names = c("length", "startLine", "endLine"))), .Names = c("_anonymous_.1",
        "fa", "f2", "f3", "f4", "_anonymous_.23", "f5", "f6", "f7"))
    checkTrue(all.equal(expected, res))
}

test_getFunctionLengths2 <- function()
{
    load(system.file("unitTests", "IRangesParsedCode.rda", package="BiocCheck"))
    BiocCheck:::checkFunctionLengths("IRanges", IRangesParsedCode)
}

test_checkExportsAreDocumented <- function()
{
    pkgdir <- system.file("testpackages", "testpkg0", package="BiocCheck")
    BiocCheck:::installAndLoad(pkgdir)
    res <- BiocCheck:::checkExportsAreDocumented(pkgdir, "testpkg0")
    checkEquals(1, .error$getNum())
    res <- BiocCheck:::checkUsageOfDont(pkgdir)
    checkEquals(2, .note$getNum())
    .zeroCounters()
}

test_checkNEWS <- function()
{
    BiocCheck:::checkNEWS(system.file("testpackages", "testpkg0",
        package="BiocCheck"))
    checkEquals(1, .note$getNum())
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
    checkEquals(1, .warning$getNum())
    .zeroCounters()
    cat("lalala", file=file.path(UNIT_TEST_TEMPDIR, "NEWS.md"))
    BiocCheck:::checkNEWS(UNIT_TEST_TEMPDIR)
    checkEquals(1, .note$getNum())
    checkEquals(1, .warning$getNum())
    .zeroCounters()
}

test_checkFormatting <- function()
{
    BiocCheck:::checkFormatting(system.file("testpackages", "testpkg0",
        package="BiocCheck"))
    checkEquals(3, .note$getNum())
}

test_checkForPromptComments <- function()
{
    BiocCheck:::checkForPromptComments(system.file("testpackages", "testpkg0",
        package="BiocCheck"))
    checkEquals(1, .note$getNum())

}

test_getPkgType <- function()
{
   cat("Foo: bar", file=file.path(UNIT_TEST_TEMPDIR, "DESCRIPTION"))
   checkEquals(NA, BiocCheck:::getPkgType(UNIT_TEST_TEMPDIR))

   cat("biocViews: bad, Software", file=file.path(UNIT_TEST_TEMPDIR, "DESCRIPTION"))
   checkEquals(NA, BiocCheck:::getPkgType(UNIT_TEST_TEMPDIR))

   cat("biocViews: DifferentialExpression, CellBiology", file=file.path(UNIT_TEST_TEMPDIR, "DESCRIPTION"))
   checkEquals("Software", BiocCheck:::getPkgType(UNIT_TEST_TEMPDIR))

   cat("biocViews: DifferentialExpression, ChipManufacturer", file=file.path(UNIT_TEST_TEMPDIR, "DESCRIPTION"))
   checkEquals(NA, BiocCheck:::getPkgType(UNIT_TEST_TEMPDIR))

   cat("biocViews: GeneCardsCustomSchema, ChipManufacturer", file=file.path(UNIT_TEST_TEMPDIR, "DESCRIPTION"))
   checkEquals("AnnotationData", BiocCheck:::getPkgType(UNIT_TEST_TEMPDIR))

   # Cancer is not a valid biocView, so return NA
   cat("biocViews: Cancer, HapMap", file=file.path(UNIT_TEST_TEMPDIR, "DESCRIPTION"))
   checkEquals(NA, BiocCheck:::getPkgType(UNIT_TEST_TEMPDIR))
}

test_checkForBiocDevelSubscription <- function()
{
    if (nchar(Sys.getenv("BIOC_DEVEL_PASSWORD")))
    {
        cat("Maintainer: Joe Blow <foo@bar.com>",
                file=file.path(UNIT_TEST_TEMPDIR, "DESCRIPTION"))
        BiocCheck:::checkForBiocDevelSubscription(UNIT_TEST_TEMPDIR)
        checkEquals(.error$getNum(), 1)
        .zeroCounters()

        cat("Maintainer: Dan Tenenbaum <dtenenba@fredhutch.org>",
                file=file.path(UNIT_TEST_TEMPDIR, "DESCRIPTION"))
        BiocCheck:::checkForBiocDevelSubscription(UNIT_TEST_TEMPDIR)
        checkTrue(stillZero())
        .zeroCounters()

        cat("Maintainer: Dan Tenenbaum <DTENENBA@fredhutch.ORG>",
                file=file.path(UNIT_TEST_TEMPDIR, "DESCRIPTION"))
        BiocCheck:::checkForBiocDevelSubscription(UNIT_TEST_TEMPDIR)
        checkTrue(stillZero())
        .zeroCounters()

        cat(sprintf("Package: %s\nVersion: 0.99.0\nAuthors@R: c(person('Joe', \n  'Blow', email='joe@blow.org', role=c('aut', 'cre')))",
            UNIT_TEST_PKG),
            file=file.path(UNIT_TEST_TEMPDIR, "DESCRIPTION"))
        BiocCheck:::checkForBiocDevelSubscription(UNIT_TEST_TEMPDIR)
        checkEquals(.error$getNum(), 1)
        .zeroCounters()

        cat(sprintf("Package: %s\nVersion: 0.99.0\nAuthors@R: c(person('Dan', \n  'Tenenbaum', email='dtenenba@fredhutch.org', role=c('aut', 'cre')))",
            UNIT_TEST_PKG),
            file=file.path(UNIT_TEST_TEMPDIR, "DESCRIPTION"))
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
        checkEquals(.error$getNum(), 1)
        .zeroCounters()
        ## api is case insensitivy
        BiocCheck:::checkSupportReg("lori.shePhErd@roswellpark.org")
        checkTrue(stillZero())


        # tags
        BiocCheck:::checkWatchedTag("lori.shepherd@roswellpark.org", "biocfilecache")
        checkTrue(stillZero())
        BiocCheck:::checkWatchedTag("lori.shepherd@roswellpark.org", "unwatchedpackage")
        checkEquals(.error$getNum(), 1)
        .zeroCounters()
        ## email is case insensitive
        BiocCheck:::checkWatchedTag("lori.shePherd@rosWellpark.org", "biocfilecache")
        checkTrue(stillZero())
        ## check tag is case insenstive
        BiocCheck:::checkWatchedTag("lori.shepherd@rosWellpark.org", "bioCfiLecache")
        checkTrue(stillZero())

    }
}

test_checkForVersionNumberMismatch <- function()
{
    pkgpath <- create_test_package('badpkg', list(Version="0.0.1"))
    stopifnot(file.exists(pkgpath))

    cmd <- sprintf('"%s"/bin/R CMD build %s', R.home(), pkgpath)
    result <- system(cmd, intern=TRUE)

    oldname <- "badpkg_0.0.1.tar.gz"
    newname <- "badpkg_9.9.9.tar.gz"
    if (!file.rename(oldname, newname))
        stop("'file.rename()' failed to rename badkpgk",
             "\n  oldname: ", oldname, " newname: ", newname,
             "\n  cmd: ", cmd,
             "\n  result:",
             "\n    ", paste(result, collapse="\n    "),
             "\n")
    BiocCheck:::installAndLoad(newname)

    BiocCheck:::checkForVersionNumberMismatch(
        newname,
        BiocCheck:::.get_package_dir(newname))
    checkEquals(.error$getNum(), 1)
    .zeroCounters()
}

test_checkForDirectSlotAccess <- function()
{
    pkgpath <- create_test_package('testpkg', list(VignetteBuilder="knitr"))
    parsedCode <- list(FooBar=BiocCheck:::parseFile(
        system.file("testfiles", "directSlotAccess.Rmd",
        package="BiocCheck"), pkgpath))
    res <- BiocCheck:::checkForDirectSlotAccess(parsedCode, pkgpath)
    checkEquals(.note$getNum(), 1)
    .zeroCounters()
    parsedCode <- list(FooBar=BiocCheck:::parseFile(
        system.file("testfiles", "noDirectSlotAccess.Rmd",
        package="BiocCheck"), pkgpath))
    res <- BiocCheck:::checkForDirectSlotAccess(parsedCode, pkgpath)
    checkEquals(.note$getNum(), 0)
    .zeroCounters()
}

test_checkRVersionDependency <- function()
{
    .zeroCounters()
    desc <- file.path(tempdir(), "DESCRIPTION")
    cat("Depends: R (>= 1.0.0)", file=desc)
    BiocCheck:::checkRVersionDependency(dirname(desc))
    checkEquals(.note$getNum(), 1)
    .zeroCounters()

    cat("Depends: R", file=desc)
    BiocCheck:::checkRVersionDependency(dirname(desc))
    checkEquals(.note$getNum(), 0)
    .zeroCounters()

    cat("Imports: foobar)", file=desc)
    BiocCheck:::checkRVersionDependency(dirname(desc))
    checkEquals(.warning$getNum(), 0)
    .zeroCounters()

    cat("Depends: R (>= 10000.0.0)", file=desc) # this test might fail some day!
    BiocCheck:::checkRVersionDependency(dirname(desc))
    checkEquals(.note$getNum(), 0)
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

test_packageAlreadyExists <- function()
{
    .zeroCounters()
    BiocCheck:::checkIsPackageNameAlreadyInUse("GenomicRanges", "CRAN")
    checkEquals(.error$getNum(),0)
    BiocCheck:::checkIsPackageNameAlreadyInUse("devtools", "CRAN")
    checkEquals(.error$getNum(),1)
    BiocCheck:::checkIsPackageNameAlreadyInUse("GenomicRanges", "BioCsoft")
    checkEquals(.error$getNum(),2)
    BiocCheck:::checkIsPackageNameAlreadyInUse("GO.db", "BioCann")
    checkEquals(.error$getNum(),3)
    BiocCheck:::checkIsPackageNameAlreadyInUse("TENxBrainData", "BioCexp")
    checkEquals(.error$getNum(),4)
    BiocCheck:::checkIsPackageNameAlreadyInUse("annotation", "BioCworkflows")
    checkEquals(.error$getNum(),5)
    BiocCheck:::checkIsPackageNameAlreadyInUse("GenomicRanges", "BioCexp")
    checkEquals(.error$getNum(),5)
    BiocCheck:::checkIsPackageNameAlreadyInUse("GenomicRanges", "BioCann")
    checkEquals(.error$getNum(),5)
    BiocCheck:::checkIsPackageNameAlreadyInUse("GenomicRanges", "BioCworkflows")
    checkEquals(.error$getNum(),5)
    BiocCheck:::checkIsPackageNameAlreadyInUse("ImNotFound", "BioCexp")
    checkEquals(.error$getNum(),5)
    BiocCheck:::checkIsPackageNameAlreadyInUse("ImNotFound", "BioCann")
    checkEquals(.error$getNum(),5)
    BiocCheck:::checkIsPackageNameAlreadyInUse("ImNotFound", "BioCworkflows")
    checkEquals(.error$getNum(),5)
    BiocCheck:::checkIsPackageNameAlreadyInUse("ImNotFound", "CRAN")
    checkEquals(.error$getNum(),5)
    BiocCheck:::checkIsPackageNameAlreadyInUse("ImNotFound", "BioCsoft")
    checkEquals(.error$getNum(),5)
    .zeroCounters()
}

test_checkUsageOfDont <- function()
{
    ## testpkg0 should trigger this note for 2 out of 3 man pages
    pkgdir <- system.file("testpackages", "testpkg0", package="BiocCheck")
    BiocCheck:::installAndLoad(pkgdir)
    notemsg <- capture.output(BiocCheck:::checkUsageOfDont(pkgdir),
                              type = "message")
    checkEquals(2, .note$getNum())
    # here we verify the correct number of pages were detected
    checkTrue( any(grepl("67%", notemsg)) )
    .zeroCounters()

    ## testpkg1 contains a man page with keyword 'internal'
    ## this shouldn't trigger the note
    pkgdir <- system.file("testpackages", "testpkg1", package="BiocCheck")
    BiocCheck:::installAndLoad(pkgdir)
    BiocCheck:::checkUsageOfDont(pkgdir)
    checkEquals(0, .note$getNum())
    .zeroCounters()
}

test_IsOrcidIdValid = function()
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
