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
        suppressMessages(devtools::create(path, canned, rstudio=FALSE))
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
    BiocCheck:::checkVignetteDir(UNIT_TEST_TEMPDIR, TRUE) ## no vignettes dir
    checkError("Missing vignettes dir doesn't cause error!")
    dir.create(file.path(UNIT_TEST_TEMPDIR, "vignettes"))
    BiocCheck:::checkVignetteDir(UNIT_TEST_TEMPDIR, TRUE) ## empty vignettes dir
    checkError("Empty vignettes dir doesn't cause error!")
    cat("nothing", file=file.path(UNIT_TEST_TEMPDIR, "vignettes", "test.Rnw"))
    BiocCheck:::checkVignetteDir(UNIT_TEST_TEMPDIR, TRUE) ## vig dir w/source file

    checkTrue(.error$getNum() == 0
        && .warning$getNum() == 0
        && .note$getNum() == 0,
        "expected no errors/warnings/notes")
    .zeroCounters()
    instdoc <- file.path(UNIT_TEST_TEMPDIR, "inst", "doc")
    dir.create(instdoc, recursive=TRUE)
    cat("nothing", file=file.path(instdoc, "test.rnw"))
    .zeroCounters()

    BiocCheck:::checkVignetteDir(UNIT_TEST_TEMPDIR, TRUE)

    checkTrue(.error$getNum() == 0
        && .warning$getNum() == 1
        && .note$getNum() == 0,
        "expected 1 warning, no notes or errors")
    .zeroCounters()
    unlink(instdoc, TRUE)
    dir.create(instdoc, recursive=TRUE)
    cat("nothing", file=file.path(instdoc, "test.Rmd"))
    BiocCheck:::checkVignetteDir(UNIT_TEST_TEMPDIR, TRUE)
    checkTrue(.warning$getNum() == 1,
        "Rmd file not seen as valid vignette source")
    .zeroCounters()
    BiocCheck:::checkVignetteDir(UNIT_TEST_TEMPDIR, FALSE)
#   I don't think we should even mention this. So commenting it out.
#    checkTrue(.note$getNum() == 1,
#        "no complaints about vignette sources in inst/doc of tarball")
    .zeroCounters()


    BiocCheck:::checkVignetteDir(system.file("testpackages",
        "testpkg0", package="BiocCheck"), TRUE)
    checkEquals(1, .warning$getNum())
    checkEquals("Evaluate more vignette chunks.",
        .warning$get()[1])
}

test_checkVersionNumber <- function()
{
    setVersion("lkjgfhfdlkgjhdflkgj")
    BiocCheck:::checkVersionNumber(UNIT_TEST_TEMPDIR)
    checkError("Garbage version doesn't cause error!")
    setVersion("1.2.3.4")
    BiocCheck:::checkVersionNumber(UNIT_TEST_TEMPDIR)
    checkError("Version 1.2.3.4 doesn't cause error!")
    isDevel <- ((packageVersion("BiocInstaller")$minor %% 2) == 1)
    .zeroCounters()
    if (isDevel)
    {
        setVersion("1.2.3")
    } else {
        setVersion("1.3.3")
    }
    BiocCheck:::checkVersionNumber(UNIT_TEST_TEMPDIR)
    checkTrue(.warning$getNum() ==1)
}

test_checkNewPackageVersionNumber <- function()
{
    setVersion("1.2.3")
    BiocCheck:::checkNewPackageVersionNumber(UNIT_TEST_TEMPDIR)
    checkError("new package with wrong version number didn't throw error!")
    setVersion("0.99.1")
    BiocCheck:::checkNewPackageVersionNumber(UNIT_TEST_TEMPDIR)
    checkTrue(stillZero())
}

test_checkBiocViews <- function()
{
    cat("Foo: bar", file=file.path(UNIT_TEST_TEMPDIR, "DESCRIPTION"))
    BiocCheck:::checkBiocViews(UNIT_TEST_TEMPDIR)
    checkTrue(.error$getNum() == 1,
        "missing biocViews doesn't produce error")
    .zeroCounters()
    cat("biocViews: foo, Cancer, bar,\n    baz", file=file.path(UNIT_TEST_TEMPDIR, "DESCRIPTION"))
    BiocCheck:::checkBiocViews(UNIT_TEST_TEMPDIR)
    checkTrue(.warning$getNum() == 1,
        "invalid biocViews don't produce warning")
    cat("biocViews: GO, CellBasedAssays", file=file.path(UNIT_TEST_TEMPDIR, "DESCRIPTION"))
    .zeroCounters()
    BiocCheck:::checkBiocViews(UNIT_TEST_TEMPDIR)
    checkTrue(.warning$getNum() == 0,
        "valid biocViews produce warning")
    .zeroCounters()
    cat("biocViews: aCGH, ChipName", file=file.path(UNIT_TEST_TEMPDIR, "DESCRIPTION"))
    BiocCheck:::checkBiocViews(UNIT_TEST_TEMPDIR)
    checkTrue(.warning$getNum() == 1,
        "biocViews from multiple categories don't produce warning")
}

test_checkBBScompatibility <- function()
{
    cat("Package : foo", file=file.path(UNIT_TEST_TEMPDIR, "DESCRIPTION"))
    BiocCheck:::checkBBScompatibility(UNIT_TEST_TEMPDIR)
    checkError("Space in DESCRIPTION field name doesn't cause error")
    .zeroCounters()

    cat("Package: foo\n\nImports: bar", file=file.path(UNIT_TEST_TEMPDIR,
        "DESCRIPTION"))
    BiocCheck:::checkBBScompatibility(UNIT_TEST_TEMPDIR)
    checkError("Blank line in DESCRIPTION doesn't cause error")
    .zeroCounters()

    cat("Package: Foo", file=file.path(UNIT_TEST_TEMPDIR, "DESCRIPTION"))
    BiocCheck:::checkBBScompatibility(UNIT_TEST_TEMPDIR)
    checkError("Package name which doesn't match dir name does not cause error!")
    cat(sprintf("Package: ", UNIT_TEST_PKG),
        file=file.path(UNIT_TEST_TEMPDIR, "DESCRIPTION"))
    BiocCheck:::checkBBScompatibility(UNIT_TEST_TEMPDIR)
    checkError("Missing Version doesn't cause error!")
    #cat(sprintf("Package: %s\nVersion: 0.99.0\nAuthors@R: syntax error", UNIT_TEST_PKG),
    #    file=file.path(UNIT_TEST_TEMPDIR, "DESCRIPTION"))
    #BiocCheck:::checkBBScompatibility(UNIT_TEST_TEMPDIR)
    #checkError("Syntax error in Authors@R doesn't cause error!")
    cat(sprintf("Package: %s\nVersion: 0.99.0\nAuthors@R: 1", UNIT_TEST_PKG),
        file=file.path(UNIT_TEST_TEMPDIR, "DESCRIPTION"))
    .zeroCounters()
    BiocCheck:::checkBBScompatibility(UNIT_TEST_TEMPDIR)
    checkError("Wrong class in Authors@R doesn't cause error!")
    cat(sprintf("Package: %s\nVersion: 0.99.0\nAuthors@R: c(person('Bioconductor', 'Package Maintainer', email='maintainer@bioconductor.org', role=c('aut')))", UNIT_TEST_PKG),
        file=file.path(UNIT_TEST_TEMPDIR, "DESCRIPTION"))
    BiocCheck:::checkBBScompatibility(UNIT_TEST_TEMPDIR)
    checkError("Missing cre role in Authors@R doesn't cause error!")
    cat(sprintf("Package: %s\nVersion: 0.99.0", UNIT_TEST_PKG),
        file=file.path(UNIT_TEST_TEMPDIR, "DESCRIPTION"))
    BiocCheck:::checkBBScompatibility(UNIT_TEST_TEMPDIR)
    checkError("Missing Maintainer and Authors@R doesn't cause error!")
    cat(sprintf("Package: %s\nVersion: 0.99.0\nMaintainer: Joe Blow",
        UNIT_TEST_PKG),
        file=file.path(UNIT_TEST_TEMPDIR, "DESCRIPTION"))
    BiocCheck:::checkBBScompatibility(UNIT_TEST_TEMPDIR)
    checkError("Missing email in Maintainer doesn't cause error!")
    .zeroCounters()
    cat(sprintf("Package: %s\nVersion: 0.99.0\nMaintainer: Joe Blow <joe@blow.com>",
        UNIT_TEST_PKG),
        file=file.path(UNIT_TEST_TEMPDIR, "DESCRIPTION"))
    BiocCheck:::checkBBScompatibility(UNIT_TEST_TEMPDIR)
    checkTrue(stillZero())
    .zeroCounters()
    cat(sprintf("Package: %s\nVersion: 0.99.0\nAuthors@R: c(person('Bioconductor', \n  'Package Maintainer', email='maintainer@bioconductor.org', role=c('aut', 'cre')))",
        UNIT_TEST_PKG),
        file=file.path(UNIT_TEST_TEMPDIR, "DESCRIPTION"))
    BiocCheck:::checkBBScompatibility(UNIT_TEST_TEMPDIR)
    checkTrue(stillZero())

}

test_checkUnitTests <- function()
{
    BiocCheck:::checkUnitTests(UNIT_TEST_TEMPDIR)
    checkTrue(.note$getNum() == 1)
    dir.create(file.path(UNIT_TEST_TEMPDIR, "tests"))
    cat("nothing", file=file.path(UNIT_TEST_TEMPDIR, "tests",
        "foo.R"))
    .zeroCounters()
    BiocCheck:::checkUnitTests(UNIT_TEST_TEMPDIR)
    checkTrue(stillZero())
}

test_installAndLoad <- function()
{
    BiocCheck:::installAndLoad(create_test_package('testpkg'))
    checkTrue("package:testpkg" %in% search(),
        "testpkg is not installed!")
}

test_checkRegistrationOfEntryPoints <- function()
{
    parsedCode1 <- BiocCheck:::parseFiles(system.file("testpackages",
        "testpkg1", package="BiocCheck"))
    BiocCheck:::installAndLoad(system.file("testpackages", "testpkg1",
        package="BiocCheck"))

    BiocCheck:::checkRegistrationOfEntryPoints("testpkg1", parsedCode1)
    checkTrue(.warning$getNum() == 1,
        "unregistered code not flagged!")
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
    df <- BiocCheck:::parseFile(testFile, "BiocCHeck")
    checkTrue(all(dim(df) == c(6,9)))
}

test_checkTorF <- function()
{
    if (is.null(parsedCode))
        parsedCode <- BiocCheck:::parseFiles(system.file("testpackages",
            "testpkg0", package="BiocCheck"))

    res <- BiocCheck:::findSymbolInParsedCode(parsedCode, "testpkg0", "T",
        "SYMBOL")
    checkTrue(res == 1)


    ## Even though F is found twice in a single file (morecode.R),
    ## res is 1, because it keeps track of the number of files that matched,
    ## not the overall number of matches. Maybe this should change,
    ## maybe it doesn't matter.
    res <- BiocCheck:::findSymbolInParsedCode(parsedCode, "testpkg0", "F",
        "SYMBOL")
    checkTrue(res == 1)
}


test_checkForBrowser <- function()
{
    if (is.null(parsedCode))
        parsedCode <- BiocCheck:::parseFiles(system.file("testpackages",
            "testpkg0", package="BiocCheck"))
    res <- BiocCheck:::findSymbolInParsedCode(parsedCode, "testpkg0", "browser",
        "SYMBOL_FUNCTION_CALL")
    checkTrue(res == 1)
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
        testpkg, list(Imports="devtools"),
        extraActions=function(path) {
            cat("f = function() devtools::create()\n",
                file=file.path(path, "R", "f.R"))
        })
    BiocCheck:::installAndLoad(pkgpath)
    run_check(testpkg)
    checkTrue(.warning$getNum() == 0L)

    .zeroCounters()

    pkgpath <- create_test_package(
        testpkg, list(Imports="devtools, dplyr"),
        extraActions=function(path) {
            cat("f = function() devtools::create()\n",
                file=file.path(path, "R", "f.R"))
        })
    BiocCheck:::installAndLoad(pkgpath)
    run_check(testpkg)
    checkTrue(.warning$getNum() == 1L)
    checkIdentical("Import dplyr in NAMESPACE as well as DESCRIPTION.",
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
    BiocCheck:::installAndLoad(system.file("testpackages", "testpkg0",
        package="BiocCheck"))
    BiocCheck:::checkForBadDepends(file.path(tempdir(), "lib", "testpkg0"))
    checkEquals(1, .error$getNum())
    checkEquals(1, .note$getNum())
    checkTrue(grepl("baddep", .error$get()[1]))
    checkTrue(grepl("colone", .note$get()[1]))
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
}

test_checkNEWS <- function()
{
    BiocCheck:::checkNEWS(system.file("testpackages", "testpkg0",
        package="BiocCheck"))
    checkEquals(1, .note$getNum())
    .zeroCounters()
    cat("lalala", file=file.path(UNIT_TEST_TEMPDIR, "NEWS"))
    BiocCheck:::checkNEWS(UNIT_TEST_TEMPDIR)
    stillZero()
    unlink(file.path(UNIT_TEST_TEMPDIR, "NEWS"))
    dir.create(file.path(UNIT_TEST_TEMPDIR, "inst"), FALSE)
    cat("lalala", file=file.path(UNIT_TEST_TEMPDIR, "inst", "NEWS.Rd"))
    BiocCheck:::checkNEWS(UNIT_TEST_TEMPDIR)
    checkEquals(1, .warning$getNum())
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
    desc <- file.path(tempdir(), "DESCRIPTION")
    cat("Depends: R (>= 1.0.0)", file=desc)
    BiocCheck:::checkRVersionDependency(dirname(desc))
    checkEquals(.warning$getNum(), 1)
    .zeroCounters()

    cat("Depends: R", file=desc)
    BiocCheck:::checkRVersionDependency(dirname(desc))
    checkEquals(.warning$getNum(), 0)
    .zeroCounters()

    cat("Imports: foobar)", file=desc)
    BiocCheck:::checkRVersionDependency(dirname(desc))
    checkEquals(.warning$getNum(), 0)
    .zeroCounters()

    cat("Depends: R (>= 10000.0.0)", file=desc) # this test might fail some day!
    BiocCheck:::checkRVersionDependency(dirname(desc))
    checkEquals(.warning$getNum(), 0)
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
