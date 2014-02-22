UNIT_TEST_PKG <- "unitTestTempDir"
UNIT_TEST_TEMPDIR <- file.path(tempdir(), UNIT_TEST_PKG)

message("You may see some warnings here -- they don't indicate unit test problems.")

library(devtools)

.printf <- BiocCheck:::.printf 

parsedCode <- NULL


inspect <- function()
{
    .printf("requirements: %s, recommendations: %s, notes: %s",
        BiocCheck:::.requirements$getNum(),
        BiocCheck:::.recommendations$getNum(),
        BiocCheck:::.notes$getNum())
    print("errors:")
    print(BiocCheck:::.requirements$get())
    print("warnings:")
    print(BiocCheck:::.recommendations$get())
    print("notes:")
    print(BiocCheck:::.notes$get())

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
    suppressMessages(create(path, canned))
    cat("#", file=file.path(path, "NAMESPACE"))
    extraActions(path)
    path
}

checkError <- function(msg)
{
    if (missing(msg)) msg = ""
    # .printf("Errors: %s, Warnings: %s, Notes: %s", 
    #     BiocCheck:::.requirements$getNum(),
    #     BiocCheck:::.recommendations$getNum(),
    #     BiocCheck:::.notes$getNum())
    checkTrue(
        BiocCheck:::.notes$getNum() == 0 &&
        BiocCheck:::.recommendations$getNum() == 0 &&
        BiocCheck:::.requirements$getNum() == 1,
        msg
    )
    zeroCounters()
}

zeroCounters <- function()
{
    BiocCheck:::.notes$zero()
    BiocCheck:::.recommendations$zero()
    BiocCheck:::.requirements$zero()
}

stillZero <- function()
{
    BiocCheck:::.notes$getNum() == 0 &&
    BiocCheck:::.recommendations$getNum() == 0 &&
    BiocCheck:::.requirements$getNum() == 0
}

.setUp <- function()
{
    dir.create(UNIT_TEST_TEMPDIR)
    zeroCounters()
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

    checkTrue(BiocCheck:::.requirements$getNum() == 0 
        && BiocCheck:::.recommendations$getNum() == 0 
        && BiocCheck:::.notes$getNum() == 0,
        "expected no errors/warnings/notes")
    zeroCounters()
    instdoc <- file.path(UNIT_TEST_TEMPDIR, "inst", "doc")
    dir.create(instdoc, recursive=TRUE)
    cat("nothing", file=file.path(instdoc, "test.rnw"))
    zeroCounters()

    BiocCheck:::checkVignetteDir(UNIT_TEST_TEMPDIR, TRUE)

    checkTrue(BiocCheck:::.requirements$getNum() == 0 
        && BiocCheck:::.recommendations$getNum() == 1 
        && BiocCheck:::.notes$getNum() == 0,
        "expected 1 warning, no notes or errors")
    zeroCounters()
    unlink(instdoc, TRUE)
    dir.create(instdoc, recursive=TRUE)
    cat("nothing", file=file.path(instdoc, "test.Rmd"))
    BiocCheck:::checkVignetteDir(UNIT_TEST_TEMPDIR, TRUE)
    checkTrue(BiocCheck:::.recommendations$getNum() == 1, 
        "Rmd file not seen as valid vignette source")
    zeroCounters()
    BiocCheck:::checkVignetteDir(UNIT_TEST_TEMPDIR, FALSE)
    checkTrue(BiocCheck:::.notes$getNum() == 1, 
        "Rmd file not seen as valid vignette source")
    zeroCounters()


    BiocCheck:::checkVignetteDir(system.file("testpackages",
        "testpkg0", package="BiocCheck"), TRUE)
    checkEquals(1, BiocCheck:::.recommendations$getNum())
    checkEquals("Evaluate more vignette chunks.",
        BiocCheck:::.recommendations$get()[1])
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
    zeroCounters()
    if (isDevel)
    {
        setVersion("1.2.3")
    } else {
        setVersion("1.3.3")
    }
    BiocCheck:::checkVersionNumber(UNIT_TEST_TEMPDIR)
    checkTrue(BiocCheck:::.recommendations$getNum() ==1)
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
    checkTrue(BiocCheck:::.recommendations$getNum() == 1,
        "missing biocViews doesn't produce warning")
    zeroCounters()
    cat("biocViews: foo, Cancer, bar,\n    baz", file=file.path(UNIT_TEST_TEMPDIR, "DESCRIPTION"))
    BiocCheck:::checkBiocViews(UNIT_TEST_TEMPDIR)
    checkTrue(BiocCheck:::.recommendations$getNum() == 1,
        "invalid biocViews don't produce warning")
    cat("biocViews: GO, CellBasedAssays", file=file.path(UNIT_TEST_TEMPDIR, "DESCRIPTION"))
    zeroCounters()
    BiocCheck:::checkBiocViews(UNIT_TEST_TEMPDIR)
    checkTrue(BiocCheck:::.recommendations$getNum() == 0,
        "valid biocViews produce warning")
    zeroCounters()
    cat("biocViews: aCGH, ChipName", file=file.path(UNIT_TEST_TEMPDIR, "DESCRIPTION"))
    BiocCheck:::checkBiocViews(UNIT_TEST_TEMPDIR)
    checkTrue(BiocCheck:::.recommendations$getNum() == 1,
        "biocViews from multiple categories don't produce warning")
}

test_checkBBScompatibility <- function()
{
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
    cat(sprintf("Package: %s\nVersion: 0.99.0\nAuthors@R: 1 + 1", UNIT_TEST_PKG),
        file=file.path(UNIT_TEST_TEMPDIR, "DESCRIPTION"))
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
    cat(sprintf("Package: %s\nVersion: 0.99.0\nMaintainer: Joe Blow <joe@blow.com>",
        UNIT_TEST_PKG),
        file=file.path(UNIT_TEST_TEMPDIR, "DESCRIPTION"))
    BiocCheck:::checkBBScompatibility(UNIT_TEST_TEMPDIR)
    checkTrue(stillZero())
    zeroCounters()
    cat(sprintf("Package: %s\nVersion: 0.99.0\nAuthors@R: c(person('Bioconductor', \n  'Package Maintainer', email='maintainer@bioconductor.org', role=c('aut', 'cre')))",
        UNIT_TEST_PKG),
        file=file.path(UNIT_TEST_TEMPDIR, "DESCRIPTION"))
    BiocCheck:::checkBBScompatibility(UNIT_TEST_TEMPDIR)
    checkTrue(stillZero())

}

test_checkUnitTests <- function()
{
    BiocCheck:::checkUnitTests(UNIT_TEST_TEMPDIR)
    checkTrue(BiocCheck:::.notes$getNum() == 1)
    dir.create(file.path(UNIT_TEST_TEMPDIR, "tests"))
    cat("nothing", file=file.path(UNIT_TEST_TEMPDIR, "tests",
        "foo.R"))
    zeroCounters()
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
    if(!require(Biobase)) suppressPackageStartupMessages(require("Biobase"))
    zeroCounters()
    BiocCheck:::checkRegistrationOfEntryPoints("Biobase")
    checkTrue(stillZero())
    zeroCounters()
    # This test could fail if devtools registers routines:
    if(!require(devtools)) suppressPackageStartupMessages(require("devtools"))
    BiocCheck:::checkRegistrationOfEntryPoints("devtools")
    checkTrue(BiocCheck:::.recommendations$getNum() == 1)
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

test_checkForDotC <- function()
{
    if (is.null(parsedCode))
        parsedCode <- BiocCheck:::parseFiles(system.file("testpackages",
            "testpkg0", package="BiocCheck"))
    res <- BiocCheck:::findSymbolInParsedCode(parsedCode, "testpkg0", ".C",
        "SYMBOL_FUNCTION_CALL")
    checkTrue(res == 2)
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
    testpkg <- 'testpkg'

    zeroCounters()

    pkgpath <- create_test_package(testpkg, list(Imports="devtools"))
    BiocCheck:::installAndLoad(pkgpath)
    BiocCheck:::checkDescriptionNamespaceConsistency(testpkg)
    checkTrue(BiocCheck:::.recommendations$getNum() == 1)
    checkEquals("Import devtools in NAMESPACE as well as DESCRIPTION.",
        BiocCheck:::.recommendations$get()[1])

    zeroCounters()

    pkgpath <- create_test_package(testpkg, extraActions=function(path){
        cat("import(devtools)\n", file=file.path(path, "NAMESPACE"))
    })
    BiocCheck:::installAndLoad(pkgpath)

    checkTrue("devtools" %in% names(getNamespaceImports(testpkg)))

    BiocCheck:::checkDescriptionNamespaceConsistency(testpkg)
    checkTrue(BiocCheck:::.recommendations$getNum() == 1)
    checkEquals("Import devtools in DESCRIPTION as well as NAMESPACE.",
        BiocCheck:::.recommendations$get()[1])

}

test_checkImportSuggestions <- function()
{
    if (suppressMessages(suppressWarnings(require(codetoolsBioC))))
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
    checkEquals(1, BiocCheck:::.requirements$getNum())
    checkEquals(1, BiocCheck:::.notes$getNum())
    checkTrue(grepl("baddep", BiocCheck:::.requirements$get()[1]))
    checkTrue(grepl("colone", BiocCheck:::.notes$get()[1]))
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
    checkEquals(1, BiocCheck:::.notes$getNum())
}

test_checkNEWS <- function()
{
    BiocCheck:::checkNEWS(system.file("testpackages", "testpkg0",
        package="BiocCheck"))
    checkEquals(1, BiocCheck:::.notes$getNum())
    zeroCounters()
    cat("lalala", file=file.path(UNIT_TEST_TEMPDIR, "NEWS"))
    BiocCheck:::checkNEWS(UNIT_TEST_TEMPDIR)
    stillZero()
    unlink(file.path(UNIT_TEST_TEMPDIR, "NEWS"))
    dir.create(file.path(UNIT_TEST_TEMPDIR, "inst"), FALSE)
    cat("lalala", file=file.path(UNIT_TEST_TEMPDIR, "inst", "NEWS.Rd"))
    BiocCheck:::checkNEWS(UNIT_TEST_TEMPDIR)
    checkEquals(1, BiocCheck:::.recommendations$getNum())
}

test_checkFormatting <- function()
{
    BiocCheck:::checkFormatting(system.file("testpackages", "testpkg0",
        package="BiocCheck"))
    checkEquals(3, BiocCheck:::.notes$getNum())
}

test_checkForPromptComments <- function()
{
    BiocCheck:::checkForPromptComments(system.file("testpackages", "testpkg0",
        package="BiocCheck"))
    checkEquals(1, BiocCheck:::.notes$getNum())

}