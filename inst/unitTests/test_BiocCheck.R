UNIT_TEST_PKG <- "unitTestTempDir"
UNIT_TEST_TEMPDIR <- file.path(tempdir(), UNIT_TEST_PKG)

message("You may see some warnings here -- they don't indicate unit test problems.")

library(devtools)

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
    #     BiocCheck:::.errors$getNum(),
    #     BiocCheck:::.warnings$getNum(),
    #     BiocCheck:::.notes$getNum())
    checkTrue(
        BiocCheck:::.notes$getNum() == 0 &&
        BiocCheck:::.warnings$getNum() == 0 &&
        BiocCheck:::.errors$getNum() == 1,
        msg
    )
    zeroCounters()
}

zeroCounters <- function()
{
    BiocCheck:::.notes$zero()
    BiocCheck:::.warnings$zero()
    BiocCheck:::.errors$zero()
}

stillZero <- function()
{
    BiocCheck:::.notes$getNum() == 0 &&
    BiocCheck:::.warnings$getNum() == 0 &&
    BiocCheck:::.errors$getNum() == 0
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
    BiocCheck:::checkVignetteDir(UNIT_TEST_TEMPDIR) ## no vignettes dir
    checkError("Missing vignettes dir doesn't cause error!")
    dir.create(file.path(UNIT_TEST_TEMPDIR, "vignettes"))
    BiocCheck:::checkVignetteDir(UNIT_TEST_TEMPDIR) ## empty vignettes dir
    checkError("Empty vignettes dir doesn't cause error!")
    cat("nothing", file=file.path(UNIT_TEST_TEMPDIR, "vignettes", "test.Rnw"))
    BiocCheck:::checkVignetteDir(UNIT_TEST_TEMPDIR) ## vig dir w/source file

    checkTrue(BiocCheck:::.errors$getNum() == 0 
        && BiocCheck:::.warnings$getNum() == 0 
        && BiocCheck:::.notes$getNum() == 0)
    zeroCounters()
    instdoc <- file.path(UNIT_TEST_TEMPDIR, "inst", "doc")
    dir.create(instdoc, recursive=TRUE)
    cat("nothing", file=file.path(instdoc, "test.rnw"))
    zeroCounters()
    BiocCheck:::checkVignetteDir(UNIT_TEST_TEMPDIR)
    checkTrue(BiocCheck:::.errors$getNum() == 0 
        && BiocCheck:::.warnings$getNum() == 1 
        && BiocCheck:::.notes$getNum() == 0)
    zeroCounters()
    unlink(instdoc, TRUE)
    dir.create(instdoc, recursive=TRUE)
    cat("nothing", file=file.path(instdoc, "test.Rmd"))
    BiocCheck:::checkVignetteDir(UNIT_TEST_TEMPDIR)
    checkTrue(BiocCheck:::.warnings$getNum() == 1, 
        "Rmd file not seen as valid vignette source")
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
    checkTrue(BiocCheck:::.warnings$getNum() ==1)
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
    checkTrue(BiocCheck:::.warnings$getNum() == 1,
        "missing biocViews doesn't produce warning")
    zeroCounters()
    cat("biocViews: foo, Cancer, bar,\n    baz", file=file.path(UNIT_TEST_TEMPDIR, "DESCRIPTION"))
    BiocCheck:::checkBiocViews(UNIT_TEST_TEMPDIR)
    checkTrue(BiocCheck:::.warnings$getNum() == 1,
        "invalid biocViews don't produce warning")
    cat("biocViews: GO, CellBasedAssays", file=file.path(UNIT_TEST_TEMPDIR, "DESCRIPTION"))
    zeroCounters()
    BiocCheck:::checkBiocViews(UNIT_TEST_TEMPDIR)
    checkTrue(BiocCheck:::.warnings$getNum() == 0,
        "valid biocViews produce warning")
    zeroCounters()
    cat("biocViews: aCGH, ChipName", file=file.path(UNIT_TEST_TEMPDIR, "DESCRIPTION"))
    BiocCheck:::checkBiocViews(UNIT_TEST_TEMPDIR)
    checkTrue(BiocCheck:::.warnings$getNum() == 1,
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
    checkTrue(BiocCheck:::.warnings$getNum() == 1)
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
    DEACTIVATED("not ready yet")
    parsedCode <- BiocCheck:::parseFiles(system.file("testpackages",
        "devtools0", package="BiocCheck"))
    res <- BiocCheck:::checkTorF(parsedCode)
    checkTrue(length(res$t) == 1)
    checkTrue(length(res$f) == 1)

}

test_checkForDotC <- function()
{
    parsedCode <- BiocCheck:::parseFiles(system.file("testpackages",
        "devtools0", package="BiocCheck"))
    res <- BiocCheck:::checkForDotC(parsedCode, "devtools0")
    checkTrue(length(res) == 1)
    res
}

test_checkDescriptionNamespaceConsistency <- function()
{
    testpkg <- 'testpkg'

    zeroCounters()

    pkgpath <- create_test_package(testpkg, list(Imports="devtools"))
    BiocCheck:::installAndLoad(pkgpath)
    BiocCheck:::checkDescriptionNamespaceConsistency(testpkg)
    checkTrue(BiocCheck:::.warnings$getNum() == 1)
    checkEquals("devtools imported in DESCRIPTION but not NAMESPACE",
        BiocCheck:::.warnings$get()[1])

    zeroCounters()

    pkgpath <- create_test_package(testpkg, extraActions=function(path){
        cat("import(devtools)\n", file=file.path(path, "NAMESPACE"))
    })
    BiocCheck:::installAndLoad(pkgpath)

    checkTrue("devtools" %in% names(getNamespaceImports(testpkg)))

    BiocCheck:::checkDescriptionNamespaceConsistency(testpkg)
    checkTrue(BiocCheck:::.warnings$getNum() == 1)
    checkEquals("devtools imported in NAMESPACE but not in DESCRIPTION:Imports",
        BiocCheck:::.warnings$get()[1])

}