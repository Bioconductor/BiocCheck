source("helpers.R")

library(devtools)
library(tinytest)

# checkVersionNumber ------------------------------------------------------
temp_dir <- tempdir()
.bioctest <- create_test_package(
    test_dir = temp_dir, description = list(Version = "lkjgfhfdlkgjhdflkgj")
)
BiocCheck:::checkVersionNumber(.bioctest)
checkCounter("Garbage version doesn't cause error!")

.bioctest <- create_test_package(
    test_dir = temp_dir, description = list(Version = "1.2.3.4")
)
BiocCheck:::checkVersionNumber(.bioctest)
checkCounter("Version 1.2.3.4 doesn't cause error!")

isDevel <- (BiocManager:::.version_bioc("devel") == BiocManager::version())
testversion <- if (isDevel) "1.2.3" else "1.3.3"
.bioctest <- create_test_package(
    test_dir = temp_dir, description = list(Version = testversion)
)
BiocCheck:::checkVersionNumber(.bioctest)
expect_equivalent(.BiocCheck$getNum("warning"), 1)
.BiocCheck$zero()
unlink(temp_dir, recursive = TRUE)


# checkNewPackageVersionNumber --------------------------------------------

temppkgdir <- tempfile()
.bioctest <- create_test_package(
    test_dir = temppkgdir, description = list(Version = "1.99.3")
)
BiocCheck:::checkNewPackageVersionNumber(.bioctest)
checkCounter(
    "New package x version non-zero",
    "warning"
)

.bioctest <- create_test_package(
    test_dir = temppkgdir, description = list(Version = "0.99.3")
)
BiocCheck:::checkNewPackageVersionNumber(.bioctest)
expect_true(stillZero())

.bioctest <- create_test_package(
    test_dir = temppkgdir, description = list(Version = "0.2.3")
)
BiocCheck:::checkNewPackageVersionNumber(.bioctest)
checkCounter(
    "New package y version not 99",
    "error"
)

.bioctest <- create_test_package(
    test_dir = temppkgdir, description = list(Version = "0.99.1")
)
BiocCheck:::checkNewPackageVersionNumber(.bioctest)
expect_true(stillZero())


# checkRbuildIgnore -------------------------------------------------------

rbuildfile <- file.path(UNIT_TEST_TEMPDIR, ".Rbuildignore")
if (!dir.exists(UNIT_TEST_TEMPDIR))
    dir.create(UNIT_TEST_TEMPDIR, recursive = TRUE)

cat(
    "tests", "tests/", "^tests$", "^tests/", "^tests/$",
    ".*/testthat", "^tests/testthat/cache$", "^longtests/testthat/cache$",
    "longtests", "longtests/", "^longtests/", "^longtests$", "^longtests/$",
    "^nnntests$",
    sep = "\n", file = rbuildfile
)
expect_identical(
    BiocCheck:::.testRbuildignore(readLines(rbuildfile)),
    c(
        TRUE, TRUE, TRUE, TRUE, FALSE,
        FALSE, FALSE, FALSE,
        TRUE, TRUE, TRUE, TRUE, FALSE,
        FALSE
    )
)

# checkBiocCheckOutputFolder ----------------------------------------------
temp_dir <- tempfile()
.bioctest <- create_test_package(test_dir = temp_dir)
check_folder <- file.path(
    .bioctest$sourceDir, paste0(.bioctest$packageName, ".BiocCheck")
)
dir.create(check_folder, recursive = TRUE)
BiocCheck:::checkBiocCheckOutputFolder(.bioctest)
expect_equivalent(.BiocCheck$getNum("error"), 1L)
.BiocCheck$zero()
unlink(temp_dir, recursive = TRUE)

# checkInstDocFolder ------------------------------------------------------
temp_dir <- tempfile()
.bioctest <- create_test_package(test_dir = temp_dir)
inst_dir <- file.path(.bioctest$sourceDir, "inst", "doc")
dir.create(inst_dir, recursive = TRUE)
BiocCheck:::checkInstDocFolder(.bioctest)
expect_equivalent(.BiocCheck$getNum("error"), 0L)

file.create(file.path(inst_dir, "index.html"))
BiocCheck:::checkInstDocFolder(.bioctest)
expect_equivalent(.BiocCheck$getNum("error"), 1L)
.BiocCheck$zero()

unlink(temp_dir, recursive = TRUE)

# checkBiocViews ----------------------------------------------------------
.BiocCheck$zero()
temp_dir <- tempfile()

.bioctest <- create_test_package(
    test_dir = temp_dir, description = list(Foo = "bar")
)
BiocCheck:::checkBiocViews(.bioctest)
checkCounter("missing biocViews doesn't produce error")


.bioctest <- create_test_package(
    test_dir = temp_dir,
    description = list(biocViews = "foo, Cancer, bar,\n    baz")
)
BiocCheck:::checkBiocViews(.bioctest)
expect_equivalent(
    .BiocCheck$getNum("warning"), 1L,
    info = "invalid biocViews does not produce warning"
)
.BiocCheck$zero()

.bioctest <- create_test_package(
    test_dir = temp_dir,
    description = list(biocViews = "GO, CellBasedAssays")
)
BiocCheck:::checkBiocViews(.bioctest)
expect_equivalent(
    .BiocCheck$getNum("warning"), 0L,
    info = "valid biocViews produce warning"
)
.BiocCheck$zero()

.bioctest <- create_test_package(
    test_dir = temp_dir,
    description = list(biocViews = "aCGH, ChipName")
)
BiocCheck:::checkBiocViews(.bioctest)
expect_equivalent(
    .BiocCheck$getNum("warning"), 1L,
    info = "biocViews from multiple categories don't produce warning"
)
.BiocCheck$zero()
unlink(temp_dir, recursive = TRUE)

# checkBadFiles -----------------------------------------------------------
temp_dir <- tempfile()
.bioctest <- create_test_package(
    test_dir = temp_dir,
    extraActions = function(pkgdir){
        badfile <- file.path(pkgdir, "something.Rproj")
        file.create(badfile)
    }
)
BiocCheck:::checkBadFiles(.bioctest)
expect_equivalent(1, .BiocCheck$getNum("error"))
.BiocCheck$zero()
unlink(temp_dir, recursive = TRUE)

# checkSystemCalls --------------------------------------------------------
.bioctest <- read_test_package("testpkg0")
msg <- BiocCheck:::findSymbolsInRFiles(
    .bioctest, "system", "SYMBOL_FUNCTION_CALL"
)
## use tinytest:: until fixed in
## https://github.com/markvanderloo/tinytest/issues/124
tinytest::expect_match(msg, "system\\(\\) in R/bad_coding\\.R.*")
.BiocCheck$zero()

# check T / F usage -------------------------------------------------------
msg <- BiocCheck:::findSymbolsInRFiles(
    .bioctest, c("T", "F"), "SYMBOL", notLookback = "$"
)
expect_equivalent(
    sum(grepl("R/TFindex.R", msg)), 4L
)
expect_equivalent(
    sum(grepl("R/morecode.R", msg)), 2L
)
.BiocCheck$zero()

# checkLicenseForRestrictiveUse -------------------------------------------
BiocCheck:::checkLicenseForRestrictiveUse("GPL-3.0")
stillZero()
.BiocCheck$zero()

BiocCheck:::checkLicenseForRestrictiveUse("GPL (>= 3)")
stillZero()
.BiocCheck$zero()

BiocCheck:::checkLicenseForRestrictiveUse("CC BY-NC-ND 4.0")
expect_equivalent(1, .BiocCheck$getNum("error"))
.BiocCheck$zero()

BiocCheck:::checkLicenseForRestrictiveUse("CC BY-NC-ND 4.0 + file LICENSE")
expect_equivalent(1, .BiocCheck$getNum("error"))
.BiocCheck$zero()

BiocCheck:::checkLicenseForRestrictiveUse("UNKNOWN")
expect_equivalent(1, .BiocCheck$getNum("note"))
.BiocCheck$zero()

BiocCheck:::checkLicenseForRestrictiveUse(NA_character_)
expect_equivalent(1, .BiocCheck$getNum("note"))
.BiocCheck$zero()

# analyze_licenses --------------------------------------------------------
licensedb <- gsub(
    "$R_HOME", Sys.getenv("R_HOME"),
    BiocCheck:::.LICENSE_DB_LOCATION, fixed = TRUE
)
result <- tools:::analyze_licenses("GPL-3.0", licensedb)
expect_true(!result$is_verified)
BiocCheck:::checkLicenseForRestrictiveUse("GPL-3.0")
expect_equivalent(1, .BiocCheck$getNum("note"))
.BiocCheck$zero()

result <- tools:::analyze_licenses("GPL-3", licensedb)
expect_true(result$is_verified)
BiocCheck:::checkLicenseForRestrictiveUse("GPL-3")
expect_equivalent(0, .BiocCheck$getNum("note"))
.BiocCheck$zero()

# checkIndivFileSizes -----------------------------------------------------

.findLargeFiles_org <- BiocCheck:::.findLargeFiles
.findLargeFiles <- function(...) {
    c("fileA.rda", "fileB.rds")
}
assignInNamespace('.findLargeFiles', .findLargeFiles, "BiocCheck")

temp_dir <- tempfile()
.bioctest <- create_test_package(test_dir = temp_dir)
BiocCheck:::checkIndivFileSizes(.bioctest)
expect_equivalent(.BiocCheck$getNum("warning"), 1)
.BiocCheck$zero()

BiocCheck:::checkDataFileSizes(.bioctest)
expect_equivalent(.BiocCheck$getNum("warning"), 1)
.BiocCheck$zero()
unlink(temp_dir, recursive = TRUE)
assignInNamespace('.findLargeFiles', .findLargeFiles_org, "BiocCheck")

# checkBBScompatibility ---------------------------------------------------

temp_dir <- tempfile()
.bioctest <- create_test_package(
    test_dir = temp_dir,
    description = list(`Package ` = " foo", License = "GPL-2")
)
BiocCheck:::checkBBScompatibility(.bioctest)
checkCounter("Space in DESCRIPTION field name doesn't cause error")
.BiocCheck$zero()

expect_error({
    create_test_package(
        test_dir = temp_dir,
        extraActions = function(pkgdir) {
            badfile <- file.path(pkgdir, "DESCRIPTION")
            writeLines(c(
                "Package: foo",
                "Version: 0.99.0",
                ## blank line causes error
                "",
                "License: GPL-2",
                "Depends: R (>= 4.0.0)"
            ), badfile)
        }
    )
}, info = "Blank line in DESCRIPTION doesn't cause error")

.bioctest <- read_test_package("testpkg0")
BiocCheck:::.conciseDescription(.bioctest)
checkCounter(
    "Description field in the DESCRIPTION file is too concise",
    "warning"
)

.bioctest <- create_test_package(
    test_dir = temp_dir,
    description = list(
        Package = "Foo",
        Description = c(
            "This is a test description field in the Foo package.",
            " The field should contain three sentences; otherwise, ",
            " it triggers a NOTE in the check."
        ),
        License = "GPL-2"
    )
)
BiocCheck:::.conciseDescription(.bioctest)
checkCounter(
    "The Description field in the DESCRIPTION is less than two sentences",
    "note"
)

.bioctest <- create_test_package(
    test_dir = temp_dir,
    description = list(Package = "Foo", License = "GPL-2")
)
BiocCheck:::checkBBScompatibility(.bioctest)
checkCounter(
    "Package name which doesn't match dir name does not cause error!"
)

expect_error({
    create_test_package(
        test_dir = temp_dir,
        ## override usethis::create_package Version
        extraActions = function(path) {
            badfile <- file.path(path, "DESCRIPTION")
            writeLines(c(
                "Package: foo",
                "License: GPL-2",
                "Depends: R (>= 4.0.0)"
            ), badfile)
        }
    )
}, info = "Missing Version doesn't cause error!")

.bioctest <- create_test_package(
    test_dir = temp_dir,
    pkgpath = file.path(temp_dir, "Foo"),
    extraActions = function(path) {
        badfile <- file.path(path, "DESCRIPTION")
        writeLines(c(
            "Package: Foo",
            "Version: 0.99.0",
            "Authors@R: 1",
            "License: GPL-2",
            "Depends: R (>= 4.0.0)"
        ), badfile)
    }
)
BiocCheck:::checkBBScompatibility(.bioctest)
checkCounter("Wrong class in Authors@R doesn't cause error!")

.bioctest <- create_test_package(
    test_dir = temp_dir,
    description = list(
        Version = "0.99.0",
        `Authors@R` = c(
            "c(person('Bioconductor', 'Package Maintainer',",
            "email='maintainer@bioconductor.org', role=c('aut')))"
        ),
        License = "GPL-2"
    ),
    use.canned = FALSE
)
BiocCheck:::checkBBScompatibility(.bioctest)
checkCounter("Missing cre role in Authors@R doesn't cause error!")
.BiocCheck$zero()


.bioctest <- create_test_package(
    test_dir = temp_dir,
    description = list(Version = "0.99.0", License = "GPL-2"),
    use.canned = FALSE
)
.desc <- .bioctest$DESCRIPTION
.bioctest$DESCRIPTION <-
    .desc[, colnames(.desc) != "Authors@R", drop = FALSE]
BiocCheck:::checkBBScompatibility(.bioctest)
checkCounter("Missing Maintainer and Authors@R doesn't cause error!")

.bioctest <- create_test_package(
    test_dir = temp_dir,
    description = list(
        `Authors@R` = "person('Joe', 'Blow', role = c('aut', 'cre'))",
        License = "GPL-2"
    ),
    use.canned = FALSE
)
BiocCheck:::checkBBScompatibility(.bioctest)
expect_true(
    .BiocCheck$getNum("error") > 0L,
    "Missing email in Maintainer doesn't cause error!"
)
.BiocCheck$zero()

.bioctest <- create_test_package(
    test_dir = temp_dir,
    description = list(
        Version = "0.99.0",
        `Authors@R` = c(
            "person('Bioconductor Package Maintainer',",
            "email='maintainer@bioconductor.org', role=c('aut', 'cre'))"
        ),
        License = "GPL-2"
    )
)
BiocCheck:::checkBBScompatibility(.bioctest)
expect_true(
    any(
        grepl(
            pattern = "Authors@R field not Author",
            .BiocCheck$get("error")[["validMaintainer"]]
        )
    )
)
expect_true(
    any(
        grepl(
            pattern = "adding .* ORCID iD",
            .BiocCheck$get("note")[["checkBBScompatibility"]]
        )
    )
)
.BiocCheck$zero()

.bioctest <- create_test_package(
    test_dir = temp_dir,
    description = list(
        Version = "0.99.0",
        `Authors@R` = c(
            "person('Bioconductor Package Maintainer',",
            "email='maintainer@bioconductor.org', role=c('aut', 'cre'),",
            "comment = c(ORCID = '0000-000-0000-0000'))"
        ),
        License = "GPL-2"
    )
)
BiocCheck:::checkBBScompatibility(.bioctest)
expect_true(
    grepl(
        pattern = "Invalid ORCID iD",
        .BiocCheck$get("note")[["checkBBScompatibility"]]
    )
)
.BiocCheck$zero()

.bioctest <- create_test_package(
    test_dir = temp_dir,
    description = list(
        Version = "0.99.0",
        Maintainer = "Joe Blow <joe@blow.com>",
        License = "GPL-2"
    )
)
BiocCheck:::checkBBScompatibility(.bioctest)
expect_true(
    grepl(
        pattern = "Remove Maintainer field",
        .BiocCheck$get("error")[["checkBBScompatibility"]]
    )
)
expect_true(
    grepl(
        pattern = "Use Authors@R",
        .BiocCheck$get("error")[["validMaintainer"]]
    )
)
.BiocCheck$zero()

.bioctest <- create_test_package(
    test_dir = temp_dir,
    description = list(
        Version = "0.99.0",
        `Authors@R` = c(
            "c(person('Bioconductor Package Maintainer',",
            "email='maintainer@bioconductor.org', role=c('aut', 'cre'),",
            "comment = c(ORCID = '0000-0000-000-0000')),",
            "person('Joe', 'Blow', email='joe@blow.com', role='cre'))"
        ),
        License = "GPL-2"
    )
)
BiocCheck:::checkBBScompatibility(.bioctest)
## ERROR: More than one maintainer & NOTE: Include ORCID iD
expect_identical(
    .BiocCheck$getNum(c("error", "warning", "note")),
    c(error = 2L, warning = 0L, note = 1L)
)
.BiocCheck$zero()

.bioctest <- create_test_package(
    test_dir = temp_dir,
    extraActions = function(path) {
        dir.create(file.path(path, "inst"))
        cat(
            paste(
                "citEntry(entry='article', title='test',",
                "author=personList(as.person('Lori Kern')), year=2020,",
                "journal='Loris Best', volume='4', issue='12',",
                "textVersion='Shepherd, Lori (2020) test. ",
                "Loris Best. 4(12)')"
            ),
            file = file.path(path,"inst", "CITATION")
        )
    }
)
BiocCheck:::checkForCitationFile(.bioctest)
expect_equivalent(.BiocCheck$getNum("warning"), 1)
.BiocCheck$zero()

.bioctest <- create_test_package(
    test_dir = temp_dir,
    extraActions = function(path) {
        dir.create(file.path(path, "inst"))
        cat(
            paste(
                "citEntry(entry='', title='test',",
                "author=personList(as.person('Lori Kern')), year=2020,",
                "journal='Loris Best', volume='4', issue='12')"
            ),
            file = file.path(path,"inst", "CITATION")
        )
    }
)
BiocCheck:::checkForCitationFile(.bioctest)
expect_equivalent(.BiocCheck$getNum("warning"), 1)
.BiocCheck$zero()
unlink(temp_dir, recursive = TRUE)

# checkUnitTests ----------------------------------------------------------
BiocCheck:::checkUnitTests(UNIT_TEST_TEMPDIR)
expect_equivalent(.BiocCheck$getNum("note"), 1)
.BiocCheck$zero()

.bioctest <- create_test_package(
    extraActions = function(path) {
        dir.create(file.path(path, "tests"), recursive = TRUE)
        cat("nothing", file = file.path(path, "tests", "foo.R"))
    }
)
BiocCheck:::checkUnitTests(.bioctest$sourceDir)
expect_true(stillZero())
.BiocCheck$zero()

BiocCheck:::checkSkipOnBioc(
    system.file("testpackages", "testpkg0", package="BiocCheck")
)
expect_equivalent(.BiocCheck$getNum("note"), 1)
.BiocCheck$zero()

# check coding practice ---------------------------------------------------
.bioctest <- read_test_package("testpkg0")
msg_sapply <- BiocCheck:::checkSapply(.bioctest)
expect_equivalent(length(msg_sapply), 1)

msg_seq <- BiocCheck:::check1toN(.bioctest)
expect_equivalent(length(msg_seq), 1)

msg_eda <- BiocCheck:::checkExternalData(.bioctest)
expect_equivalent(length(msg_eda), 4)

msg_dl <- BiocCheck:::checkOnAttachLoadCalls(.bioctest)
expect_equivalent(length(msg_dl), 2)

avail_pkgs <- BiocManager::available()
msg_sc <- BiocCheck:::checkSingleColon(.bioctest, avail_pkgs)
testval <- ifelse("BiocCheck" %in% avail_pkgs, 1, 0)
expect_equivalent(length(msg_sc), testval)

.bioctest <- read_test_package("testpkg2")
res <- BiocCheck:::findSymbolsInVignettes(.bioctest, c("T", "F"), "SYMBOL")
expect_equivalent(length(res), 1)

# findPasteInSignaler -----------------------------------------------------
rfile <- tempfile()
writeLines(c(
    "message(paste('foo', 'bar'))",
    "message(paste('foo', x))",
    "message(paste(x, collapse = '\t'))",
    "message('foo', paste(x, collapse = 't'))"
), rfile)
expect_true(
    length(BiocCheck:::.findPasteInSignaler(rfile)) == 2L
)

# findSignalerInSignaler --------------------------------------------------
.SIGNALERS_TXT <- c("message", "warning", "stop")
rfile <- tempfile()
writeLines(c(
    "message('warning: see here')",
    "warning('error here')",
    "stop('ErrOR: see here')",
    "stop('message here')"
), rfile)
expect_true(
    length(BiocCheck:::.findSignalerInSignaler(rfile, .SIGNALERS_TXT)) == 4L
)

# .tryInstallwLoad --------------------------------------------------------
.bioctest <- create_test_package()
temppkg <- BiocCheck:::.tryInstallwLoad(.bioctest)
liblocation <- file.path(temppkg, "lib")
expect_true(dir.exists(liblocation))
expect_true(
    identical(
        readLines(file.path(temppkg, "install.stderr")),
        character(0L)
    )
)
testloadEnv <- try(
    loadNamespace(.bioctest$packageName, lib.loc = liblocation)
)
expect_true(is.environment(testloadEnv))
unloadNamespace(testloadEnv)
unlink(temppkg, recursive = TRUE)

# packageName -------------------------------------------------------------
## test tarball rename
.bioctest <- create_test_package(description = list(Version = "0.99.0"))
oldname <- devtools::build(.bioctest$sourceDir)
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
expect_identical(
    .BiocPackage$initialize(newname)[["packageName"]],
    .bioctest$packageName
)
file.remove(newname)

# checkDeprecatedPackages -------------------------------------------------
temp_dir <- tempfile()

.bioctest <- create_test_package(
    test_dir = temp_dir,
    description = list(Depends = "multicore, unitTestTempDir")
)
BiocCheck:::checkDeprecatedPackages(.bioctest)
checkCounter("Depending on multicore didn't cause error!")
unlink(temp_dir, recursive = TRUE)

# parseFile ---------------------------------------------------------------
temp_dir <- tempfile()
.bioctest <- create_test_package(
    test_dir = temp_dir,
    extraActions = function(path) {
        testFile <- file.path(path, "R", "testfile.R")
        cat("1 + 1", file = testFile)
    }
)
df <- BiocCheck:::parseFile(.bioctest, .bioctest$RSources)
expect_identical(dim(df), c(6L,9L))
## test that  testpkg0_child.Rmd is read in using `child =` chunk
## in testpkg0.Rmd
.bioctest <- read_test_package("testpkg0")
incl <- BiocCheck:::parseFile(
    .bioctest, .bioctest$VigSources["vignettes/testpkg0.Rmd"]
)
expect_true(all(c("2", "+", "1") %in% incl[, "text"]))
unlink(temp_dir, recursive = TRUE)

# checkForBrowser ---------------------------------------------------------
.bioctest <- read_test_package("testpkg0")
parsedCode <- BiocCheck:::parseFiles(.bioctest)
res <- BiocCheck:::findSymbolsInParsedCode(
    parsedCodeList = parsedCode,
    symbolNames = "browser",
    tokenTypes = "SYMBOL_FUNCTION_CALL"
)
expect_equivalent(length(res), 1)
.BiocCheck$zero()

# findSymbolsInRFiles -----------------------------------------------------
.bioctest <- read_test_package("testpkg0")
msg <- BiocCheck:::findSymbolsInRFiles(
    .bioctest, BiocCheck:::.BAD_INSTALL_CALLS, "SYMBOL_FUNCTION_CALL"
)
tinytest::expect_match(
    msg, "update\\.packages\\(\\)|install\\(\\)"
)
.BiocCheck$zero()

# checkCatInRCode ---------------------------------------------------------
.bioctest <- read_test_package("testpkg0")
msg <- BiocCheck:::checkCatInRCode(.bioctest, c("cat", "print"))
expect_equivalent(length(msg), 9)
.BiocCheck$zero()

# checkDepDefInRCode ------------------------------------------------------
.bioctest <- read_test_package("testpkg0")
msg <- BiocCheck:::findSymbolsInRFiles(
    .bioctest, c(".Deprecated", ".Defunct"), "SYMBOL_FUNCTION_CALL"
)
expect_equivalent(length(msg), 2L)
.BiocCheck$zero()

# checkEqInAssignment -----------------------------------------------------
.bioctest <- read_test_package("testpkg0")
msg <- BiocCheck:::checkEqInAssignment(
    .bioctest, symbol = "=", tokenType = "EQ_ASSIGN"
)
expect_equivalent(length(msg), 3)
.BiocCheck$zero()

# checkVigInstalls --------------------------------------------------------
.bioctest <- read_test_package("testpkg0")
BiocCheck:::checkVigInstalls(.bioctest)
expect_equivalent(.BiocCheck$getNum("error"), 1)
tinytest::expect_match(
    .BiocCheck$get("error")[[1L]][1L],
    "Package installation calls"
)
.BiocCheck$zero()

# checkDupChunkLabels -----------------------------------------------------
vigfile <-  system.file(
    "testpackages", "testpkg0", "vignettes", "dupChunks.Rmd",
    package="BiocCheck", mustWork = TRUE
)
BiocCheck:::checkDupChunkLabels(vigfile)
expect_equivalent(
    length(.BiocCheck$get("error")[["checkDupChunkLabels"]]), 3
)
checkCounter("Duplicate chunk labels didn't cause error!")

# checkTFSymbolUsage ------------------------------------------------------
.bioctest <- read_test_package("testpkg0")
BiocCheck:::checkTFSymbolUsage(.bioctest)
expect_equivalent(.BiocCheck$getNum("warning"), 1)
.BiocCheck$zero()

# checkVigSessionInfo -----------------------------------------------------
.bioctest <- read_test_package("testpkg0")
BiocCheck:::checkVigSessionInfo(.bioctest)
expect_equivalent(.BiocCheck$getNum("note"), 1)
.BiocCheck$zero()

# checkForInstall ---------------------------------------------------------
.bioctest <- read_test_package("testpkg0")
parsedCode <- BiocCheck:::parseFiles(.bioctest)
res <- BiocCheck:::findSymbolInParsedCode(
    parsedCode, "testpkg0", "install", "SYMBOL_FUNCTION_CALL"
)
expect_equivalent(res, 2)

# checkVigBiocInst --------------------------------------------------------
.bioctest <- read_test_package("testpkg0")
BiocCheck:::checkVigBiocInst(.bioctest)
expect_true(.BiocCheck$getNum("warning") == 1)
.BiocCheck$zero()

# checkClassNEEQLookup ----------------------------------------------------
temp_dir <- tempfile()
.bioctest <- create_test_package(
    test_dir = temp_dir,
    extraActions = function(path) {
        Rdir <- file.path(path, "R")
        if (!dir.exists(Rdir))
            dir.create(Rdir)
        fl <- tempfile(tmpdir = Rdir, fileext = ".R")
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
    }
)
match <- BiocCheck:::checkClassNEEQLookup(.bioctest)
expect_identical(4L, length(match))
unlink(temp_dir, recursive = TRUE)

# checkDESCRIPTIONfile ----------------------------------------------------
dcf <- matrix("https://example.com", dimnames = list(NULL, "URL"))
BiocCheck:::checkDESCfields(dcf)
expect_equivalent(.BiocCheck$getNum("note"), 1)
.BiocCheck$zero()

dcf <- matrix("https://example.com", dimnames = list(NULL, "BugReports"))
BiocCheck:::checkDESCfields(dcf)
expect_equivalent(.BiocCheck$getNum("note"), 1)
.BiocCheck$zero()

dcf <- matrix(
    c("https://example.com", "https://example.com"), nrow = 1,
    dimnames = list(NULL, c("BugReports", "URL"))
)
BiocCheck:::checkDESCfields(dcf)
expect_equivalent(.BiocCheck$getNum("note"), 0)
.BiocCheck$zero()

dcf <- matrix(
    c("https://example.com", "https://example.com"), nrow = 1,
    dimnames = list(NULL, c("BugReports", "URL"))
)
BiocCheck:::checkBiocDepsDESC(dcf)
expect_equivalent(.BiocCheck$getNum("warning"), 1)
.BiocCheck$zero()

dcf <- matrix(
    "S4Vectors (== 0.99.0)", nrow = 1,
    dimnames = list(NULL, "Depends")
)
BiocCheck:::checkBiocDepsDESC(dcf)
expect_equivalent(.BiocCheck$getNum("warning"), 0)
.BiocCheck$zero()

dcf <- matrix(
    "S4Vectors (== 0.99.0)", nrow = 1,
    dimnames = list(NULL, "Depends")
)
BiocCheck:::checkPinnedDeps(dcf)
expect_equivalent(.BiocCheck$getNum("error"), 1)
.BiocCheck$zero()

# remotesUsage ------------------------------------------------------------
.bioctest <- read_test_package("testpkg0")
BiocCheck:::checkRemotesUsage(.BiocPackage = .bioctest)
expect_equivalent(1, BiocCheck:::.BiocCheck$getNum("error"))
expect_true(grepl("Remotes:", BiocCheck:::.BiocCheck$get("error")[1]))
.BiocCheck$zero()
.bioctest <- read_test_package("testpkg1")
BiocCheck:::checkRemotesUsage(.bioctest)
expect_equivalent(0, BiocCheck:::.BiocCheck$getNum("error"))
.BiocCheck$zero()

# LazyDataUsage -----------------------------------------------------------
.bioctest <- read_test_package("testpkg0")
BiocCheck:::checkLazyDataUsage(.bioctest)
expect_equivalent(1, BiocCheck:::.BiocCheck$getNum("note"))
.BiocCheck$zero()

# checkForLibraryRequire -------------------------------------------------
.bioctest <- read_test_package("testpkg0")
msg <- BiocCheck:::checkForLibraryRequire(.bioctest)
expect_equivalent(1L, .BiocCheck$getNum("warning"))
expect_equivalent(length(msg), 14L)
.BiocCheck$zero()

# getFunctionLengths ------------------------------------------------------
file <- system.file(
    "testpackages", "testpkg0", "R", "parseme.R",
    package = "BiocCheck"
)
df <- getParseData(parse(file, keep.source = TRUE))
res <- BiocCheck:::getFunctionLengths(df)
res0 <- tibble::tribble(
    ~length, ~startLine, ~endLine, ~codingLines,
    2,          1,        2,            2,
    1,          3,        3,            1,
    1,          6,        6,            1,
    5,          9,       13,            4,
    4,         16,       19,            4,
    6,         23,       28,            3,
    1,         31,       31,            1,
    1,         33,       33,            1,
    6,         35,       40,            2,
    7,         44,       50,            4
)
res0 <- apply(res0, 1L, force, simplify = FALSE)
names(res0) <- c(
    "_anonymous_.1", "fa", "f2", "f3", "f4", "_anonymous_.23",
    "f5", "f6", "f7", "f8"
)
expect_identical(res0, res)

# getFunctionLengths2 -----------------------------------------------------
## 1 function is greater than 50 lines long
.bioctest <- read_test_package("testpkg0")
parsedCode <- BiocCheck:::parseFiles(.bioctest)
res <- BiocCheck:::checkFunctionLengths(parsedCode, "testpkg0")
expect_equivalent(BiocCheck:::.BiocCheck$getNum("note"), 1)
expect_true(
    grepl(
        pattern = "There is 1 function greater than 50 lines",
        x = BiocCheck:::.BiocCheck$note$checkFunctionLengths[[1]]
    ),
    info = "Checking we report functions > 50 lines long."
)
.BiocCheck$zero()

# checkExportsAreDocumented ------------------------------------------------
.bioctest <- read_test_package("testpkg0")
instdir <- BiocCheck:::.tryInstallwLoad(.bioctest)
res <- BiocCheck:::checkExportsAreDocumented(
    .bioctest, lib.loc = file.path(instdir, "lib")
)
expect_equivalent(1, .BiocCheck$getNum("error"))
.BiocCheck$zero()
res <- BiocCheck:::checkUsageOfDont(.bioctest)
expect_equivalent(2, .BiocCheck$getNum("note"))
.BiocCheck$zero()

# checkNEWS ---------------------------------------------------------------
BiocCheck:::checkNEWS(system.file("testpackages", "testpkg0",
    package="BiocCheck"))
expect_equivalent(1, .BiocCheck$getNum("note"))
.BiocCheck$zero()
if (!dir.exists(UNIT_TEST_TEMPDIR))
    dir.create(UNIT_TEST_TEMPDIR)
cat("lalala", file = file.path(UNIT_TEST_TEMPDIR, "NEWS"))
BiocCheck:::checkNEWS(UNIT_TEST_TEMPDIR)
stillZero()
unlink(file.path(UNIT_TEST_TEMPDIR, "NEWS"))
dir.create(file.path(UNIT_TEST_TEMPDIR, "inst"), FALSE)
cat("lalala", file = file.path(UNIT_TEST_TEMPDIR, "inst", "NEWS.Rd"))
BiocCheck:::checkNEWS(UNIT_TEST_TEMPDIR)
expect_equivalent(1, .BiocCheck$getNum("warning"))
.BiocCheck$zero()
cat("lalala", file = file.path(UNIT_TEST_TEMPDIR, "NEWS.md"))
BiocCheck:::checkNEWS(UNIT_TEST_TEMPDIR)
expect_equivalent(1, .BiocCheck$getNum("note"))
expect_equivalent(1, .BiocCheck$getNum("warning"))
.BiocCheck$zero()

# checkFormatting ---------------------------------------------------------
.bioctest <- read_test_package("testpkg0")
BiocCheck:::checkFormatting(.bioctest)
expect_equivalent(3, .BiocCheck$getNum("note"))
.BiocCheck$zero()

# checkForPromptComments -------------------------------------------------
.bioctest <- read_test_package("testpkg0")
BiocCheck:::checkForPromptComments(.bioctest)
expect_equivalent(1, .BiocCheck$getNum("note"))
.BiocCheck$zero()

# getPkgType -------------------------------------------------------------
temp_dir <- tempfile()
.bioctest <- create_test_package(
    test_dir = temp_dir, description = list(Foo = "bar")
)
expect_identical("", .bioctest$packageType)

.bioctest <- create_test_package(
    test_dir = temp_dir,
    description = list(biocViews = "bad, Software")
)
expect_identical(.bioctest$packageType, NA_character_)

.bioctest <- create_test_package(
    test_dir = temp_dir,
    description = list(biocViews = "DifferentialExpression, CellBiology")
)
expect_identical("Software", .bioctest$packageType)

.bioctest <- create_test_package(
    test_dir = temp_dir,
    description =
        list(biocViews = "DifferentialExpression, ChipManufacturer")
)
expect_identical(.bioctest$packageType, NA_character_)

.bioctest <- create_test_package(
    test_dir = temp_dir,
    description =
        list(biocViews = "GeneCardsCustomSchema, ChipManufacturer")
)
expect_identical("AnnotationData", .bioctest$packageType)

# Cancer is not a valid biocView, so return exception
.bioctest <- create_test_package(
    test_dir = temp_dir,
    description = list(biocViews = "Cancer, HapMap")
)
expect_identical(.bioctest$packageType, NA_character_)
unlink(temp_dir, recursive = TRUE)

# checkForBiocDevelSubscription -------------------------------------------
if (nchar(Sys.getenv("BIOC_DEVEL_PASSWORD"))) {

    temp_dir <- tempfile()
    .bioctest <- create_test_package(
        test_dir = temp_dir,
        description = list(Maintainer = "Joe Blow <foo@bar.com>")
    )
    BiocCheck:::checkForBiocDevelSubscription(.bioctest)
    checkCounter("Maintainer subscribed to the bioc-devel mailing list")

    .bioctest <- create_test_package(
        test_dir = temp_dir,
        description = list(
            Maintainer =
                "Bioconductor Maintainer <maintainer@bioconductor.org>"
        ),
        use.canned = FALSE
    )
    BiocCheck:::checkForBiocDevelSubscription(.bioctest)
    expect_true(stillZero())
    .BiocCheck$zero()

    .bioctest <- create_test_package(
        test_dir = temp_dir,
        description = list(
            Maintainer =
                "Bioconductor Maintainer <MAINTAINER@bioconductor.ORG>"
        ),
        use.canned = FALSE
    )
    BiocCheck:::checkForBiocDevelSubscription(.bioctest)
    expect_true(stillZero())
    .BiocCheck$zero()

    result_email <- BiocCheck:::getMaintainerEmail(.bioctest)
    expect_true(
        identical(result_email, "MAINTAINER@bioconductor.ORG")
    )

    .bioctest <- create_test_package(
        test_dir = temp_dir,
        description = list(
            Package = "uniTestTempDir",
            Version = "0.99.0",
            `Authors@R` = c("person('BioC', 'Maintainer',",
                "email = 'Maintainer@bioconductor.org',",
                "role = c('aut', 'cre'))"
            )
        ),
        use.canned = FALSE
    )
    result_email <- BiocCheck:::getMaintainerEmail(.bioctest)
    expect_identical(result_email, "Maintainer@bioconductor.org")

    .bioctest <- create_test_package(
        test_dir = temp_dir,
        description = list(
            Version = "0.99.0",
            `Authors@R` = c(
                "c(person('Joe', \n  'Blow', email='joe@blow.org',",
                "role=c('aut', 'cre')))"
            )
        ),
        use.canned = FALSE
    )
    BiocCheck:::checkForBiocDevelSubscription(.bioctest)
    checkCounter("Maintainer subscribed to the bioc-devel mailing list")

    .bioctest <- create_test_package(
        test_dir = temp_dir,
        description = list(
            Version = "0.99.0",
            `Authors@R` = c(
                "c(person('BioC', \n  'Maintainer',",
                "email='maintainer@bioconductor.org',",
                "role=c('aut', 'cre')))"
            )
        ),
        use.canned = FALSE
    )
    BiocCheck:::checkForBiocDevelSubscription(.bioctest)
    expect_true(stillZero())
    .BiocCheck$zero()
    unlink(temp_dir, recursive = TRUE)

}

# checkForSupportSiteRegistration ----------------------------------------
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
    .BiocCheck$zero()
    BiocCheck:::checkSupportReg("lori.shepherd@roswellpark.org")
    expect_true(stillZero())
    BiocCheck:::checkSupportReg("foo@bar.com")
    expect_equivalent(.BiocCheck$getNum("error"), 1)
    .BiocCheck$zero()
    ## api is case insensitivy
    BiocCheck:::checkSupportReg("lori.shePhErd@roswellpark.org")
    expect_true(stillZero())


    # tags
    BiocCheck:::checkWatchedTag(
        "lori.shepherd@roswellpark.org", "biocfilecache"
    )
    expect_true(stillZero())
    BiocCheck:::checkWatchedTag(
        "lori.shepherd@roswellpark.org", "unwatchedpackage"
    )
    expect_equivalent(.BiocCheck$getNum("error"), 1)
    .BiocCheck$zero()
    ## email is case insensitive
    BiocCheck:::checkWatchedTag(
        "lori.shePherd@rosWellpark.org", "biocfilecache"
    )
    expect_true(stillZero())
    ## check tag is case insenstive
    BiocCheck:::checkWatchedTag(
        "lori.shepherd@rosWellpark.org", "bioCfiLecache"
    )
    expect_true(stillZero())

}

# checkForVersionNumberMismatch -------------------------------------------
temp_dir <- tempfile()
.bioctest <- create_test_package(
    test_dir = temp_dir,
    description = list(Version="0.0.1")
)
pkgname <- .bioctest$packageName

oldname <- devtools::build(.bioctest$sourceDir)

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
.newbioctest <- .BiocPackage$initialize(newname)

BiocCheck:::checkForVersionNumberMismatch(
    .newbioctest
)
expect_equivalent(.BiocCheck$getNum("error"), 1)
.BiocCheck$zero()
unlink(temp_dir, recursive = TRUE)

# checkForDirectSlotAccess ------------------------------------------------
temp_dir <- tempfile()
.bioctest <- create_test_package(
    test_dir = temp_dir,
    description = list(VignetteBuilder="knitr"),
    extraActions = function(pkgpath) {
        vigdir <- file.path(pkgpath, "vignettes")
        if (!dir.exists(vigdir))
            dir.create(vigdir, recursive = TRUE)
        file.copy(
            system.file(
                "testfiles", "directSlotAccess.Rmd",
                package = "BiocCheck", mustWork = TRUE
            ),
            file.path(vigdir, "directSlotAccess.Rmd")
        )
        file.copy(
            system.file(
                "testfiles", "noDirectSlotAccess.Rmd",
                package = "BiocCheck", mustWork = TRUE
            ),
            file.path(vigdir, "noDirectSlotAccess.Rmd")
        )
    }
)
pkgpath <- .bioctest$sourceDir

parsedCode <- list(
    FooBar = BiocCheck:::parseFile(
        .bioctest, .bioctest$VigSources[1]
    )
)
res <- BiocCheck:::checkForDirectSlotAccess(parsedCode, pkgpath)
expect_equivalent(BiocCheck:::.BiocCheck$getNum("note"), 1)
.BiocCheck$zero()
parsedCode <- list(
    FooBar = BiocCheck:::parseFile(
        .bioctest, .bioctest$VigSources[2]
    )
)
res <- BiocCheck:::checkForDirectSlotAccess(parsedCode, pkgpath)
expect_equivalent(.BiocCheck$getNum("note"), 0)
.BiocCheck$zero()
unlink(temp_dir, recursive = TRUE)

# checkRVersionDependency -------------------------------------------------
temp_dir <- tempfile()
.bioctest <- create_test_package(
    test_dir = temp_dir,
    description = list(Depends = "R (>= 1.0.0)")
)
BiocCheck:::checkRVersionDependency(.bioctest)
expect_equivalent(.BiocCheck$getNum("note"), 1)
.BiocCheck$zero()

.bioctest <- create_test_package(
    test_dir = temp_dir,
    description = list(Depends = "R")
)
BiocCheck:::checkRVersionDependency(.bioctest)
expect_equivalent(.BiocCheck$getNum("note"), 0)
.BiocCheck$zero()

.bioctest <- create_test_package(
    test_dir = temp_dir,
    description = list(Imports = "foobar)")
)
BiocCheck:::checkRVersionDependency(.bioctest)
expect_equivalent(.BiocCheck$getNum("warning"), 0)
.BiocCheck$zero()

.bioctest <- create_test_package(
    test_dir = temp_dir,
    description = list(Depends = "R (>= 10000.0.0)")
)
BiocCheck:::checkRVersionDependency(.bioctest)
expect_equivalent(.BiocCheck$getNum("note"), 0)
.BiocCheck$zero()
unlink(temp_dir, recursive = TRUE)

# doesManPageHaveRunnableExample -------------------------------------------
good <- tools::parse_Rd(system.file("testpackages", "testpkg0", "man",
        "has-devel.Rd", package = "BiocCheck"))

bad <- tools::parse_Rd(system.file("testpackages", "testpkg0", "man",
        "baddep.Rd", package = "BiocCheck"))

expect_true(BiocCheck:::doesManPageHaveRunnableExample(good))

expect_false(BiocCheck:::doesManPageHaveRunnableExample(bad))

# checkForValueSection ----------------------------------------------------
.bioctest <- read_test_package("testpkg0")
mans <-
    BiocCheck:::.read_all_rds(.bioctest$manSources, .bioctest$usesRdpack)
tags <- lapply(mans, BiocCheck:::.RdTags)
expect_true(!BiocCheck:::.valueInParsedRd(mans[[1]], tags[[1]]))
expect_true(!BiocCheck:::.valueInParsedRd(mans[[2]], tags[[2]]))
expect_true(BiocCheck:::.valueInParsedRd(mans[[3]], tags[[3]]))

.bioctest <- read_test_package("testpkg1")
mans <-
    BiocCheck:::.read_all_rds(.bioctest$manSources, .bioctest$usesRdpack)
tags <- lapply(mans, BiocCheck:::.RdTags)
expect_true(!BiocCheck:::.valueInParsedRd(mans[[1]], tags[[1]]))
expect_true(!BiocCheck:::.valueInParsedRd(mans[[2]], tags[[2]]))

# packageAlreadyExists -----------------------------------------------------
.BiocCheck$zero()
nerrors <- 0L
BiocCheck:::checkIsPackageNameAlreadyInUse("GenomicRanges", "CRAN")
expect_equivalent(.BiocCheck$getNum("error"), nerrors)
BiocCheck:::checkIsPackageNameAlreadyInUse("devtools", "CRAN")
expect_equivalent(.BiocCheck$getNum("error"), nerrors)
BiocCheck:::checkIsPackageNameAlreadyInUse("GenomicRanges", "BioCsoft")
nerrors <- nerrors + 1L
expect_equivalent(.BiocCheck$getNum("error"), nerrors)
BiocCheck:::checkIsPackageNameAlreadyInUse("gwascatData", "BioCann")
nerrors <- nerrors + 1L
expect_equivalent(.BiocCheck$getNum("error"), nerrors)
BiocCheck:::checkIsPackageNameAlreadyInUse("TENxBrainData", "BioCexp")
nerrors <- nerrors + 1L
expect_equivalent(.BiocCheck$getNum("error"), nerrors)
BiocCheck:::checkIsPackageNameAlreadyInUse("annotation", "BioCworkflows")
nerrors <- nerrors + 1L
expect_equivalent(.BiocCheck$getNum("error"), nerrors)
BiocCheck:::checkIsPackageNameAlreadyInUse("GenomicRanges", "BioCexp")
expect_equivalent(.BiocCheck$getNum("error"), nerrors)
BiocCheck:::checkIsPackageNameAlreadyInUse("GenomicRanges", "BioCann")
expect_equivalent(.BiocCheck$getNum("error"), nerrors)
BiocCheck:::checkIsPackageNameAlreadyInUse("GenomicRanges", "BioCworkflows")
expect_equivalent(.BiocCheck$getNum("error"), nerrors)
BiocCheck:::checkIsPackageNameAlreadyInUse("ImNotFound", "BioCexp")
expect_equivalent(.BiocCheck$getNum("error"), nerrors)
BiocCheck:::checkIsPackageNameAlreadyInUse("ImNotFound", "BioCann")
expect_equivalent(.BiocCheck$getNum("error"), nerrors)
BiocCheck:::checkIsPackageNameAlreadyInUse("ImNotFound", "BioCworkflows")
expect_equivalent(.BiocCheck$getNum("error"), nerrors)
BiocCheck:::checkIsPackageNameAlreadyInUse("ImNotFound", "CRAN")
expect_equivalent(.BiocCheck$getNum("error"), nerrors)
BiocCheck:::checkIsPackageNameAlreadyInUse("ImNotFound", "BioCsoft")
expect_equivalent(.BiocCheck$getNum("error"), nerrors)
.BiocCheck$zero()

# BiocCheckReporters -------------------------------------------------------
pkgdir <- system.file("testpackages", package="BiocCheck")
hypo_checkdir <- file.path(pkgdir, "hypoPkg.BiocCheck")
oldCheckDir <- .BiocCheck$metadata$BiocCheckDir
.BiocCheck$metadata$BiocCheckDir <- hypo_checkdir
.BiocCheck$report(debug = FALSE, isOnBBS = TRUE)
expect_true(
    !dir.exists(hypo_checkdir)
)
expect_true(
    !dir.exists(hypo_checkdir)
)
.BiocCheck$metadata$BiocCheckDir <- oldCheckDir

# checkUsageOfDont ---------------------------------------------------------
## testpkg0 should trigger this note for 2 out of 3 man pages
.bioctest <- read_test_package("testpkg0")
BiocCheck:::.tryInstallwLoad(.bioctest)
notemsg <- capture.output(
    BiocCheck:::checkUsageOfDont(.bioctest), type = "message"
)
expect_equivalent(2, .BiocCheck$getNum("note"))
# here we verify the correct number of pages were detected
expect_true( any(grepl("67%", notemsg)) )
.BiocCheck$zero()

## testpkg1 contains a man page with keyword 'internal'
## this shouldn't trigger the note
.bioctest <- read_test_package("testpkg1")
BiocCheck:::.tryInstallwLoad(.bioctest)
BiocCheck:::checkUsageOfDont(.bioctest)
expect_equivalent(0, .BiocCheck$getNum("note"))
.BiocCheck$zero()

# checkORCID ---------------------------------------------------------------
orcid <- c(
    "0000-0001-6197-3471",
    "0000-0001-6197-347X",
    "0000-0001-6197-34XX",
    "0000-0001-6197-3471-0000",
    "",
    NA_character_
)
valid <- c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE)
expect_identical(valid, BiocCheck:::.checkORCID(orcid))

# getBiocCheckDir <- function() {
.bioctest <- read_test_package("testpkg0")
expect_identical(
    basename(.bioctest$BiocCheckDir), "testpkg0.BiocCheck"
)
expect_identical(
    basename(dirname(.bioctest$BiocCheckDir)),
    "testpackages"
)

# getDirFiles --------------------------------------------------------------
vigfiles <- list.files(
    system.file(
        "testpackages", "testpkg0", "vignettes", package="BiocCheck"
    ),
    full.names = TRUE
)
targets <- file.path(basename(dirname(vigfiles)), basename(vigfiles))
expect_identical(
    targets[1], unname(BiocCheck:::.getDirFiles(vigfiles[1]))
)
expect_identical(targets, unname(BiocCheck:::.getDirFiles(vigfiles)))
