#' @importFrom tools file_path_sans_ext file_ext parse_Rd Rd2ex
#' @importFrom utils capture.output data getParseData head packageDescription
#'   packageVersion globalVariables readCitationFile
#' @importFrom stringdist stringdistmatrix
#' @importFrom knitr purl
#' @importFrom BiocManager available install repositories version
#' @import biocViews methods

# Checks for BiocCheck ----------------------------------------------------

checkForVersionNumberMismatch <- function(package, package_dir)
{
    bn <- basename(package)
    bn <- sub(".tar.gz", "", bn, TRUE)
    ver <- strsplit(bn, "_")[[1]][2]
    dcf <- read.dcf(file.path(package_dir, "DESCRIPTION"))
    dcfVer <- unname(dcf[, "Version"])
    if (!ver == dcfVer)
    {
        handleError(
            "Version number in tarball filename must match Version field ",
            "in DESCRIPTION. (Tip: create tarball with R CMD build)")
    }
}

checkDeprecatedPackages <- function(pkgdir)
{
    allDepends <- getAllDependencies(pkgdir)
    allDeprecated <- getAllDeprecatedPkgs()
    if ("multicore" %in% allDepends)
    {
        handleError("Use 'BiocParallel' instead of 'multicore'. ",
            "'multicore' is deprecated and does not work on Windows."
        )
    }
    logVec <- allDeprecated %in% allDepends
    if (any(logVec)){
        handleError(
            "Package dependency in the DESCRIPTION is 'Deprecated'. ",
            "Update your package to not rely on the following:",
            messages = allDeprecated[logVec]
        )
    }
}

checkRemotesUsage <- function(pkgdir)
{
    dcf <- read.dcf(file.path(pkgdir, "DESCRIPTION"))
    if ("Remotes" %in% colnames(dcf))
        handleError(
            "Package dependencies must be on CRAN or Bioconductor.",
            " Remove 'Remotes:' from DESCRIPTION"
        )
}

checkLazyDataUsage <- function(pkgdir)
{
    dcf <- read.dcf(file.path(pkgdir, "DESCRIPTION"))
    if ("LazyData" %in% colnames(dcf) &&
        tools:::str_parse_logic(dcf[, "LazyData"]))
        handleNote(
            "'LazyData:' in the 'DESCRIPTION' should be set to false or removed"
        )
}

checkNewPackageVersionNumber <- function(pkgdir)
{
    dcf <- read.dcf(file.path(pkgdir, "DESCRIPTION"))
    version <- dcf[, "Version"]
        if (!grepl("^0+[-.][0-9]+[-.][0-9]+$", version))
            handleWarning(
                "New package x version starting with non-zero value ",
                "(e.g., 1.y.z, 2.y.z); got ", sQuote(version), ".")
        if(!grepl("^[0-9]+[-.]99[-.][0-9]+$", version))
            handleError(
                "New package 'y' version not 99 (i.e., x.99.z)",
                "; Package version: ", version
            )
}

checkVersionNumber <- function(pkgdir)
{
    dcf <- read.dcf(file.path(pkgdir, "DESCRIPTION"))
    version <- dcf[, "Version"]
    regex <- "^[0-9]+[-\\.]([0-9]+)[-\\.][0-9]+$"
    if(!grepl(regex, version))
    {
        handleError(
            "Invalid package Version, see ",
            "https://contributions.bioconductor.org/versionnum.html"
        )
        return()
    }
    tryCatch({
        pv <- package_version(version)
        x <- pv$major
        y <- pv$minor
        mod <- y %% 2
        isDevel <- identical(
            BiocManager:::.version_bioc("devel"), BiocManager::version()
        )
        bioc.mod <- ifelse(isDevel, 1, 0)
        if (x == 0) {
            handleMessage("Package version ", as.character(pv), "; pre-release")
        } else if (mod != bioc.mod) {
            shouldBe <- ifelse(isDevel, "odd", "even")
            vers <- ifelse(isDevel, "devel", "release")
            handleWarning(
                "y of x.y.z version should be ", shouldBe, " in ", vers)
        }

    }, error = function(e) {
        handleError("Invalid package version")
    })
}

checkRVersionDependency <- function(package_dir) {
    desc <- file.path(package_dir, "DESCRIPTION")
    dcf <- read.dcf(desc)
    if ("Depends" %in% colnames(dcf))
    {
        res <- cleanupDependency(dcf[, "Depends"], FALSE)
        if ("R" %in% res)
        {
            ind <- which(res == "R")
            verStr <- names(res)[ind]
            if (nchar(verStr))
            {
                pkgVer <- as.package_version(verStr)
                RVer <- package_version(
                    paste0(getRversion()[, c(1, 2)], ".0")
                )
                if (pkgVer < RVer)
                    handleNote(sprintf(
                        "Update R version dependency from %s to %s.",
                        pkgVer, RVer
                    ))
            }
        }
    }
}

checkPackageSize <- function(pkg, pkgdir, size=5){
    pkgType <- getPkgType(pkgdir)
    if (is.na(pkgType) ||  pkgType == "Software") {
        maxSize <- size*10^6 ## 5MB
        pkgSize <- file.size(pkg)
        if (pkgSize > maxSize){
            msgs <- c(
                paste0("Package Size: ",
                       as.character(round(pkgSize/(10^6),2)), " MB"),
                paste0("Size Requirement: ",
                       sprintf("%.2f", round(maxSize/(10^6),2)), " MB")
            )
            handleError(
                "Package tarball exceeds the Bioconductor size requirement.",
                messages = msgs
            )
        }
    }
}

.MAX_FILE_SIZE <- 5*10^6 ## 5MB
.DATA_DIRS <- c("data", "inst/extdata", "data-raw")

.filter_data <- function(filedf, for_data = FALSE) {
    files <- filedf[["path"]]
    data_dirs <- paste0(.DATA_DIRS, "/")
    hitmat <- vapply(
        data_dirs,
        grepl,
        logical(length(files)),
        x = files,
        fixed = TRUE
    )
    hits <- as.logical(rowSums(hitmat))
    decision <- if (for_data) force else `!`
    filedf[decision(hits), , drop = FALSE]
}

.in_data <- function(f) { f %in% .DATA_DIRS }

.findLargeFiles <- function(pkgdir, data_only) {
    gitignore <- file.exists(file.path(pkgdir, ".gitignore"))
    if (requireNamespace("gert", quietly = TRUE) && gitignore) {
        fileinfo <- gert::git_ls(repo = pkgdir)
        fileinfo <- .filter_data(fileinfo, for_data = data_only)
        files <- unlist(
            fileinfo[fileinfo[["filesize"]] > .MAX_FILE_SIZE, "path"]
        )
        file.path(pkgdir, files)
    } else {
        folders <- list.dirs(pkgdir, full.names = FALSE, recursive = TRUE)
        decision <- if (data_only) force else Negate
        folders <- Filter(decision(.in_data), folders)
        files <- list.files(
            file.path(pkgdir, folders), full.names = TRUE, recursive = TRUE
        )
        filesizes <- file.size(files)
        files[filesizes > .MAX_FILE_SIZE]
    }
}

checkIndivFileSizes <- function(pkgdir)
{
    largefiles <- .findLargeFiles(pkgdir, data_only = FALSE)
    if (length(largefiles))
        handleWarning(
            "Package files exceed the 5MB size limit.",
            help_text = "Files over the limit: ",
            messages = largefiles
        )
}

checkDataFileSizes <- function(pkgdir) {
    largedata <- .findLargeFiles(pkgdir, data_only = TRUE)
    if (length(largedata))
        handleWarning(
            "Data files exceed the 5MB size limit.",
            help_text =
                "Use 'ExperimentHub' or 'AnnotationHub' for the files: ",
            messages = largedata
        )
}

.testRbuildignore <- function(text) {
    entries <- Filter(nchar,
        grep("^#", trimws(text), value = TRUE, invert = TRUE)
    )
    grepl("^\\^?(long)?tests[\\$\\/]?$", entries)
}

checkRbuildignore <- function(pkgdir) {
    rbuildfile <- file.path(pkgdir, ".Rbuildignore")
    if (file.exists(rbuildfile)) {
        rbuild <- readLines(rbuildfile)
        testIgnore <- .testRbuildignore(rbuild)
        if (any(testIgnore))
            handleError(
                ".Rbuildignore includes 'tests' or 'longtests' folder."
            )
    }
}

checkBiocCheckOutputFolder <- function(pkgdir, pkg_name) {
    alldirs <- list.dirs(pkgdir, full.names = FALSE)
    bioccheck_out_dir <- paste(pkg_name, "BiocCheck", sep = ".")
    if (bioccheck_out_dir %in% alldirs)
        handleError(
            "Remove '", bioccheck_out_dir, "' from the package directory"
        )
}

checkInstDocFolder <- function(pkgdir, pkg_name) {
    alldirs <- list.dirs(pkgdir, full.names = FALSE)
    instdocfiles <- list.files(file.path(pkgdir, "inst/doc"))
    if ("inst/doc" %in% alldirs && length(instdocfiles))
        handleError(
            "Remove 'inst/doc' folder from the package source"
        )
}

.readBiocViews <- function(pkgdir) {
    desc <- file.path(pkgdir, "DESCRIPTION")
    dcf <- read.dcf(desc)
    .parseBiocViews(dcf)
}

.parseBiocViews <- function(dcf) {
    if ("biocViews" %in% colnames(dcf))
        strsplit(dcf[, "biocViews"], "\\s*,\\s*")[[1]]
    else
        ""
}

checkBiocViews <- function(pkgdir)
{
    invalid <- FALSE
    handleCheck("Checking that biocViews are present...")
    views <- .readBiocViews(pkgdir)
    if (identical(length(views), 1L) && !nzchar(views)) {
        handleError("No biocViews terms found.")
        return(TRUE)
    }

    biocViewsVocab <- .load_data("biocViewsVocab", "biocViews")

    handleCheck("Checking package type based on biocViews...")
    type <- guessPackageType(views)
    handleMessage(type)
    handleCheck("Checking for non-trivial biocViews...")
    toplevel <- c("Software", "AnnotationData", "ExperimentData", "Workflow")
    if (all(views %in% toplevel)) {
        handleError(
            "Add biocViews other than ",
            paste(unique(views), collapse=", ")
        )
        return(TRUE)
    }

    parents <- vapply(views, getParent, character(1L), biocViewsVocab)
    parents <- Filter(nzchar, parents)

    handleCheck("Checking that biocViews come from the same category...")
    if (length(unique(parents)) > 1)
    {
        handleWarning("Use biocViews from one category only ",
            "(one of Software, ExperimentData, AnnotationData, Workflow)")
        return(TRUE)
    }
    branch <- unique(parents)
    # TODO: Fix this
    if (interactive())
        env <- environment()
    else
        env <- .GlobalEnv

    handleCheck("Checking biocViews validity...")
    if (!all(views %in% nodes(biocViewsVocab)))
    {
        badViews <- views[!(views %in% nodes(biocViewsVocab))]
        badViewsVec <- paste(badViews, collapse=", ")

        terms <- c(badViews, nodes(biocViewsVocab))
        distmat <- stringdistmatrix(terms, useNames="strings", method="lv")
        distmat <- as.matrix(distmat)
        distmat <- distmat > 0 & distmat < 3
        distmat[badViews, badViews] <- FALSE

        suggestedViews <- vapply(badViews, function(view) {
            alt <- colnames(distmat)[distmat[view,]]
            msg <- shQuote(view)
            if (length(alt)) {
                alt <- shQuote(alt)
                oneof <- if (length(alt) > 1L) "one of" else ""
                alt <- paste(oneof, paste(alt, collapse = ", "))
                msg <- paste0(msg, ": Did you mean", alt, "?")
            }
            msg
        }, character(1))

        handleWarning(
            "Invalid BiocViews term(s):", messages = unlist(suggestedViews)
        )
        invalid <- TRUE
    }

    if (packageVersion("biocViews") < package_version("1.33.9")) {
        ## conditional to keep the logic the same
        if (branch %in% c("Software", "AnnotationData", "ExperimentData"))
            branch <- gsub("data", "", tolower(branch))
    }

    handleCheck("Checking for recommended biocViews...")
    recommended <- try({
        recommendBiocViews(pkgdir, branch)[["recommended"]]
    }, silent = TRUE)

    if (!inherits(recommended, "try-error") && nzchar(recommended))
    {
        handleNote(
            "Consider adding these automatically suggested biocViews: ",
            recommended)
        invalid <- TRUE
    }
    return(invalid)
}

.checkDescription <- function(desc) {
    dcf <- read.dcf(desc)
    if ("Description" %in% colnames(dcf)) {
        desc_field <- dcf[, "Description"]
        desc_words <- lengths(strsplit(desc_field, split = "[[:space:]]+"))
        desc_sentences <- length(
            strsplit(desc_field, split = "[.!?][[:space:]]+")[[1L]]
        )
        msg <- "The Description field in the DESCRIPTION is made up of less
            than 3 sentences. Provide a more detailed description of the
            package."

        # values chosen sensibly in a data-driven manner
        if (nchar(desc_field) < 50 || desc_words < 20)
            handleWarning(
                "Description field in the DESCRIPTION file is too concise"
            )
        else if (desc_sentences < 3)
            handleNote(paste(strwrap(msg), collapse="\n"))
    }
}

.checkORCID <- function(orcid)
{
    re <- "^[0-9]{4}-[0-9]{4}-[0-9]{4}-[0-9]{3}[0-9X]$"
    grepl(re, orcid)
}


checkBBScompatibility <- function(pkgdir, source_tarball)
{
    lines <- readLines(file.path(pkgdir, "DESCRIPTION"), warn=FALSE)
    desc <- file.path(pkgdir, "DESCRIPTION")
    handleCheck("Checking for blank lines in DESCRIPTION...")
    if (any(nchar(lines)==0))
    {
        handleError("Remove blank lines from DESCRIPTION.")
        return()
    }
    handleCheck("Checking if DESCRIPTION is well formatted...")
    dcf <- tryCatch({
        read.dcf(desc)
    }, error = function(err) {
        handleError("DESCRIPTION is malformed.")
        handleMessage(conditionMessage(err))
        return()
    })

    handleCheck("Checking for proper Description: field...")
    .checkDescription(desc)

    handleCheck("Checking for whitespace in DESCRIPTION field names...")
    if (any(grepl("\\s", colnames(dcf))))
    {
        handleError("Remove whitespace from DESCRIPTION field names.")
        return()
    }
    segs <- strsplit(pkgdir, .Platform$file.sep)[[1]]
    pkgNameFromDir <- segs[length(segs)]
    handleCheck("Checking that Package field matches directory/tarball name...")
    if (dcf[, "Package"] != pkgNameFromDir)
    {
        handleError(
            "Package directory '", pkgNameFromDir, "' must match Package: ",
            "field (got '", dcf[, "Package"], "')."
        )
        return()
    }
    handleCheck("Checking for Version: field...")
    if (!"Version" %in% colnames(dcf))
    {
        handleError("No 'Version:' field in DESCRIPTION.")
        return()
    }
    handleCheck("Checking for valid maintainer...")
    if (!source_tarball){
        if (("Authors@R" %in% colnames(dcf)) & any((c("Author","Maintainer") %in% colnames(dcf)))){
            handleError(
                "Use only the Authors@R field not Author/Maintainer fields."
            )
        } else {
            if (any((c("Author","Maintainer") %in% colnames(dcf))))
                handleError("Do not use Author/Maintainer fields. Use Authors@R.")
        }
    }

    maintainer <- NULL
    if ("Authors@R" %in% colnames(dcf))
    {
        env <- new.env(parent=emptyenv())
        env[["c"]] <- c
        env[["person"]] <- utils::person
        pp <- parse(text=dcf[,"Authors@R"], keep.source=TRUE)
        tryCatch(people <- eval(pp, env),
            error=function(e) {
                handleError("Authors@R field must be valid R code.")
            })
        if (!exists("people")) return()
        if (!"person" %in% class(people))
        {
            handleError("Authors@R must evaluate to 'person' object.")
            return()
        }
        fnd <- vapply(people, FUN.VALUE=logical(1), USE.NAMES=FALSE,
                      FUN=function(person){ "cre" %in% person$role})
        if (length(which(fnd)) > 1L){
            handleError("Designated only one maintainer with Authors@R [cre].")
        }
        for (person in people)
        {
            if ("ORCID" %in% names(person$comment)) {
                orcid <- person$comment[["ORCID"]]
                validID <- .checkORCID(orcid)
                if (!validID)
                    handleNote(
                        "Invalid ORCID iD for ",
                        person$given, " ", person$family
                    )
            } else if ("cre" %in% person$role) {
                handleNote(
                    "Consider adding the maintainer's ORCID iD in 'Authors@R'",
                    " with 'comment=c(ORCID=\"...\")'"
                )
            }
            if ("cre" %in% person$role)
            {
                email <- person$email
                if (is.null(email))
                    return(NULL)
                given <- paste(person$given, collapse=" ")
                if (is.null(given))
                    given <- ""
                family <- paste(person$family, collapse=" ")
                if (is.null(family))
                    family <- ""
                if (given == "" && family == "")
                    return(NULL)
                res <- sprintf("%s %s <%s>", given, family, email)
                res <- sub("^ +", "", res)
                maintainer <- res
                break
            }
        }
        if (is.null(maintainer))
        {
            handleError("Authors@R field in DESCRIPTION file is malformed.")
            return()
        }
    } else if ("Maintainer" %in% colnames(dcf)) {
        handleError("Remove Maintainer field. Use Authors@R [cre] designation.")
        return()
    } else {
        handleError("No Authors@R [cre] field in DESCRIPTION file.")
        return()
    }
    # now need to make sure that regexes work, a la python/BBS
    # I think R CMD check now does this already but can't hurt to keep
    regex <- '(.*\\S)\\s*<(.*)>\\s*'
    match <- regexec(regex, maintainer)[[1]]
    match.length <- attr(match, "match.length")
    if (all(match == -1) && all(match.length == -1))
    {
        handleError("Maintainer field in DESCRIPTION file is malformed.")
        return()
    }
}

.parsePackageImportsFromNamespace <- function(pkg, libloc) {
    importFields <- c("imports", "importClasses", "importMethods")
    imps <- parseNamespaceFile(pkg, libloc)[importFields]
    for (i in names(imps))
        imps[[i]] <- vapply(imps[[i]], `[[`, character(1L), 1L)
    unique(unlist(imps))
}

checkVignetteDir <- function(pkgdir, checkingDir)
{
    vigdir <- file.path(pkgdir, "vignettes")

    vde <- checkVigDirExists(vigdir)
    if (!vde) {
        handleError("No 'vignettes' directory.")
        return()
    }

    vigdircontents <- getVigSources(vigdir)
    if (!length(vigdircontents)) {
        handleError("No vignette sources in vignettes/ directory.")
        return()
    }

    checkInstContents(pkgdir, checkingDir)

    checkVigFiles(vigdir, vigdircontents)

    desc <- file.path(pkgdir, "DESCRIPTION")
    builder <- getVigBuilder(desc)

    if (!is.null(builder)){
        checkVigBuilder(builder, vigdircontents)
    }

    checkVigMetadata(vigdircontents)

    checkVigTypeRNW(vigdircontents)

    checkVigEngine(builder, vigdircontents)

    checkVigSuggests(builder, vigdircontents, pkgdir)

    checkVigTemplate(vigdircontents)

    checkVigChunkEval(vigdircontents)

    checkDupChunkLabels(vigdircontents)

    checkVigBiocInst(pkgdir)

    checkVigInstalls(pkgdir)

    checkVigClassUsage(pkgdir)

    checkTFSymbolUsage(pkgdir)

    checkVigSessionInfo(pkgdir)

    checkVigEvalAllFalse(pkgdir)
}

checkVigDirExists <- function(vigdir) { dir.exists(vigdir) }

checkInstContents <- function(pkgdir, checkingDir)
{
    instdocdir <- file.path(pkgdir, "inst", "doc")
    instdocdircontents <- getVigSources(instdocdir)
    if (length(instdocdircontents) && checkingDir)
        handleWarning(
            "Remove vignette sources from inst/doc; ",
            "they belong in vignettes/."
        )
}

checkVigFiles <- function(vigdir, vigdircontents){
    vigs <- tolower(basename(vigdircontents))
    allvigfiles <- setdiff(tolower(dir(vigdir, all.files=TRUE, ignore.case=TRUE,
                               recursive=TRUE)), vigs)

    if (length(allvigfiles) != 0){
        badFiles <- unlist(lapply(vigs,
                           FUN = function(x, allvigfiles){
                               vl <- tools::file_path_sans_ext(x)
                               badext <- c(".tex", ".html", ".pdf",
                                           ".aux", ".log")
                               ext <- paste0(vl, badext)
                               fnd <- intersect(allvigfiles, ext)
                               fnd
                           },
                           allvigfiles = allvigfiles))
        if (length(badFiles) != 0){
            handleNote(
                "Potential intermediate files found:",
                messages = paste0("vignettes/", badFiles)
            )
        }
    }
}

checkVigBuilder <- function(builder, vigdircontents)
{
# check DESCRIPTION is in at least one vignette
    vigExt <- tolower(tools::file_ext(vigdircontents))
    badBuilder <- character(0)
    for (desc in builder){
        res <- vapply(vigdircontents, vigHelper, logical(1), builder = desc)
        if(!any(res, na.rm=TRUE)){
            if (!(desc == "Sweave" && any(vigExt == "rnw"))){
                badBuilder <- c(badBuilder, desc)
            }
        }
    }
    if (length(badBuilder) != 0L){
        handleError(
            "'VignetteBuilder' listed in DESCRIPTION but not ",
            "found as 'VignetteEngine' in any vignettes:",
            messages = badBuilder
        )
    }
}

checkVigTypeRNW <- function(vigdircontents) {
    vigExt <- tolower(tools::file_ext(vigdircontents))
    isRNW <- vigExt == "rnw"
    vigNames <- basename(vigdircontents[isRNW])
    if (length(vigNames))
        handleWarning(
            "Use RMarkdown instead of Sweave 'Rnw' vignettes.",
            help_text = "Rnw vignette(s) found:",
            messages = vigNames
        )
}

checkVigMetadata <- function(vigdircontents)
{
    badVig <- character(0)
    vigExt <- tolower(tools::file_ext(vigdircontents))
    dx <- which(vigExt != "rnw")
    vigdircontents <- vigdircontents[dx]
    for (file in vigdircontents) {
        lines <- readLines(file, n=100L, warn=FALSE)
        idx <- grep(lines, pattern="vignette:")
        if (length(idx) == 0L)
            badVig <- c(badVig, basename(file))
    }
     if (length(badVig) != 0L){
        handleWarning(
            "Vignette(s) missing Vignette metadata. See ",
            "http://r-pkgs.had.co.nz/vignettes.html",
            help_text = "Update the following files:",
            messages = badVig
        )
    }
}

checkVigEngine <- function(builder, vigdircontents)
{
# check Engines are in DESCRIPTION
    vigExt <- tolower(tools::file_ext(vigdircontents))
    dx <- which(vigExt != "rnw")

    # check for very rare case that mulitple build
    # engines specified in vignette
    builderRes <- grepPkgDir(file.path(dirname(vigdircontents[1]),
                                       .Platform$file.sep),
                             "-rHn 'VignetteEngine{'")
    filenames <- vapply(builderRes,
                        FUN=function(x){strsplit(x,
                            split=" ")[[1]][1]},
                        character(1))
    inval <- names(which(table(filenames) > 1))
    if (length(inval)){
        handleErrorFiles(
            "More than one VignetteEngine specified.",
            help_text = "Found in vignette/files:",
            messages = inval
        )
        dx <- dx[!(basename(vigdircontents[dx]) %in% inval)]
    }
    if (length(dx) != 0) {
        res <-
            vapply(vigdircontents[dx], vigHelper, logical(1), builder=builder)
        if (length(which(!res)) != 0L){
            handleError(
                "'VignetteEngine' specified but not in the DESCRIPTION.",
                help_text =
                    "Add 'VignetteEngine' to DESCRIPTION from the following:",
                messages = basename(names(which(!res)))
            )
        }
        nadx <- which(is.na(res))
        if (length(nadx) != 0L || is.null(builder)){
            files <- res[nadx]
            if (is.null(builder))
                files <- c(files, "DESCRIPTION")
            handleError(
                "No 'VignetteEngine' specified in vignette or DESCRIPTION. ",
                help_text = paste(
                    "Add a 'VignetteEngine' to the following files or",
                    "a default 'VignetteBuilder' in DESCRIPTION: "
                ),
                messages = basename(names(files))
            )
        }
    }
}

checkVigSuggests <- function(builder, vigdircontents, pkgdir)
{
    vigExt <- tolower(tools::file_ext(vigdircontents))
    res <- lapply(vigdircontents, getVigEngine)
    lst <- unique(c(unlist(unname(res)), builder))
    if (any(is.na(lst)))
        lst <- lst[!is.na(lst)]
    dep <- getAllDependencies(pkgdir)
    if (!all(lst %in% dep)){
        handleWarning(
            "Package listed as VignetteEngine or VignetteBuilder ",
            "but not currently Suggested. ",
            help_text = "Add the following to Suggests in DESCRIPTION:",
            messages = lst[!(lst %in% dep)]
        )
    }
}

checkVigTemplate <- function(vigdircontents)
{
    badVig <- character(0)
    badVig2 <- character(0)
    for (file in vigdircontents) {
        lines <- readLines(file, warn=FALSE)
        if (identical(tolower(tools::file_ext(file)), "rmd"))
            lines <- .getYAMLfront(lines)
        idx <- grep(lines, pattern="VignetteIndexEntry")
        if (length(idx)) {
            title <- tolower(gsub(".*\\{|\\}.*", "", lines[idx]))
            if (identical(title, "vignette title"))
                badVig <- c(badVig, basename(file))
        }
        if (!length(idx))
            badVig2 <- c(badVig2, basename(file))
    }
    if (length(badVig))
        handleWarning(
            "Vignette(s) still using 'VignetteIndexEntry{Vignette Title}' ",
            help_text = "The following files use template defaults:",
            messages = badVig
        )
    if (length(badVig2))
        handleWarning(
            "Vignette(s) missing '\\%VignetteIndexEntry{Vignette Title}'. ",
            help_text = "Update the following files:",
            messages = badVig2
        )
}

checkVigChunkEval <- function(vigdircontents)
{
    chunks <- 0
    efs <- 0
    noneval <- 0
    for (file in vigdircontents)
    {
        lines <- readLines(file, warn=FALSE)
        vignetteType <- knitr:::detect_pattern(lines, tools::file_ext(file))
        if (is.null(vignetteType)) {
            chunklines <- character(0)
            nonEvalChunk <- character(0)
        } else {
            chunkPattern <- knitr::all_patterns[[vignetteType]]$chunk.begin
            chunklines <- lines[grep(chunkPattern, lines)]

            # find non evaluated code chunks (```, ```r, ```R, etc.)
            # assumes every other one for start and stop of code chunk
            nonEvalChunk <- lines[grep("^[\t >]*```+\\s*", lines)]
            if (length(nonEvalChunk)) {
                nonEvalChunk <- nonEvalChunk[c(TRUE,FALSE)]
                indx <- grep("^[\t >]*```+\\s*\\{([a-zA-Z0-9_]+.*)\\}\\s*$",
                             nonEvalChunk)
                if (length(indx))
                    nonEvalChunk <- nonEvalChunk[-indx]
            }
        }
        chunks <- chunks + length(chunklines) + length(nonEvalChunk)

        efs <- efs +
            length(grep("eval\\s*=\\s*F(ALSE)?", chunklines))

        noneval <- noneval + length(nonEvalChunk)
    }

    totnon <- efs + noneval
    percent <- ifelse(
        chunks == 0 && totnon == 0,
        0L,
        as.integer((totnon * 100 / chunks))
    )

    if (percent >= 50){
        handleWarning("Evaluate more vignette chunks.")
        msg <- sprintf(
            "%s out of %s code chunks = %i%% unevaluated",
            totnon,
            chunks,
            percent
        )
        handleMessage(msg, indent = 8)
        handleMessage(
            sprintf("%s non-exec code chunk(s) (e.g., '```r')", noneval),
            indent = 8
        )
    }
}

checkVigEvalAllFalse <- function(pkgdir){
    pkgdir <- file.path(pkgdir, "vignettes")
    Vigdir <- sprintf("%s%s", pkgdir, .Platform$file.sep)
    msg_eval <- grepPkgDir(Vigdir,
                           "-rHn 'knitr::opts_chunk\\$set(.*eval\\s*=\\s*F'")
    if (length(msg_eval)) {
        handleWarningFiles(
            " Vignette set global option 'eval=FALSE'",
            messages = msg_eval
        )
    }
}

checkDupChunkLabels <- function(vigfiles) {
    viglist <- structure(
        vector("logical", length(vigfiles)),
        .Names = vigfiles
    )
    for (vfile in vigfiles) {
        tempR <- tempfile(fileext=".R")
        tryCatch({
            quiet_knitr_purl(input = vfile, output = tempR, quiet = TRUE)
        }, error = function(e) {
            viglist[[vfile]] <<- grepl(
                "Duplicate chunk label", conditionMessage(e), fixed = TRUE
            )
            if (viglist[[vfile]])
                invisible(NULL)
            else
                stop(e)
        })
    }
    if (any(viglist))
        handleErrorFiles(
            " Vignette(s) found with duplicate chunk labels",
            messages = basename(vigfiles[viglist])
        )
}

.OLD_INSTALL_CALLS <-
    c("BiocInstaller", "biocLite", "useDevel", "biocinstallRepos")

checkVigBiocInst <- function(pkgdir) {
    msg_return <- findSymbolsInVignettes(
        pkgdir,
        Symbols = .OLD_INSTALL_CALLS,
        tokenTypes = c("COMMENT", "SYMBOL_FUNCTION_CALL")
    )
    if (length(msg_return)) {
        handleWarningFiles(
            " BiocInstaller code found in vignette(s)",
            messages = msg_return
        )
    }
}

.BAD_INSTALL_CALLS <- c("biocLite", "install.packages", "install_packages",
    "update.packages", "install")

checkVigInstalls <- function(pkgdir) {
    match_return <- findSymbolsInVignettes(
        pkgdir,
        Symbols = .BAD_INSTALL_CALLS,
        tokenTypes = "SYMBOL_FUNCTION_CALL"
    )
    grep_return <- findSymbolsInVignettes(
        pkgdir,
        Symbols = ".*install[^ed].*",
        tokenTypes = "SYMBOL_FUNCTION_CALL",
        FUN = .grepTokenTextCode
    )
    msg_return <- c(match_return, grep_return)
    if (length(msg_return)) {
        handleErrorFiles(
            "Installation calls found in vignette(s)",
            messages = msg_return
        )
    }
}

## Completely suppress spurious knitr:::remind_sweave() warning that shows up
## in TclTk popup window in addition to the usual text-only warning.
quiet_knitr_purl <- function(...)
{
    args <- list(...)
    callr::r(
        function(...) suppressWarnings(knitr::purl(...)),
        args = args,
        env = c(CI = "true")
    )
}

purl_or_tangle <- function(input, output, quiet, ...) {
    vigEng <- getVigEngine(input)
    vigExt <- tolower(tools::file_ext(input))
    if (!identical(vigExt, "rnw") || identical(vigEng, "knitr"))
        quiet_knitr_purl(input = input, output = output, quiet = quiet, ...)
    else
        utils::Stangle(file = input, output = output, quiet = quiet)
}

try_purl_or_tangle <- function(input, output, quiet, ...) {
    tryCatch({
        purl_or_tangle(input = input, output = output, quiet = quiet, ...)
    }, error = function(e) {
        hasDups <- grepl(
            "Duplicate chunk label", conditionMessage(e), fixed = TRUE
        )
        if (hasDups) {
            file.create(output)
            invisible(NULL)
        } else {
            stop(e)
        }
    })
}

checkVigClassUsage <- function(pkgdir) {
    vigdir <- file.path(pkgdir, "vignettes", "")
    vigfiles <- getVigSources(vigdir)
    viglist <- structure(
        vector("list", length(vigfiles)), .Names = basename(vigfiles)
    )
    for (vfile in vigfiles) {
        tempR <- tempfile(fileext=".R")
        try_purl_or_tangle(input = vfile, output = tempR, quiet = TRUE)
        tokens <- getClassNEEQLookup(tempR)
        viglist[[basename(vfile)]] <- sprintf(
            "%s (code line %d, column %d)",
            basename(vfile), tokens[,"line1"], tokens[,"col1"]
        )
    }
    viglist <- Filter(length, viglist)
    if (length(viglist)) {
        handleWarningFiles(
            " Avoid class membership checks with class() / is() and == / !=",
            "; Use is(x, 'class') for S4 classes",
            messages = unlist(viglist, use.names = FALSE)
        )
    }
}

checkTFSymbolUsage <- function(pkgdir) {
    viglist <- findSymbolsInVignettes(pkgdir, c("T", "F"), "SYMBOL")
    if (length(viglist)) {
        handleWarningFiles(
            " Avoid T/F variables; If logical, use TRUE/FALSE",
            messages = unlist(viglist, use.names = FALSE)
        )
    }
}

checkVigSessionInfo <- function(pkgdir) {
    vigdir <- file.path(pkgdir, "vignettes", "")
    vigfiles <- getVigSources(vigdir)
    notFoundVig <- structure(
        vector("logical", length(vigfiles)), .Names = vigfiles
    )
    for (vfile in vigfiles) {
        pc <- structure(
            list(parseFile(vfile, pkgdir)), .Names = vfile
        )
        res <- findSymbolsInParsedCode(
            parsedCodeList = pc,
            symbolNames = c("sessionInfo", "session_info"),
            tokenTypes = "SYMBOL_FUNCTION_CALL"
        )
        if (!length(res)) {
            notFoundVig[[vfile]] <- TRUE
        }
    }
    if (any(notFoundVig)) {
        handleNote(
            " 'sessionInfo' not found in vignette(s)",
            help_text = "Missing from file(s):",
            messages = .getDirFiles(vigfiles[notFoundVig])
        )
    }
}

checkIsVignetteBuilt <- function(package_dir, build_output_file)
{
    if (!file.exists(build_output_file))
    {
        stop("build output file '", build_output_file, "' does not exist.")
    }
    lines <- readLines(build_output_file)
    if (!any(grepl("^\\* creating vignettes \\.\\.\\.", lines)))
    {
        msg <- "Vignette must be built by
        'R CMD build'. Please see the `Vignette Checks` section of
        the BiocCheck vignette."
        handleError(msg)
    }
}

findSymbolsInRFiles <-
    function(pkgdir, Symbols, tokenType, fun = TRUE, ...)
{
    rfiles <- getRSources(pkgdir)
    parsedCodes <- lapply(
        structure(rfiles, .Names = rfiles), parseFile, pkgdir = pkgdir
    )
    msg_res <- findSymbolsInParsedCode(
        parsedCodeList = parsedCodes,
        symbolNames = Symbols,
        tokenTypes = tokenType,
        fun = fun, ...
    )
    unlist(msg_res)
}

findSymbolsInVignettes <-
    function(pkgdir, Symbols, tokenTypes, FUN = .getTokenTextCode)
{
    vigdir <- file.path(pkgdir, "vignettes", "")
    vigfiles <- getVigSources(vigdir)
    viglist <- list()
    for (vfile in vigfiles) {
        tempR <- tempfile(fileext=".R")
        try_purl_or_tangle(input = vfile, output = tempR, quiet = TRUE)
        tokens <- FUN(parseFile(tempR, pkgdir), tokenTypes, Symbols)
        viglist[[.getDirFiles(vfile)]] <- sprintf(
            "%s (code line %d, column %d)",
           .getDirFiles(vfile), tokens[,"line1"], tokens[,"col1"]
        )
    }
    Filter(length, viglist)
}

checkPkgInstallCalls <- function(package_dir, badCalls = .BAD_INSTALL_CALLS) {
    msg_installs <- findSymbolsInRFiles(
        package_dir, badCalls, "SYMBOL_FUNCTION_CALL"
    )
    if (length(msg_installs)) {
        handleNote(
            "Avoid using install, biocLite, install.packages,",
            " or update.packages",
            help_text = "Functions in files:",
            messages = msg_installs
        )
    }
    # for unit testing
    invisible(msg_installs)
}

checkForLibraryRequire <- function(pkgdir) {
    msg_lib <- findSymbolsInRFiles(
        pkgdir,
        Symbols = c("library", "require"),
        tokenType = "SYMBOL_FUNCTION_CALL"
    )
    if (length(msg_lib)) {
        handleWarningFiles(
            " Avoid the use of 'library' or 'require' in R code",
            messages = msg_lib
        )
    }
    # for unit testing
    invisible(msg_lib)
}

checkCodingPractice <- function(pkgdir, parsedCode, package_name)
{
    Rdir <- file.path(pkgdir, "R")

    # sapply
    msg_sapply <- checkSapply(Rdir)
    if (length(msg_sapply)) {
        handleNoteFiles(
            " Avoid sapply(); use vapply()",
            messages = msg_sapply
        )
    }

    # 1:...
    msg_seq <- check1toN(Rdir)
    if (length(msg_seq)) {
        handleNoteFiles(
            " Avoid 1:...; use seq_len() or seq_along()",
            messages = msg_seq
        )
    }

    # pkg:fun...
    msg_sc <- checkSingleColon(Rdir)
    if (length(msg_sc)) {
        handleErrorFiles(
            " Use double colon for qualified imports: 'pkg::foo()'",
            messages = msg_sc
        )
    }

    # cat() and print()
    msg_cat <- checkCatInRCode(Rdir)
    if (length(msg_cat)) {
        handleNoteFiles(
            " Avoid 'cat' and 'print' outside of 'show' methods",
            messages = msg_cat
        )
    }

    # assignment with =
    msg_eq <- checkEqInAssignment(Rdir)
    if (length(msg_eq)) {
        handleNoteFiles(
            " Avoid using '=' for assignment and use '<-' instead",
            messages = msg_eq
        )
    }

    # message(paste(...))
    msg_mp <- checkPasteInSignaler(Rdir)
    if (length(msg_mp)) {
        handleNoteFiles(
            " Avoid the use of 'paste' in condition signals",
            messages = msg_mp
        )
    }

    # stop("Error: ")
    msg_ss <- checkSignalerInSignaler(Rdir)
    if (length(msg_ss)) {
        handleNoteFiles(
            " Avoid redundant 'stop' and 'warn*' in signal conditions",
            messages = msg_ss
        )
    }

    # T/F
    msg_tf <- findSymbolsInRFiles(
        pkgdir, c("T", "F"), "SYMBOL", lookback = "'$'"
    )
    if (length(msg_tf)) {
        handleWarning(
            " Avoid T/F variables; If logical, use TRUE/FALSE ",
            help_text = paste("Found", length(msg_tf), "times:"),
            messages = msg_tf
        )
    }

    # class() ==
    msg_class <- checkClassNEEQLookup(pkgdir)
    if (length(msg_class)) {
        handleWarningFiles(
            " Avoid class membership checks with class() / is() and == / !=",
            "; Use is(x, 'class') for S4 classes",
            messages = msg_class
        )
    }

    # system() vs system2()
    msg_sys <- checkSystemCall(pkgdir)
    if(length(msg_sys)) {
        handleNoteFiles(
            " Avoid system() ; use system2()",
            messages = msg_sys
        )
    }

    # external data
    msg_eda <- checkExternalData(Rdir)
    if (length(msg_eda)) {
        handleErrorFiles(
            " Avoid references to external hosting platforms",
            messages = msg_eda
        )
    }

    # download / download.file in .onAttach / .onLoad
    msg_dl <- checkOnAttachLoadCalls(Rdir)
    if (length(msg_dl)) {
        handleErrorFiles(
            " Avoid downloads in '.onAttach' or '.onLoad' functions",
            messages = msg_dl
        )
    }

    # set.seed
    msg_seed <- findSymbolsInRFiles(pkgdir, "set.seed", "SYMBOL_FUNCTION_CALL")
    if (length(msg_seed)){
        handleWarning(
            " Remove set.seed usage (found ", length(msg_seed), " times)",
            messages = msg_seed
        )
    }

    # .Deprecated / .Defunct usage should be updated after every release
    msg_depr <- findSymbolsInRFiles(
        pkgdir, c(".Deprecated", ".Defunct"), "SYMBOL_FUNCTION_CALL"
    )
    if (length(msg_depr)) {
        handleWarning(
            ".Deprecated / .Defunct usage (found ",
            length(msg_depr), " times)",
            messages = msg_depr
        )
    }

    handleCheck("Checking parsed R code in R directory, examples, vignettes...")

    # direct slot access
    checkForDirectSlotAccess(parsedCode, package_name)

    # browser() calls
    msg_b <- findSymbolsInRFiles(pkgdir, "browser", "SYMBOL_FUNCTION_CALL")
    if (length(msg_b)) {
        handleWarning(
            "Remove browser() statements (found ", length(msg_b), " times)",
            messages = msg_b
        )
    }

    # install() / install.packages() calls
    msg_inst <- findSymbolsInRFiles(
        pkgdir, .BAD_INSTALL_CALLS, "SYMBOL_FUNCTION_CALL"
    )
    if (length(msg_inst)) {
        handleError(
            "Remove install() calls (found ", length(msg_inst), " times)",
            messages = msg_inst
        )
    }

    # <<-
    msg_da <- findSymbolsInRFiles(pkgdir, "<<-", "LEFT_ASSIGN")
    if (length(msg_da)) {
        handleNote(
            "Avoid '<<-' if possible (found ", length(msg_da), " times)",
            messages = msg_da
        )
    }

    # Sys.setenv calls
    msg_env <- findSymbolsInRFiles(pkgdir, "Sys.setenv", "SYMBOL_FUNCTION_CALL")
    if (length(msg_env)) {
        handleError(
            "Avoid 'Sys.setenv' (found ", length(msg_env), " times)",
            messages = msg_env
        )
    }

    # suppressWarnings/Messages calls
    msg_supp <- findSymbolsInRFiles(
        pkgdir,
        c("suppressWarnings", "suppressMessages"),
        "SYMBOL_FUNCTION_CALL"
    )
    if (length(msg_supp)) {
        handleNote(
            "Avoid 'suppressWarnings'/'*Messages' if possible (found ",
            length(msg_supp), " times)",
            messages = msg_supp
        )
    }
}

checkSapply <- function(Rdir) {
    msg_sapply <- findSymbolsInRFiles(
        dirname(Rdir), "sapply", "SYMBOL_FUNCTION_CALL", FALSE
    )
}

check1toN <- function(Rdir){

    rfiles <- getRSources(Rdir)
    msg_seq <- lapply(rfiles, function(rfile) {
        tokens <- getParseData(parse(rfile, keep.source=TRUE))
        tokens <- tokens[tokens[,"token"] != "expr", ,drop=FALSE]
        colons <- which(tokens[,"text"] == ":") - 1
        colons <- colons[tokens[colons, "text"] == "1"]
        tokens <- tokens[colons, , drop=FALSE]
        tokens <- tokens[ tokens[,"text"] == "1", , drop=FALSE]
        sprintf(
            "%s (line %d, column %d)",
            basename(rfile), tokens[,"line1"], tokens[,"col1"]
        )
    })
    msg_seq <- unlist(msg_seq)
}

checkSingleColon <- function(Rdir, avail_pkgs = character(0L)) {

    rfiles <- getRSources(Rdir)
    names(rfiles) <- basename(rfiles)
    colon_pres <- lapply(rfiles, function(rfile) {
        tokens <- getParseData(parse(rfile, keep.source = TRUE))
        tokens <- tokens[tokens[,"token"] != "expr", ,drop=FALSE]
        colons <- which(tokens[,"text"] == ":") - 1
        colons <- colons[grepl("[[:alpha:]]", tokens[colons, "text"])]
        tokens[colons, , drop = FALSE]
    })
    colon_pres <- Filter(nrow, colon_pres)
    if (length(colon_pres))
        avail_pkgs <- BiocManager::available()
    msg_sc <- lapply(names(colon_pres), function(rfile, framelist) {
        tokens <- framelist[[rfile]]
        tokens <- tokens[tokens[, "text"] %in% avail_pkgs, , drop = FALSE]
        sprintf(
            "%s (line %d, column %d)",
            rfile, tokens[, "line1"], tokens[, "col1"]
        )
    }, framelist = colon_pres)
    msg_sc <- unlist(msg_sc)
}

.filtTokens <-
    function(ind, tokens, keywords = c("paste0", "paste"))
{
    txt <- tokens[ind, "text"]
    filt <- tolower(txt) %in% keywords
    #filt <- grepl(txt, keywords, ignore.case = ignore.case)
    if (any(filt) && "collapse" %in% txt)
        filt <- FALSE
    ind[filt]
}

.getTokens <- function(rfile) {
    tokens <- getParseData(parse(rfile, keep.source = TRUE))
    tokens[tokens[,"token"] != "expr", ,drop=FALSE]
}

.grepSymbolRanges <-
    function(tokens, patterns, tokenType = "SYMBOL_FUNCTION_CALL", isExp = FALSE)
{
    txt <- tokens[, "text"]
    found <- lapply(patterns, function(pattern) grepl(pattern, txt))
    found <- Reduce(`|`, found)
    hits <- which(found & tokens[, "token"] == tokenType)
    openBracket <- if (isExp) "{" else "("
    opar <- which(txt == openBracket)
    startHit <- vapply(hits, function(x) min(opar[opar > x]), numeric(1L))
    parnum <- tokens[startHit, "parent"]
    endHit <- nrow(tokens) - match(parnum, rev(tokens[, "parent"]))
    Map(seq, startHit, endHit)
}

.findSymbolRanges <-
    function(tokens, symbols, tokenType = "SYMBOL_FUNCTION_CALL", isExp = FALSE)
{
    txt <- tokens[, "text"]
    signalers <- which(
        txt %in% symbols & tokens[, "token"] == tokenType
    )
    openBracket <- if (isExp) "{" else "("
    opar <- which(txt == openBracket)
    startSig <- vapply(signalers, function(x) min(opar[opar > x]), numeric(1L))
    parnum <- tokens[startSig, "parent"]
    endSig <- nrow(tokens) - match(parnum, rev(tokens[, "parent"]))
    Map(seq, startSig, endSig)
}

.findInSignaler <- function(rfile, symbols, FUN, ...) {
    tokens <- .getTokens(rfile)
    sigRanges <- .findSymbolRanges(tokens, symbols)
    pasteInd <- lapply(sigRanges, FUN, tokens = tokens, ...)
    tokens <- tokens[unlist(pasteInd), , drop = FALSE]
    rfile <- paste0("R/", basename(rfile))
    sprintf(
        "%s (line %d, column %d)",
        rfile, tokens[, "line1"], tokens[, "col1"]
    )
}

.SIGNALERS_TXT <- c("message", "warning", "stop")

.findPasteInSignaler <- function(rfile, symbols = .SIGNALERS_TXT) {
    .findInSignaler(rfile, symbols, .filtTokens)
}

.filtersetMethodRanges <- function(tokens) {
    excl <- .findSymbolRanges(tokens, "setMethod")
    if (length(excl)) {
        showHits <- vapply(excl,
            function(x) '"show"' %in% tokens[x, "text"], logical(1))
        negind <- unlist(lapply(excl[showHits], `-`))
        tokens <- tokens[negind, ]
    }
    tokens
}

.filterS3printRanges <- function(tokens) {
    excl <- .grepSymbolRanges(tokens, "^print\\..*", tokenType = "SYMBOL")
    if (length(excl)) {
        showHits <- vapply(excl,
            function(x) "cat" %in% tokens[x, "text"], logical(1)
        )
        negind <- unlist(lapply(excl[showHits], `-`))
        tokens <- tokens[negind, ]
    }
    tokens
}

checkCatInRCode <-
    function(Rdir, symbols = c("cat", "print"))
{
    pkgdir <- dirname(Rdir)
    rfiles <- getRSources(pkgdir)
    parsedCodes <- lapply(
        structure(rfiles, .Names = rfiles), parseFile, pkgdir = pkgdir
    )
    parsedCodes <- lapply(parsedCodes, .filtersetMethodRanges)
    parsedCodes <- lapply(parsedCodes, .filterS3printRanges)
    msg_res <- findSymbolsInParsedCode(
        parsedCodeList = parsedCodes,
        symbolNames = symbols,
        tokenTypes = c("SYMBOL_FUNCTION_CALL", "SYMBOL")
    )
    unlist(msg_res)
}

checkEqInAssignment <- function(Rdir, symbol = "=", tokenType = "EQ_ASSIGN") {
    pkgdir <- dirname(Rdir)
    rfiles <- getRSources(pkgdir)
    parsedCodes <- lapply(
        structure(rfiles, .Names = rfiles), parseFile, pkgdir = pkgdir
    )
    msg_res <- findSymbolsInParsedCode(
        parsedCodeList = parsedCodes,
        symbolNames = symbol,
        tokenTypes = tokenType,
        fun = FALSE
    )
    unlist(msg_res)
}

.grepTokens <-
    function(ind, tokens, keywords)
{
    txt <- tokens[ind, , drop = FALSE]
    strs <- txt$token == "STR_CONST"
    ind <- ind[strs]
    txt <- txt[strs, "text"]
    filt <- grepl(paste0(keywords, collapse = "|"), txt, ignore.case = TRUE)
    ind[filt]
}

.findSignalerInSignaler <- function(rfile, symbols) {
    .findInSignaler(rfile, symbols, .grepTokens,
        keywords = c("message", "warn", "error"))
}

checkPasteInSignaler <- function(Rdir) {
    rfiles <- getRSources(Rdir)
    pasteSig <- lapply(rfiles, .findPasteInSignaler)
    pasteSig <- unlist(pasteSig)
}

checkSignalerInSignaler <- function(Rdir) {
    rfiles <- getRSources(Rdir)
    sisig <- lapply(rfiles, .findSignalerInSignaler, symbols = .SIGNALERS_TXT)
    sisig <- unlist(sisig)
}

.checkValidNEEQPattern <- function(tokens, eqnums) {
    tokens[["rowID"]] <- seq_len(nrow(tokens))
    unlist(lapply(eqnums, function(eq) {
        parnum <- tokens[eq, "parent"]
        hits <- which(tokens[, "parent"] %in% parnum)
        if (!length(hits)) { return(NULL) }
        startEQ <- min(hits)
        endEQ <- max(hits)
        EQblock <- tokens[startEQ:endEQ, ]
        hasIS <- EQblock[, "token"] == "SYMBOL_FUNCTION_CALL" &
            EQblock[, "text"] %in% c("is", "class")
        if (any(hasIS) && "STR_CONST" %in% EQblock[EQblock[["rowID"]] > eq, "token"])
            eq
        else
            NULL
    }))
}

getClassNEEQLookup <- function(rfile) {
    tokens <- getParseData(parse(rfile, keep.source = TRUE))
    eqtoks <- which(tokens[, "token"] %in% c("NE", "EQ"))
    eqtoks <- .checkValidNEEQPattern(tokens, eqtoks)
    tokens[eqtoks, , drop = FALSE]
}

checkClassNEEQLookup <- function(pkgdir) {
    rfiles <- getRSources(pkgdir)
    names(rfiles) <- basename(rfiles)
    NEEQ_pres <- lapply(rfiles, getClassNEEQLookup)
    NEEQ_pres <- Filter(nrow, NEEQ_pres)
    msg_neeq <- lapply(names(NEEQ_pres), function(rfile, framelist) {
        tokens <- framelist[[rfile]]
        sprintf(
            "%s (line %d, column %d)",
            rfile, tokens[, "line1"], tokens[, "col1"]
        )
    }, framelist = NEEQ_pres)
    unlist(msg_neeq)
}

checkSystemCall <- function(pkgdir){

    pkgdir <- sprintf("%s%s", pkgdir, .Platform$file.sep)
    msg_sys <- grepPkgDir(pkgdir, "-rHn '^system(.*'")
}

checkExternalData <- function(Rdir) {

    rfiles <- getRSources(Rdir)
    msg_eda <- lapply(rfiles, function(rfile) {
        tokens <- getParseData(parse(rfile, keep.source=TRUE))
        tokens <- tokens[tokens[,"token"] == "STR_CONST", ,drop=FALSE]

        platforms <- "githubusercontent|github.*[^html\"]$|gitlab|bitbucket|[^\\.]dropbox"
        txtkns <- tokens[, "text"]
        hits <- grepl(platforms, txtkns, ignore.case = TRUE) &
            grepl("dl|\\.\\w+\"$", txtkns)
        tokens <- tokens[hits, , drop = FALSE]

        sprintf(
            "%s (line %d, column %d)",
            basename(rfile), tokens[,"line1"], tokens[,"col1"]
        )
    })
    unlist(msg_eda)
}

checkOnAttachLoadCalls <- function(Rdir) {

    rfiles <- getRSources(Rdir)
    pkgdir <- dirname(Rdir)
    parsedCodes <- lapply(
        structure(rfiles, .Names = rfiles), parseFile, pkgdir = pkgdir
    )
    parsedCodes <- lapply(parsedCodes, function(tokens) {
        tokens <- tokens[!tokens[, "token"] %in% c("expr", "COMMENT"), ]
        incl <- .findSymbolRanges(
            tokens, c(".onLoad", ".onAttach"), "SYMBOL", TRUE
        )
        tokens[unlist(incl), ]
    })
    parsedCodes <- Filter(nrow, parsedCodes)
    msg_dl <- findSymbolsInParsedCode(
        parsedCodeList = parsedCodes,
        symbolNames = "download.*",
        tokenTypes = "SYMBOL_FUNCTION_CALL",
        FUN = .grepTokenTextCode
    )
    unlist(msg_dl)
}

checkForDirectSlotAccess <- function(parsedCode, package_name)
{
    idx <- grepl("\\.R$", names(parsedCode), ignore.case=TRUE)
    parsedCode <- parsedCode[!idx]
    res <- findSymbolInParsedCode(parsedCode, package_name, "@", "'@'")
    if (res > 0)
    {
        handleNote(
            "Use accessors; don't access S4 class slots via ",
            "'@' in examples/vignettes.")
    }
}

checkFunctionLengths <- function(parsedCode, pkgname)
{
    parsedCode <- parsedCode[grepl("\\.[Rr]$", names(parsedCode))]
    if (!length(parsedCode))
        return(invisible())
    fileNames <- .getDirFiles(names(parsedCode))
    dflist <- structure(
        vector("list", length(names(parsedCode))),
        .Names = fileNames
    )
    names(parsedCode) <- fileNames
    for (filename in names(parsedCode)) {
        pc <- parsedCode[[filename]]
        res <- getFunctionLengths(pc)
        functionNames <- names(res)
        mt <- do.call(rbind, res)
        fname <- if (is.null(functionNames)) character(0L) else filename
        df <- cbind.data.frame(
            filename = fname, functionName = functionNames, mt,
            row.names = NULL
        )
        dflist[[filename]] <- df
    }
    dflist <- Filter(nrow, dflist)
    df <- do.call(rbind, dflist)
    if (length(df) && nrow(df)) {
        df <- df[order(-df[["length"]]),]
        h <- df[df[["length"]] > 50,]
        if (nrow(h)) {
            fn_msg <- apply(head(h, n=5), 1L, function(row) {
                sprintf(
                    "%s() (%s): %s lines",
                    row['functionName'], row['filename'], row['length']
                )
            })
            handleNote(
                "The recommended function length is 50 lines or less. ",
                .nline_report(h),
                help_text = "The longest 5 functions are:" ,
                messages = fn_msg
            )
        }
    }
}

.nline_report <- function(data) {
    fnoun <- "functions"
    plural <- !identical(nrow(data), 1L)
    mverb <- if (plural) "are" else "is"
    if (!plural)
        fnoun <- substr(fnoun, 1, nchar(fnoun) - 1)
    paste(
        "There", mverb, nrow(data), fnoun, "greater than 50 lines."
    )
}

checkManDocumentation <- function(package_dir, package_name, libloc)
{
    # canned man prompts
    checkForPromptComments(package_dir)

    # non empty value section exists
    checkForValueSection(package_dir)

    # exports are documented and 80% runnable
    checkExportsAreDocumented(package_dir, package_name, lib.loc = libloc)

    # usage of donttest and dontrun
    checkUsageOfDont(package_dir)
}

checkForPromptComments <- function(pkgdir)
{
    manpages <- dir(file.path(pkgdir, "man"),
        pattern="\\.Rd$", ignore.case=TRUE, full.names=TRUE)
    names(manpages) <- basename(manpages)

    bad <- vapply(manpages,
        function(manpage) {
            lines <- readLines(manpage, warn=FALSE)
            any(grepl("^%%\\s+~", lines))
        },
        logical(1L)
    )

    if (any(bad))
        handleNote(
            "Auto-generated '%% ~' comments found in Rd man pages.",
            messages = names(bad)[bad]
        )
}

.tagListExtract <- function(rd, tags, Tag) {
    if (missing(tags))
        tags <- tools:::RdTags(rd)
    if (!Tag %in% tags)
        character(0L)
    else
        unlist(rd[tags == Tag], recursive = FALSE)
}

.tagsExtract <- function(rd, tags, Tag) {
    tagList <- .tagListExtract(rd = rd, tags = tags, Tag = Tag)
    as.character(tagList)
}

.valueInParsedRd <- function(rd, tags) {
    tagList <- .tagListExtract(rd, tags, "\\value")
    values <- Filter(function(x) attr(x, "Rd_tag") != "COMMENT", tagList)
    value <- paste(values, collapse = "")
    nzchar(trimws(value)) && length(values)
}

.usesRdpack <- function(pkgdir) {
    alldeps <- getAllDependencies(pkgdir)
    "Rdpack" %in% alldeps
}

.parse_Rd_pack <- function(manpage, usesRdpack) {
    sysfile_rdpack <- system.file(package = "Rdpack")
    rdpack_avail <- nzchar(sysfile_rdpack)
    if (usesRdpack && rdpack_avail)
        rdmacros <- file.path(
            sysfile_rdpack, "help", "macros", "refmacros.Rd"
        )
    else
        rdmacros <- file.path(R.home("share"), "Rd", "macros", "system.Rd")

    tools::parse_Rd(manpage, macros = rdmacros)
}

.read_all_rds <- function(pkgdir) {
    manpages <- list.files(
        path = file.path(pkgdir, "man"),
        pattern = "\\.[Rr][Dd]$",
        full.names = TRUE
    )
    names(manpages) <- .getDirFiles(manpages)
    usesRdpack <- .usesRdpack(pkgdir)
    lapply(
        manpages,
        function(manpage, usesRdpack) {
            .parse_Rd_pack(manpage, usesRdpack)
        },
        usesRdpack = usesRdpack
    )
}

.formatsInParsedRd <- function(rd, tags) {
    formats <- .tagsExtract(rd, tags, "\\format")
    value <- paste(formats, collapse = "")
    nzchar(trimws(value)) && length(formats)
}

.skipRdCheck <- function(rd, tags) TRUE

.whichRdCheck <- function(docType) {
    switch(
        docType,
        package =,
        class = .skipRdCheck,
        data = .formatsInParsedRd,
        fun =,
        .valueInParsedRd
    )
}

checkForValueSection <- function(pkgdir)
{
    all_rds <- .read_all_rds(pkgdir)
    all_tags <- lapply(all_rds, tools:::RdTags)
    docTypes <- mapply(docType, rd = all_rds, tags = all_tags, SIMPLIFY = FALSE)
    docTypes[!lengths(docTypes)] <- "fun"
    funs <- lapply(docTypes, .whichRdCheck)
    isData <- unlist(docTypes) == "data"
    ok <- mapply(
        function(afun, rds, atags) {
            afun(rds, atags)
        },
        afun = funs, rds = all_rds, atags = all_tags,
        SIMPLIFY = TRUE
    )
    dataOK <- ok[isData]
    elseOK <- ok[!isData]
    if (!all(dataOK)) {
        not_oks <- names(ok[isData][!dataOK])
        handleWarningFiles(
            "Empty or missing \\format sections found in data man page(s).",
            messages = not_oks
        )
    }
    if (!all(elseOK)) {
        not_oks <- names(ok[!isData][!elseOK])
        handleWarningFiles(
            "Empty or missing \\value sections found in man page(s).",
            messages = not_oks
        )
    }
}

# Which pages document things that are exported?
checkExportsAreDocumented <- function(pkgdir, pkgname, lib.loc)
{
    manpages <- dir(file.path(pkgdir, "man"),
        pattern="\\.Rd$", ignore.case=TRUE, full.names=TRUE)
    pkg_ns <- loadNamespace(pkgname, lib.loc = lib.loc)
    exports <- getNamespaceExports(pkg_ns)
    ## attempt to unload package namespace
    try(unloadNamespace(pkg_ns), silent = TRUE)
    badManPages <- character(0)
    exportingPagesCount <- 0L
    noExamplesCount <- 0L
    uses_rd_pack <- .usesRdpack(pkgdir)

    for (manpage in manpages)
    {
        rd <- .parse_Rd_pack(manpage, usesRdpack = uses_rd_pack)
        name <-
            unlist(rd[unlist(lapply(rd, function(x)
                attr(x, "Rd_tag") == "\\name"))][[1]][1])
        aliases <- unlist(lapply(rd[unlist(lapply(rd,
            function(x) attr(x, "Rd_tag") == "\\alias"))], "[[", 1))
        namesAndAliases <- c(name, aliases)
        exportedTopics <- unique(namesAndAliases[namesAndAliases %in% exports])
        if (length(exportedTopics))
        {
            exportingPagesCount <- exportingPagesCount + 1
        }
        if (length(exportedTopics) &&
            !doesManPageHaveRunnableExample(rd))
        {
            noExamplesCount <- noExamplesCount + 1
            badManPages <- append(badManPages, basename(manpage))
        }
    }

    ratio <- (exportingPagesCount - noExamplesCount) / exportingPagesCount

    if (exportingPagesCount > 0 && ratio  < 0.8)
        handleError(
            "At least 80% of man pages documenting exported objects must ",
            "have runnable examples.",
            help_text = "The following pages do not:",
            messages = badManPages
        )
    else if (length(badManPages))
        handleNote(
            "Consider adding runnable examples to man pages that document ",
            "exported objects.",
            messages = badManPages
        )

    badManPages # for testing
}

checkUsageOfDont <- function(pkgdir)
{
    manpages <- dir(file.path(pkgdir, "man"),
        pattern="\\.Rd$", ignore.case=TRUE, full.names=TRUE)

    hasBad <- rep(FALSE, length(manpages))
    hasdontrun <- rep(FALSE, length(manpages))
    uses_rd_pack <- .usesRdpack(pkgdir)
    for (dx in seq_along(manpages))
    {
        manpage <- manpages[dx]
        rd <- .parse_Rd_pack(manpage, usesRdpack = uses_rd_pack)
        example <- unlist(lapply(rd,
            function(x) attr(x, "Rd_tag") == "\\examples"))
        hasExamples <- any(example)
        if (hasExamples){
            rdCode <- as.character(rd)
            exampleCode <- rdCode[which(rdCode == "\\examples"):length(rdCode)]
            donttestVec <- vapply(exampleCode, grepl, logical(1),
                                  pattern="\\\\donttest", perl=TRUE,
                                  USE.NAMES=FALSE)
            dontrunVec <- vapply(exampleCode, grepl, logical(1),
                                  pattern="\\\\dontrun", perl=TRUE,
                                  USE.NAMES=FALSE)
            ## check for the 'internal' keyword - this will be a false positive
            keyword <- unlist(lapply(rd,
                function(x) attr(x, "Rd_tag") == "\\keyword"))
            if (any(keyword)) {
                internalVec <- vapply(as.character(rd[keyword]), grepl, logical(1),
                                     pattern="internal", USE.NAMES=FALSE)
            } else {
                internalVec <- FALSE
            }
            if (any(donttestVec | dontrunVec) & !any(internalVec))
                hasBad[dx] <- TRUE

            if (any(dontrunVec) & !any(internalVec))
                hasdontrun[dx] <- TRUE
        }
    }
    if (any(hasBad)){
        perVl <- as.character(round(length(which(hasBad))/length(hasBad)*100))
        handleNoteFiles(
            "Usage of dontrun{} / donttest{} tags found in man page examples. ",
            paste0(perVl, "% of man pages use at least one of these tags."),
            messages = basename(manpages)[hasBad]
        )
    }
    if (any(hasdontrun)){
        handleNoteFiles(
            "Use donttest{} instead of dontrun{}.",
            messages = basename(manpages)[hasdontrun]
        )
     }

}

checkNEWS <- function(pkgdir)
{
    newsloc <- file.path(pkgdir, c("inst", "inst", "inst", ".","."),
                         c("NEWS.Rd", "NEWS", "NEWS.md", "NEWS.md", "NEWS"))
    newsFnd <- newsloc[file.exists(newsloc)]
    if (0L == length(newsFnd)){
        handleNote(
            "Consider adding a NEWS file, so your package news will be ",
            "included in Bioconductor release announcements.")
        return()
    }
    if (length(newsFnd) > 1L){
        handleNote(
            "More than 1  NEWS file found. ",
            "See ?news for recognition ordering.",
            help_text = "Please remove one of the following: ",
            messages = gsub(pattern=pkgdir, replacement="", newsFnd)
        )
    }
    news <- head(newsFnd, 1)
    .build_news_db_from_package_NEWS_Rd <-
        get(".build_news_db_from_package_NEWS_Rd", getNamespace("tools"))
    .build_news_db_from_package_NEWS_md <-
        get(".build_news_db_from_package_NEWS_md", getNamespace("tools"))
    .news_reader_default <-
        get(".news_reader_default", getNamespace("tools"))
    tryCatch({
        suppressWarnings({
            db <-
                if (grepl("Rd$", news)){
                    tools:::.build_news_db_from_package_NEWS_Rd(news)
                } else if (grepl("md$", news)){
                    tools:::.build_news_db_from_package_NEWS_md(news)
                } else {
                    tools:::.news_reader_default(news)
                }
        })
    }, error=function(e){
        ## FIXME find a good reference to creating well-formed NEWS, and
        ## reference it here.
        ## Surprisingly, there does not seem to be one.
        handleWarning(
            "Fix formatting of ", basename(news), ". Malformed package NEWS ",
            "will not be included in Bioconductor release announcements.")
    })
}

## This could maybe be more comprehensive, but
## it's what R CMD check does to decide whether
## to run tests.
## OOPS - R CMD check is looking at the INSTALLED directory
checkUnitTests <- function(pkgdir)
{
    tests_dir <- file.path(pkgdir, "tests")
    cond <- length(dir(tests_dir, pattern = "\\.(R|Rin)$"))
    if (dir.exists(tests_dir) && (!cond))
    {
        handleError(
            "Add a .R or .Rin file in tests/ directory or unit tests will ",
            "not be run by R CMD check. See ",
            "https://contributions.bioconductor.org/tests.html")
        return()
    }
    if (!(dir.exists(tests_dir) && cond))
    ## end stolen code
    {
        msg <- paste0(
            "Consider adding unit tests. We strongly encourage them. See",
            "\n  ",
            "https://contributions.bioconductor.org/tests.html"
        )
        handleNote(msg)
    }
}

## check if testthat contains skip_on_bioc() and throw note of it does
checkSkipOnBioc <- function(pkgdir)
{
    testdir <- file.path(pkgdir, "tests", "testthat")
    if (!file.exists(testdir))
        return()

    testfiles <- dir(testdir, pattern = "\\.[Rr]$", full.names = TRUE)
    msg <- vapply(testfiles, function(testfile){
        tokens <- getParseData(parse(testfile, keep.source=TRUE))
        if ("skip_on_bioc" %in% unlist(tokens)) {
            basename(testfile)
        } else NA_character_
    }, character(1))
    msg <- paste(msg[!is.na(msg)], collapse = " ")
    if (nzchar(msg)) {
        handleNote("skip_on_bioc() found in testthat files: ", msg)
    }
}

.lineReport <- function(linedf) {
    paste0(
        linedf[, "File"], "#L", linedf[, "Line"], " ",
        substr(linedf[, "Context"], 1, 40), " ..."
    )
}

.rmYAMLfm <- function(lines) {
    fm_idx <- grep("^---\\s*$", lines)
    offval <- 0L
    if (length(fm_idx) && !identical(length(fm_idx), 2L))
        warning("More than 2 YAML front matter delimiters, i.e., '---' found")
    if (length(fm_idx)) {
        offval <- max(fm_idx)
        lines <- lines[-seq(min(fm_idx), offval)]
    }
    class(lines) <- c("readLines", class(lines))
    attributes(lines) <- list(offset = offval)
    lines
}

.roxygen_in_desc <- function(pkgdir) {
    dcf <- read.dcf(file.path(pkgdir, "DESCRIPTION"))
    "RoxygenNote" %in% colnames(dcf)
}

checkFormatting <- function(pkgdir, nlines=6)
{
    pkgname <- basename(pkgdir)
    files <- c(
        dir(file.path(pkgdir, "R"), pattern="\\.R$", ignore.case=TRUE,
            full.names=TRUE),
        file.path(pkgdir, "NAMESPACE"),
        if (!.roxygen_in_desc(pkgdir))
            dir(file.path(pkgdir, "man"), pattern="\\.Rd$", ignore.case=TRUE,
                full.names=TRUE),
        dir(file.path(pkgdir, "vignettes"), full.names=TRUE,
            pattern="\\.Rnw$|\\.Rmd$|\\.Rrst$|\\.Rhtml$|\\.Rtex$",
            ignore.case=TRUE)
    )
    totallines <- 0L
    ok <- TRUE
    long <- tab <- indent <- Context()

    for (file in files)
    {
        if (file.exists(file) && file.info(file)$size == 0)
        {
            handleNote("Add content to the empty file ",
                .getDirFiles(file))
        }

        if (file.exists(file) && file.info(file)$size > 0)
        {
            lines <- readLines(file, warn=FALSE)
            offset <- 0L
            totallines <- totallines + length(lines)

            n <- nchar(lines, allowNA=TRUE)
            idx <- !is.na(n) & (n > 80L)
            long <- rbind(long, Context(file, lines, idx, offset))

            if (identical(tolower(tools::file_ext(file)), "rmd")) {
                lines <- .rmYAMLfm(lines)
                offset <- attr(lines, "offset")
            }

            idx <- grepl("\t", lines)
            tab <- rbind(tab, Context(file, lines, idx, offset))

            res <- regexpr("^([ ]+)", lines)
            match.length <- attr(res, "match.length")
            idx <- (match.length != -1L) & (match.length %% 4 != 0)
            indent <- rbind(indent, Context(file, lines, idx, offset))
        }
    }

    if (n <- nrow(long))
    {
        ok <- FALSE
        msg <- sprintf(
            "Consider shorter lines; %s lines (%i%%) are > 80 characters long.",
            n, round((n / totallines) * 100))
        msgs <- .lineReport(long)
        handleNote(
            msg,
            help_text = "First few lines:",
            messages = msgs
        )
    }

    if (n <- nrow(tab))
    {
        ok <- FALSE
        msg <- sprintf(
            "Consider 4 spaces instead of tabs; %s lines (%i%%) contain tabs.",
            n, round((n / totallines) * 100))
        msgs <- .lineReport(tab)
        handleNote(msg,
            help_text = "First few lines:",
            messages = msgs
        )
    }

    if (n <- nrow(indent))
    {
        ok <- FALSE
        msgs <- .lineReport(indent)
        handleNote(
            "Consider multiples of 4 spaces for line indents; ", n, " lines",
            " (", round((n / totallines) * 100), "%) are not.",
            help_text = "First few lines:",
            messages = msgs
        )
    }

    if (!ok)
    {
        handleMessage(
            "See https://contributions.bioconductor.org/r-code.html")
        handleMessage(
            "See styler package: https://cran.r-project.org/package=styler ",
            "as described in the BiocCheck vignette.")
    }
}

checkIsPackageNameAlreadyInUse <- function(
    pkgName,
    repo = c(
        "CRAN", "BioCsoft", "BioCann", "BioCexp", "BioCworkflows", "BioCbooks"
    )
) {
    repo <- match.arg(repo)
    msg <- paste0("'", pkgName, "' already exists in Bioconductor.")

    if (identical(repo, "CRAN"))
        msg <- paste(
            "The package already exists on CRAN. Packages submitted to",
            "Bioconductor must be removed from CRAN before the next",
            "Bioconductor release."
        )

    repo.url <- switch(
        repo,
        CRAN = sprintf(
            "%s/src/contrib/PACKAGES", BiocManager::repositories()[repo]
        ),
        BioCsoft = "http://bioconductor.org/packages/devel/bioc/VIEWS",
        BioCann =
            "http://bioconductor.org/packages/devel/data/annotation/VIEWS",
        BioCexp =
            "http://bioconductor.org/packages/devel/data/experiment/VIEWS",
        BioCworkflows =
            "http://bioconductor.org/packages/devel/workflows/VIEWS",
        BioCbooks =
            "https://bioconductor.org/packages/devel/books/VIEWS"
    )

    conn <- url(repo.url)
    on.exit({ close(conn) })
    dcf <- try(read.dcf(conn), silent = TRUE)
    if (is(dcf, "try-error"))
        return(
            handleMessage(
                "Unable to access repository ",
                BiocManager::repositories()[repo]
            )
        )

    isDuplicate <- tolower(pkgName) %in% tolower(dcf[,"Package"])
    if (isDuplicate && identical(repo, "CRAN"))
        handleWarning(msg)
    else if (isDuplicate)
        handleError(msg)
}

#' @importFrom httr2 req_body_form resp_status resp_body_html
checkForBiocDevelSubscription <- function(pkgdir)
{
    email <- getMaintainerEmail(pkgdir)
    if (is.null(email)) {
        handleError(
            "Unable to determine maintainer email from DESCRIPTION file.",
            nframe = 3L
        )
        return()
    }
    if (identical(tolower(email), "maintainer@bioconductor.org"))
    {
        handleMessage("Maintainer email is ok.")
        return()
    }
    response <- try({
        request("https://stat.ethz.ch/mailman/admin/bioc-devel") |>
            req_body_form(
                adminpw = Sys.getenv("BIOC_DEVEL_PASSWORD")
            ) |>
            req_perform()
    }, silent = TRUE)
    if (inherits(response, "try-error")) {
        handleMessage(
            "Unable to connect to the Bioc-devel mailing list:",
            "\n  ", conditionMessage(attr(response, "condition")))
        return()
    } else if (resp_status(response) >= 300) {
        handleMessage(
            "Unable to connect to the Bioc-devel mailing list:",
            "\n  status code ", resp_status(response))
        return()
    }
    response2 <- request(
        "https://stat.ethz.ch/mailman/admin/bioc-devel/members?letter=4") |>
        req_body_form(
            findmember = email, adminpw = Sys.getenv("BIOC_DEVEL_PASSWORD")
        ) |>
        req_perform()
    content <- resp_body_html(response2)
    result_email <- unlist(
        rvest::html_table(content)[[5L]][3L, 2L], use.names = FALSE
    )
    if (identical(tolower(result_email), tolower(email))) {
        handleMessage("Maintainer is subscribed to bioc-devel.")
    } else {
        handleError(
            "Subscribe to the Bioc-devel mailing list by going to ",
            "https://stat.ethz.ch/mailman/listinfo/bioc-devel",
            nframe = 3L
        )
    }
}

checkForSupportSiteRegistration <- function(package_dir)
{
    email <- getMaintainerEmail(package_dir)
    if (identical(tolower(email), "maintainer@bioconductor.org"))
    {
        handleMessage("Maintainer email is ok.")
        return()
    }
    accountExists <- checkSupportReg(email)

    if (accountExists){
        pkgname <- tolower(basename(package_dir))
        checkWatchedTag(email, pkgname)
    }
}

#' @importFrom httr2 req_perform request resp_body_json
checkSupportReg <- function(email){

    url <- paste0("https://support.bioconductor.org/api/email/", email)
    response <- try(
        req_perform(request(url)), silent = TRUE
    )
    response_error <- inherits(response, "try-error")
    result <- !response_error && resp_body_json(response)
    if (response_error) {
        handleError(
            "Unable to find your email in the Support Site:",
            "\n  ", conditionMessage(attr(response, "condition"))
        )
    } else if (resp_body_json(response)) {
        handleMessage("Maintainer is registered at support site.")
    } else {
        handleError(
            "Register your email account in the Support Site; ",
            "visit https://support.bioconductor.org/accounts/signup/"
        )
    }
    result
}

checkWatchedTag <- function(email, pkgname){

    url <- paste0("https://support.bioconductor.org/api/watched/tags/", email)
    response <- try(
        req_perform(request(url)), silent = TRUE
    )
    if (inherits(response, "try-error")) {
        handleMessage(
            "Unable to find your email in the Support Site:",
            "\n  ", conditionMessage(attr(response, "condition"))
        )
    } else {
        alltags <- resp_body_json(response)[["watched_tags"]]
        taglist <- unlist(strsplit(alltags, split = ","))
        tags <- tolower(taglist)
        if (tolower(pkgname) %in% tags)
            handleMessage("Package is in the Support Site Watched Tags.")
        else
            handleError(
                "Add package to Watched Tags in your Support Site profile; ",
                "visit https://support.bioconductor.org/accounts/edit/profile"
            )
    }
}

.HIDDEN_FILE_EXTS <- c(
    ".renviron", ".rprofile", ".rproj", ".rproj.user", ".rhistory",
    ".rapp.history", ".o", ".sl", ".so", ".dylib", ".a", ".dll", ".def",
    ".ds_store", "unsrturl.bst", ".log", ".aux", ".backups", ".cproject",
    ".directory", ".dropbox", ".exrc", ".gdb.history", ".gitattributes",
    ".gitmodules", ".hgtags", ".project", ".seed", ".settings",
    ".tm_properties", ".rdata"
)

# taken from
# https://github.com/wch/r-source/blob/trunk/src/library/tools/R/build.R#L462
# https://github.com/wch/r-source/blob/trunk/src/library/tools/R/check.R#L4025
hidden_file_data <- data.frame(
    file_ext = .HIDDEN_FILE_EXTS,
    hidden_only = c(TRUE, TRUE, FALSE, TRUE, TRUE,
        TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
        TRUE, TRUE, FALSE, FALSE, FALSE, FALSE,
        FALSE, FALSE, FALSE, FALSE, TRUE,
        TRUE, FALSE, TRUE, FALSE, FALSE,
        FALSE, TRUE)
)

# Checks for BiocCheckGitClone --------------------------------------------

checkBadFiles <- function(package_dir){
    swith <- ifelse(hidden_file_data[["hidden_only"]], .Platform$file.sep, "")
    ext_expr <- paste0(
        swith, "\\", hidden_file_data[["file_ext"]], "$", collapse = "|"
    )

    fls <- dir(package_dir, recursive=TRUE, all.files=TRUE)
    flist <- split(fls, startsWith(fls, "inst"))
    warns <- grep(ext_expr, ignore.case = TRUE, flist[['TRUE']], value = TRUE)
    errs <- grep(ext_expr, ignore.case = TRUE, flist[['FALSE']], value = TRUE)

    if (length(warns)) {
        handleWarning(
            "System files in '/inst' should not be Git tracked.",
            messages = warns
        )
    }

    if (length(errs)) {
        handleError(
            "System files found that should not be Git tracked.",
            messages = errs
        )
    }
}

.LICENSE_DB_LOCATION <- "$R_HOME/share/licenses/license.db"

.checkLicenseForRestrictiveUse <- function(license) {
    handleCheck("Checking License: for restrictive use...")

    if (!identical(length(license), 1L) || is.na(license)) {
        handleNote("malformed 'License:' field '", license, "'")
        return(invisible())
    }
    ldb_file <- file.path(R.home("share"), "licenses", "license.db")
    if (!file.exists(ldb_file)) {
        handleNote(
            "license database not found. ",
            "Expected location: '", ldb_file, "'. ",
            "License: '", license, "'"
        )
        return(invisible())
    }
    licenses <- read.dcf(ldb_file)
    result <- tools:::analyze_licenses(license, licenses)
    test <- result[["restricts_use"]]
    if (isTRUE(test))
        handleError("License '", license, "' restricts use")
    else if (is.na(test) || !result[, "is_verified"]) {
        handleNote(
            "License '", license, "' unknown; refer to ", .LICENSE_DB_LOCATION
        )
        handleMessage(
            "and https://choosealicense.com/appendix/ for more info.",
            indent = 6L
        )
    }
}

.checkDESCfields <- function(dcf) {
    handleCheck("Checking for recommended DESCRIPTION fields...")

    fields <- c("URL", "BugReports")
    if ("Date" %in% colnames(dcf)) {
        date <- dcf[, "Date"]
        if (!grepl("^\\d{4}-\\d{2}-\\d{2}$", date))
            handleNote("'Date:' field format is not 'YYYY-MM-DD': ", date)
    }
    present <- fields %in% colnames(dcf)
    res <- fields[!present]
    if (length(res)) {
        notFields <- paste(shQuote(res), collapse = ", ")
        handleNote("Provide ", notFields, " field(s) in DESCRIPTION")
    }
}

.checkBiocDepsDESC <- function(dcf, which = c("Depends", "Imports")) {
    handleCheck("Checking for Bioconductor software dependencies...")
    which_fields <- dcf[, colnames(dcf) %in% which]
    all_deps <- unlist(
        lapply(which_fields, function(x) strsplit(x, ",\\s*")[[1L]]),
        use.names = FALSE
    )
    all_deps <- gsub("(\\w+)\\s+\\(.*\\)$", "\\1", all_deps)
    all_deps <- all_deps[all_deps != "R"]
    repo <- BiocManager:::.repositories_bioc(BiocManager::version())["BioCsoft"]
    biocdb <- utils::available.packages(repos = repo)
    bioc_deps <- all_deps %in% rownames(biocdb)
    percent <- unname(round(prop.table(table(bioc_deps))["TRUE"], 2L) * 100)

    if (!any(bioc_deps)) {
        views <- .parseBiocViews(dcf)
        handleFUN <-
            if ("Infrastructure" %in% views) handleNote else handleWarning
        msg <- "No Bioconductor dependencies detected. Note that some
            infrastructure packages may not have Bioconductor dependencies.
            For more information, reach out to the Bioconductor community
            and/or consider a CRAN submission."
        handleFUN(msg)
    } else {
        handleMessage(
            "Bioconductor dependencies found in Imports & Depends (",
            percent,
            "%)."
        )
    }
}

checkDescription <- function(package_dir) {
    handleCheck("Checking if DESCRIPTION is well formatted...")
    dcf <- tryCatch({
        read.dcf(file.path(package_dir, "DESCRIPTION"))
    }, error = function(err) {
        handleError("DESCRIPTION is malformed.")
        handleMessage(conditionMessage(err))
        return()
    })
    handleCheck("Checking for valid maintainer...")
    authr <- "Authors@R" %in% colnames(dcf)
    autmain <- c("Author","Maintainer") %in% colnames(dcf)
    if (authr && any(autmain))
        handleError(
            "Use Authors@R field not Author/Maintainer fields. Do not use both."
        )
    else if (any(autmain))
        handleError("Do not use Author/Maintainer fields. Use Authors@R.")
}

.checkPinnedDeps <- function(dcf) {
    handleCheck("Checking for pinned package versions in DESCRIPTION...")
    deps <- c("Depends", "Imports", "Suggests", "Enhances", "LinkingTo")
    validdeps <- deps[deps %in% colnames(dcf)]
    doubleeq <- grepl("==", dcf[, validdeps], fixed = TRUE)
    if (any(doubleeq))
        handleError("Dependencies in the DESCRIPTION file contain '=='")
}

checkDESCRIPTIONFile <- function(package_dir) {
    dcf <- read.dcf(file.path(package_dir, "DESCRIPTION"))

    .checkLicenseForRestrictiveUse(dcf[,"License"])
    .checkDESCfields(dcf)
    .checkBiocDepsDESC(dcf)
    .checkPinnedDeps(dcf)
}

checkForCitationFile <- function(package_dir) {
    citfile_location <- file.path(package_dir, "inst", "CITATION")
    if (file.exists(citfile_location)) {
        handleCheck(
            "Checking that provided CITATION file is correctly formatted..."
        )
        cit <- try(readCitationFile(citfile_location), silent = TRUE)
        if (is(cit, "try-error"))
            handleWarning(
                "Unable to read CITATION file with 'utils::readCitationFile()'"
            )
        else if (is.null(cit$doi))
            handleWarning(
                "The 'doi' argument is missing or empty in the CITATION's ",
                "'bibentry()'. Only include a CITATION file if there is a ",
                "preprint or publication associated with this Bioconductor ",
                "package."
            )
    } else {
        handleNote(
            "(Optional) CITATION file not found. Only include a CITATION ",
            "file if there is a preprint or publication for this Bioconductor ",
            "package. Note that Bioconductor packages are not required to ",
            "have a CITATION file but it is useful both for users and for ",
            "tracking Bioconductor project-wide metrics. When including a ",
            "CITATION file, add the publication using the  'doi' argument ",
            "of 'bibentry()'."
        )
    }
}
