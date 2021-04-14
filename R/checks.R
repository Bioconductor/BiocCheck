##########################
#
#  Checks for BiocCheck
#
###########################


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

## Make sure this is run after pkg is installed.
checkForBadDepends <- function(pkgdir)
{
    pkgname <- strsplit(basename(pkgdir), "_")[[1]][1]
    depends <- cleanupDependency(packageDescription(pkgname)$Depends)
    depends <- append(depends,
        cleanupDependency(packageDescription(pkgname)$Imports))
    output <- getBadDeps(pkgdir)
    if (is.null(output)){
        # put these here to be consistent output messaging
        handleCheck("Checking if other packages can import this one...")
        handleCheck("Checking to see if we understand object initialization...")
        return()
    }

    output <- unique(unlist(strsplit(output, "\n")))
    output <- output[grep("no visible", output)]
    if (length(output) == 0){
        # put these here to be consistent output messaging
        handleCheck("Checking if other packages can import this one...")
        handleCheck("Checking to see if we understand object initialization...")
        return()
    }
    res <- regexpr("'[^']*'", output)
    fns <- regexpr("^[^:]*:", output)
    if (all(res == -1L)){
        # put these here to be consistent output messaging
        handleCheck("Checking if other packages can import this one...")
        handleCheck("Checking to see if we understand object initialization...")
        return()
    }

    res <- gsub("'", "", regmatches(output, res))
    fns <- sub(":$", "", regmatches(output, fns))
    inGlobals <- res %in% globalVariables(package=pkgname)
    res <- res[!inGlobals]
    fns <- fns[!inGlobals]

    pkgs <- character(length(fns))
    for (pkg in depends)
        pkgs[res %in% getNamespaceExports(pkg)] <- pkg
    found <- nzchar(pkgs)

    handleCheck("Checking if other packages can import this one...")
    if (any(found)) {
        handleError(
            "Packages providing ", sum(found), " object(s) used in this ",
            "package should be imported in the NAMESPACE file, otherwise ",
            "packages importing this package may fail.")

        msg <- sprintf("  %s::%s in %s()", pkgs[found], res[found], fns[found])
        handleVerbatim(c("", "package::object in function()", msg, ""))
    }

    handleCheck("Checking to see if we understand object initialization...")
    if (!all(found)) {
        n <- sum(!found)
        handleNote(
            "Consider clarifying how ", n, " object(s) are initialized. ",
            "Maybe ", if (n == 1L) "it is" else "they are", " ",
            "part of a data set loaded with data(), or perhaps part of an ",
            "object referenced in with() or within().")

        msg <- sprintf("%s (%s)", fns[!found], res[!found])
        handleVerbatim(c("function (object)", msg))
    }
}

checkDeprecatedPackages <- function(pkgdir)
{
    allDepends <- getAllDependencies(pkgdir)
    allDeprecated <- getAllDeprecatedPkgs()
    if ("multicore" %in% allDepends)
    {
        handleError("Use 'BiocParallel' instead of 'multicore'. ",
            "'multicore' is deprecated and does not work on Windows.")
    }
    logVec <- allDeprecated %in% allDepends
    if (any(logVec)){
        handleError("Package dependency in the DESCRIPTION is 'Deprecated'. ",
                    "Update your package to not rely on the following:")
        for(i in allDeprecated[logVec])
            handleMessage(i, indent=8)
    }
}

checkRemotesUsage <- function(pkgdir)
{
    dcf <- read.dcf(file.path(pkgdir, "DESCRIPTION"))
    if ("Remotes" %in% colnames(dcf))
        handleError("Package dependencies must be on CRAN or Bioconductor. Remove 'Remotes:' from DESCRIPTION")
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
                "New package y version not 99 (e.g., x.99.z, ",
                "x.99.z, ...); got ",sQuote(version), ".")
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
            "http://www.bioconductor.org/developers/how-to/version-numbering/")
        return()
    }
    tryCatch({
        pv <- package_version(version)
        x <- pv$major
        y <- pv$minor
        mod <- y %% 2
        isDevel <- (BiocManager:::.version_bioc("devel") == BiocManager::version())
        bioc.mod <- ifelse(isDevel, 1, 0)
        if (x == 0) {
            handleMessage("Package version ", as.character(pv), "; pre-release")
        } else if (mod != bioc.mod) {
            shouldBe <- ifelse(isDevel, "odd", "even")
            vers <- ifelse(isDevel, "devel", "release")
            handleWarning(
                "y of x.y.z version should be ", shouldBe, " in ", vers)
        }

    }, error=function(e) handleError("Invalid package version"))
}

.compareVersions <- function(pkgVer, RVer = getRversion()[, 1:2]) {
    if (pkgVer < RVer)
        handleNote(
            sprintf("Update R version dependency from %s to %s.", pkgVer, RVer)
        )
}

checkRVersionDependency <- function(package_dir)
{
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
                ver <- as.package_version(verStr)
                .compareVersions(ver)
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
            handleError(
                "Package Source tarball exceeds Bioconductor size requirement.")
            handleMessage(paste0("Package Size: ",
                                 as.character(round(pkgSize/(10^6),4)), " MB"),
                          indent=8)
            handleMessage(paste0("Size Requirement: ",
                                 sprintf("%.4f", round(maxSize/(10^6),4)), " MB"),
                          indent=8)
        }
    }
}

checkIndivFileSizes <- function(pkgdir)
{
    pkgType <- getPkgType(pkgdir)
    if (is.na(pkgType) ||  pkgType == "Software") {
        maxSize <- 5*10^6 ## 5MB
        allFiles <- list.files(pkgdir, all.files=TRUE, recursive=TRUE)
        allFilesFullName <- file.path(pkgdir, allFiles)
        sizes <- file.size(allFilesFullName)
        largeFiles <- paste(allFiles[sizes > maxSize], collapse=" ")
        if (any(sizes > maxSize)) {
            handleWarning(
                "The following files are over 5MB in size: ",
                paste0("'", largeFiles, "'", collapse = " ")
            )
            return(TRUE)
        }
    }
}

.testRbuildignore <- function(text) {
    entries <- Filter(nchar,
        grep("^#", trimws(text), value = TRUE, invert = TRUE)
    )
    grepl("test.*$", entries)
}

checkRbuildignore <- function(pkgdir) {
    rbuildfile <- file.path(pkgdir, ".Rbuildignore")
    if (file.exists(rbuildfile)) {
        rbuild <- readLines(rbuildfile)
        testIgnore <- .testRbuildignore(rbuild)
        if (any(testIgnore))
            handleError(
                ".Rbuildignore file includes the 'tests' folder."
            )
    }
}

checkBiocViews <- function(pkgdir)
{
    dirty <- FALSE
    dcf <- read.dcf(file.path(pkgdir, "DESCRIPTION"))
    handleCheck("Checking that biocViews are present...")
    if (!"biocViews" %in% colnames(dcf))
    {
        handleError("No biocViews terms found.")
        return(TRUE)
    }
    biocViews <- dcf[, "biocViews"]
    views <- strsplit(gsub("\\s*,\\s*", ",", biocViews), ",")[[1]]
    # views <- gsub("\\s", "", views)
    biocViewsVocab <- NULL
    data("biocViewsVocab", package="biocViews", envir=environment())
    handleCheck("Checking package type based on biocViews...")
    type = guessPackageType(views)
    handleMessage(type)
    handleCheck("Checking for non-trivial biocViews...")
    toplevel <- c("Software", "AnnotationData", "ExperimentData", "Workflow")
    if (length(views) == 0){
        handleError("No biocViews terms found.")
        return(TRUE)
    }else{
        if (all(views %in% toplevel)) {
            handleError(
                "Add biocViews other than ", paste(unique(views), collapse=", "))
            return(TRUE)
        }
    }

    parents <-
        unlist(lapply(views, getParent, biocViewsVocab), use.names=FALSE)

    handleCheck("Checking that biocViews come from the same category...")
    if (length(unique(parents)) > 1)
    {
        handleWarning("Use biocViews from one category only ",
            "(one of Software, ExperimentData, AnnotationData, Workflow)")
        return(TRUE)
    }
    branch <- unique(parents)


#    biocViewsVocab <- NULL ## to keep R CMD check happy
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
        distmat[badViews, badViews] = FALSE

        suggestedViews <- vapply(badViews, function(view) {
            alt <- colnames(distmat)[distmat[view,]]
            msg <- paste0("'", view, "' is an invalid BiocViews term.")
            if (length(alt))
                msg <- paste0(
                    msg, " Did you mean: ",
                    paste0("'", alt, "'", collapse = " ")
            )
            msg
        }, character(1))

        handleWarning(unlist(suggestedViews))
        dirty <- TRUE
    }

    if (packageVersion("biocViews") < package_version("1.33.9")) {
        if (branch == "Software") {
            branch = "software"
        } else if (branch == "AnnotationData") {
            branch = "annotation"
        } else if (branch == "ExperimentData") {
            branch = "experiment"
        }
    }

    handleCheck("Checking for recommended biocViews...")
    rec <- tryCatch(suppressMessages(suppressWarnings({
        recommendBiocViews(pkgdir, branch)
    })), error=function(e) {
        NULL
    })

    if (!is.null(rec))
    {
        if (length(rec$recommended) == 1 && !nzchar(rec$recommended)) {
        } else {
            handleNote(
                "Consider adding these automatically suggested biocViews: ",
                rec$recommended)
            dirty <- TRUE
        }
    }
    return(dirty)
}

.checkDescription <- function(desc) {
    dcf <- read.dcf(desc)
    if ("Description" %in% colnames(dcf)) {
        desc_field <- dcf[, "Description"]
        desc_words <- lengths(strsplit(desc_field, split = "[[:space:]]+"))
        desc_sentences <- length(gregexpr("[[:alnum:] ][.!?]", desc_field)[[1]])
        msg <- "The Description field in the DESCRIPTION is made up by less
            than 3 sentences. Please consider expanding this field, and
            structure it as a full paragraph"

        if (nchar(desc_field) < 50 || desc_words < 20) # values chosen sensibly in a data-driven manner
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
            "field (got '", dcf[, "Package"], "').")
        return()
    }
    handleCheck("Checking for Version field...")
    if (!"Version" %in% colnames(dcf))
    {
        handleError("No 'Version:' field in DESCRIPTION.")
        return()
    }
    handleCheck("Checking for valid maintainer...")
    if (!source_tarball){
        if (("Authors@R" %in% colnames(dcf)) & any((c("Author","Maintainer") %in% colnames(dcf)))){
            handleError("Use Authors@R field not Author/Maintainer fields. Do not use both.")
        } else {
            if (any((c("Author","Maintainer") %in% colnames(dcf))))
                handleError("Do not use Author/Maintainer fields. Use Authors@R.")
        }
    }

    maintainer <- NULL
    if ("Authors@R" %in% colnames(dcf))
    {
        env <- new.env(parent=emptyenv())
        env[["c"]] = c
        env[["person"]] <- utils::person
        pp <- parse(text=dcf[,"Authors@R"], keep.source=TRUE)
        tryCatch(people <- eval(pp, env),
            error=function(e) {
                handleError("AuthorsR@ field must be valid R code.")
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
                        "Invalid ORCID ID for ",
                        person$given, " ", person$family
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
    regex = '(.*\\S)\\s*<(.*)>\\s*'
    match <- regexec(regex, maintainer)[[1]]
    match.length <- attr(match, "match.length")
    if (all(match == -1) && all(match.length == -1))
    {
        handleError("Maintainer field in DESCRIPTION file is malformed.")
        return()
    }
}

checkDescriptionNamespaceConsistency <- function(pkgname)
{
    dImports <- cleanupDependency(packageDescription(pkgname)$Imports)
    deps <- cleanupDependency(packageDescription(pkgname)$Depends)
    nImports <- names(getNamespaceImports(pkgname))
    nImports <- nImports[which(nImports != "base")]

    if(!(all(dImports %in% nImports)))
    {
        badones <- dImports[!dImports %in% nImports]
        tryCatch({
            ## FIXME: not 100% confident that the following always succeeds
            dcolon <- .checkEnv(loadNamespace(pkgname), .colonWalker())$done()
            badones <- setdiff(badones, dcolon)
        }, error=function(...) NULL)
        if (length(badones))
            handleWarning(
                "Import ", paste(badones, collapse=", "), " in NAMESPACE ",
                "as well as DESCRIPTION.")
    }
    if (!all (nImports %in% dImports))
    {
        badones <- nImports[!nImports %in% dImports]
        if (!is.null(deps))
        {
            badones <- badones[!badones %in% deps]
        }
        if (length(badones))
        {
            handleWarning(
                "Import ", paste(unique(badones), collapse=", "), " in ",
                "DESCRIPTION as well as NAMESPACE.")
        }
    }
}

checkImportSuggestions <- function(pkgname)
{
    suggestions <- NULL
    tryCatch(suppressMessages(suppressWarnings({
        suggestions <-
            capture.output(codetoolsBioC::writeNamespaceImports(pkgname))
    })), error=function(e) {
        suggestions <- "ERROR"
        handleMessage("Could not get namespace suggestions.")
    })

    if(length(suggestions) && (!is.null(suggestions)) &&
        (suggestions != "ERROR"))
    {
            handleMessage("Namespace import suggestions are:")
            handleVerbatim(suggestions, indent=4, exdent=4, width=100000)
            handleMessage("--END of namespace import suggestions.")
    }

    if ((!is.null(suggestions)) && (!length(suggestions)))
    {
        handleMessage("No suggestions.")
    }

    suggestions
}

checkVignetteDir <- function(pkgdir, checkingDir)
{
    vigdir <- file.path(pkgdir, "vignettes")

    res <- checkVigDirExists(pkgdir, vigdir)
    if (!res)
        return()

    vigdircontents <- getVigSources(vigdir)
    if (length(vigdircontents)==0)
    {
        handleError("No vignette sources in vignettes/ directory.")
        return()
    }

    checkInstContents(pkgdir, checkingDir)

    checkVigFiles(vigdir, vigdircontents)

    desc <- file.path(pkgdir, "DESCRIPTION")
    if (file.exists(desc))
        builder <- getVigBuilder(desc)
    else
        builder <- NULL

    if (!is.null(builder)){
        checkVigBuilder(builder, vigdircontents)
    }

    checkVigMetadata(vigdircontents)

    checkVigEngine(builder, vigdircontents)

    checkVigSuggests(builder, vigdircontents, pkgdir)

    checkVigTemplate(vigdircontents)

    checkVigChunkEval(vigdircontents)

    checkVigBiocInst(pkgdir)

    msg_eval <- checkVigEvalAllFalse(pkgdir)
    if(length(msg_eval) > 0) {
        handleWarning(" Vignette set global option 'eval=FALSE'")
        handleMessage("Found in files:", indent=6)
        for (msg in msg_eval)
            handleMessage(msg, indent=8)
    }

}

checkVigDirExists <- function(pkgdir, vigdir)
{
    if (!file.exists(vigdir))
    {
        handleError("No 'vignettes' directory.")
        return(FALSE)
    } else {
        return(TRUE)
    }
}

checkInstContents <- function(pkgdir, checkingDir)
{
    instdocdir <- file.path(pkgdir, "inst", "doc")
    instdocdircontents <- getVigSources(instdocdir)
    if (length(instdocdircontents) > 0)
    {
        if (checkingDir)
        {
                handleWarning(
                "Remove vignette sources from inst/doc; ",
                "they belong in vignettes/.")
        }
    }
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
            handleNote("Potential intermediate files found:")
            for (msg in badFiles)
                handleMessage(paste0("vignettes/", msg), indent=8)
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
        handleError("VignetteBuilder listed in DESCRIPTION but not ",
                    "found as VignetteEngine in any vignettes:")
        handleMessage(badBuilder, indent=8)
    }
}

checkVigMetadata <- function(vigdircontents)
{
    badVig <- character(0)
    vigExt <- tolower(tools::file_ext(vigdircontents))
    dx <- which(vigExt != "rnw")
    vigdircontents = vigdircontents[dx]
    for (file in vigdircontents) {
        lines <- readLines(file, n=100L, warn=FALSE)
        idx <- grep(lines, pattern="vignette:")
        if (length(idx) == 0L)
            badVig = c(badVig, basename(file))
    }
     if (length(badVig) != 0L){
        handleWarning(
            "Vignette[s] missing Vignette metadata. See ",
            "http://r-pkgs.had.co.nz/vignettes.html ;",
            " Update the following files:"
        )
        handleMessage(badVig, indent=8)
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
    if (length(inval) > 0){
        handleError("More than one VignetteEngine specified.")
        handleMessage("Found in vignette/ files:", indent=6)
        for (msg in inval)
            handleMessage(msg, indent=8)

        dx <- dx[!(basename(vigdircontents[dx]) %in% inval)]
    }
    if (length(dx) != 0) {
        res <-
            vapply(vigdircontents[dx], vigHelper, logical(1), builder=builder)
        if (length(which(!res)) != 0L){
            handleError(
                "VignetteEngine specified but not in DESCRIPTION. ",
                "Add the VignetteEngine from the following files to ",
                "DESCRIPTION:"
            )
            handleMessage(basename(names(which(!res))), indent=8)
        }
        nadx <- which(is.na(res))
        if (length(nadx) != 0L || is.null(builder)){
            handleError(
                "No VignetteEngine specified in vignette or DESCRIPTION. ",
                "Add VignetteEngine to the following files or add a default ",
                "VignetteBuilder in DESCRIPTION: ")
            files = res[nadx]
            if (is.null(builder))
                files = c(files, "DESCRIPTION")
            handleMessage(basename(names(files)), indent=8)
        }
    }
}

checkVigSuggests <- function(builder, vigdircontents, pkgdir)
{
    vigExt <- tolower(tools::file_ext(vigdircontents))
    res <- sapply(vigdircontents, getVigEngine)
    lst <- unique(c(unlist(unname(res)), builder))
    if (any(is.na(lst)))
        lst <- lst[!is.na(lst)]
    dep <- getAllDependencies(pkgdir)
    if (!all(lst %in% dep)){
        handleWarning("Package listed as VignetteEngine or VignetteBuilder ",
                      "but not currently Suggested. ",
                      "Add the following to Suggests in DESCRIPTION:")
        for(i in lst[!(lst %in% dep)])
            handleMessage(i, indent=8)
    }
}

checkVigTemplate <- function(vigdircontents)
{
    badVig <- character(0)
    badVig2 <- character(0)
    for (file in vigdircontents) {
        lines <- readLines(file, n=100L, warn=FALSE)
        idx <- grep(lines, pattern="VignetteIndexEntry")
        if (length(idx) != 0L){
            title <- tolower(gsub(".*\\{|\\}.*", "", lines[idx]))
            if (title == "vignette title"){
                badVig = c(badVig, basename(file))
            }
        }
        if (length(idx) == 0L){
            badVig2 = c(badVig2, basename(file))
        }
    }
    if (length(badVig) != 0L){
        handleWarning(
            "Vignette[s] still using 'VignetteIndexEntry{Vignette Title}' ",
            "Update the following files from using template values:"
        )
        handleMessage(badVig, indent=8)
    }
    if (length(badVig2) != 0L){
        handleWarning(
            "Vignette[s] missing '\\%VignetteIndexEntry{Vignette Title}'. ",
            "Update the following files:"
        )
        handleMessage(badVig2, indent=8)
    }
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
            nonEvalChunk <- lines[grep("^[\t >]*```+\\s*",
                                          lines)][c(TRUE,FALSE)]
            indx <- grep("^[\t >]*```+\\s*\\{([a-zA-Z0-9_]+.*)\\}\\s*$",
                         nonEvalChunk)
            if (length(indx) > 0L)
                nonEvalChunk <- nonEvalChunk[-indx]

        }
        chunks <- chunks + length(chunklines) + length(nonEvalChunk)

        efs <- efs +
            length(grep("eval\\s?=\\s?FALSE", chunklines))

        noneval <- noneval + length(nonEvalChunk)
    }

    percent <- ifelse(chunks == 0 && (efs+noneval) == 0, 0, ((efs+noneval)/chunks) * (100/1))

    if (percent >= 50){
        handleWarning("Evaluate more vignette chunks.")
        handleMessage(sprintf("# of code chunks: %s", chunks), indent=8)
        handleMessage(sprintf("# of eval=FALSE: %s", efs), indent=8)
        handleMessage(sprintf("# of nonexecutable code chunks by syntax: %s", noneval), indent=8)
        handleMessage(sprintf("# total unevaluated %s (%i%%)",(efs+noneval), as.integer(percent)), indent=8)

    }
}

checkVigEvalAllFalse <- function(pkgdir){

    pkgdir <- file.path(pkgdir, "vignettes")
    Vigdir <- sprintf("%s%s", pkgdir, .Platform$file.sep)
    msg_eval <- grepPkgDir(Vigdir,
                           "-rHn 'knitr::opts_chunk\\$set(.*eval\\s*=\\s*F'")
    msg_eval
}

checkVigBiocInst <- function(pkgdir) {
    vigdir <- file.path(pkgdir, "vignettes")
    vigdir <- sprintf("%s%s", vigdir, .Platform$file.sep)
    msg_return <- grepPkgDir(vigdir,
        "-EHrn 'BiocInstaller|biocLite|useDevel|biocinstallRepos'")
    if (length(msg_return)) {
        handleWarning(" BiocInstaller code found in vignette(s)")
        handleMessage("Found in file(s):", indent=6)
        for (msg in msg_return)
            handleMessage(msg, indent=8)
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

checkLibraryCalls <- function(pkgdir)
{
    pkgdir <- file.path(pkgdir, "R")
    rfiles <- dir(
        pkgdir, ignore.case = TRUE, pattern="\\.R$", full.names=TRUE
    )
    badCalls <- c("biocLite", "install.packages", "update.packages", "install")
    msg_installs <- lapply(rfiles, function(rfile){
        tokens <- getParseData(parse(rfile, keep.source=TRUE))
        tokens <- tokens[tokens[,"text"] %in% badCalls, , drop = FALSE]
        sprintf("%s: %d", basename(rfile), tokens[,"line1"])
    })
    msg_installs <- unlist(msg_installs)
    if (length(msg_installs) > 0) {
        handleNote(
            "install, biocLite, install.packages, or update.packages found in R files"
        )
        for (msg in msg_installs)
            handleMessage(msg)
    }
}

checkForLibraryMe <- function(pkgname, parsedCode)
{
    badfiles <- c()
    for (filename in names(parsedCode))
    {
        if (!grepl("\\.R$|\\.Rd$", filename, ignore.case=TRUE))
            next
        df <- parsedCode[[filename]]
        if (nrow(df))
        {
            res <- doesFileLoadPackage(df, pkgname)
            if (length(res))
            {
                badfiles <- append(badfiles, mungeName(filename, pkgname))
            }
        }
    }
    if (length(badfiles))
    {
        msg <- sprintf("The following files call library or require on %s.
            This is not necessary.\n%s", pkgname,
            paste(badfiles, collapse=", "))
        handleWarning(msg)
    }

}

checkCodingPractice <- function(pkgdir, parsedCode, package_name)
{
    Rdir <- file.path(pkgdir, "R")

    # sapply
    msg_sapply <- checkSapply(Rdir)
    if (length(msg_sapply) > 0) {
        handleNote(" Avoid sapply(); use vapply()")
        handleMessage("Found in files:", indent=6)
        for (msg in msg_sapply)
            handleMessage(msg, indent=8)
    }

    # 1:...
    msg_seq <- check1toN(Rdir)
    if (length(msg_seq) > 0) {
        handleNote(" Avoid 1:...; use seq_len() or seq_along()")
        handleMessage("Found in files:", indent=6)
        for (msg in msg_seq)
            handleMessage(msg, indent=8)
    }

    # pkg:fun...
    msg_sc <- checkSingleColon(Rdir)
    if (length(msg_sc)) {
        handleError(" Use double colon for qualified imports: 'pkg::foo()'")
        handleMessage("Found in files:", indent=6)
        for (msg in msg_sc)
            handleMessage(msg, indent=8)
    }

    # message(paste(...))
    msg_mp <- checkPasteInSignaler(Rdir)
    if (length(msg_mp)) {
        handleNote(" Avoid the use of 'paste' in condition signalers")
        handleMessage("Found in files:", indent=6)
        for (msg in msg_mp)
            handleMessage(msg, indent=8)
    }

    # stop("Error: ")
    msg_ss <- checkSignalerInSignaler(Rdir)
    if (length(msg_ss)) {
        handleNote(" Avoid redundancy in signalers")
        handleMessage("Found in files:", indent=6)
        for (msg in msg_ss)
            handleMessage(msg, indent=8)
    }

    # T/F
    res <- checkLogicalUseFiles(pkgdir)
    pkgname <- basename(pkgdir)
    res2 <- findLogicalRdir(pkgname, c("T","F"))
    if (length(c(res,res2)) > 0 ){
        handleWarning(" Use TRUE/FALSE instead of T/F")
        if (length(res2) > 0){
            handleMessage("Found in R/ directory functions:", indent=6)
            for (msg in res2)
                handleMessage(msg, indent=8)
        }
        if (length(res) > 0){
            handleMessage("Found in files:", indent=6)
            for (msg in res)
                handleMessage(msg, indent=8)
        }
    }

    # class() ==
    msg_class <- checkClassEqUsage(pkgdir)
    if (length(msg_class) > 0) {
        handleWarning(" Avoid class() == or class() != ; use is() or !is()")
        handleMessage("Found in files:", indent=6)
        for (msg in msg_class)
            handleMessage(msg, indent=8)
    }

    # system() vs system2()
    msg_sys <- checkSystemCall(pkgdir)
    if(length(msg_sys) > 0) {
        handleNote(" Avoid system() ; use system2()")
        handleMessage("Found in files:", indent=6)
        for (msg in msg_sys)
            handleMessage(msg, indent=8)
    }

    # external data
    msg_eda <- checkExternalData(Rdir)
    if (length(msg_eda)) {
        handleError(" Avoid references to external hosting platforms")
        handleMessage("Found in files:", indent=6)
        for (msg in msg_eda)
            handleMessage(msg, indent=8)
    }

    # set.seed
    res <- findLogicalRdir(pkgname, "set.seed")
    if (length(res) > 0){
        handleWarning(" Remove set.seed usage in R code")
        handleMessage("Found in R/ directory functions:", indent=6)
        for (msg in res)
            handleMessage(msg, indent=8)
    }

    handleCheck("Checking parsed R code in R directory, examples, vignettes...")

    # direct slot access
    checkForDirectSlotAccess(parsedCode, package_name)

    # browser() calls
    res <- findSymbolInParsedCode(parsedCode, package_name, "browser",
                                  "SYMBOL_FUNCTION_CALL")
    if (res > 0)
        handleWarning("Remove browser() statements (found in ", res, " files)")


    # <<-
    res <- findSymbolInParsedCode(parsedCode, package_name, "<<-",
                                  "LEFT_ASSIGN")
    if (res > 0)
        handleNote("Avoid '<<-' if possible (found in ", res, " files)")

}

checkSapply <- function(Rdir){

    rfiles <- dir(Rdir, ignore.case = TRUE, pattern="\\.R$", full.names=TRUE)
    msg_sapply <- lapply(rfiles, function(rfile){
        tokens <- getParseData(parse(rfile, keep.source=TRUE))
        tokens <- tokens[tokens[,"text"] == "sapply", ,drop=FALSE]
        sprintf(
            "%s (line %d, column %d)",
            basename(rfile), tokens[,"line1"], tokens[,"col1"]
        )
    })
    msg_sapply <- unlist(msg_sapply)
}

check1toN <- function(Rdir){

    rfiles <- dir(Rdir, ignore.case = TRUE, pattern="\\.R$", full.names=TRUE)
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

    rfiles <- dir(Rdir, pattern = "\\.[Rr]$", full.names = TRUE)
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

.findSignalerRanges <- function(rfile, tokens) {
    txt <- tokens[, "text"]
    signalers <- which(txt %in% c("message", "warning", "stop"))
    opar <- which(txt == "(")
    startSig <- vapply(signalers, function(x) min(opar[opar > x]), numeric(1L))
    parnum <- tokens[startSig, "parent"]
    endSig <- nrow(tokens) - match(parnum, rev(tokens[, "parent"]))
    Map(seq, startSig, endSig)
}

.findInSignaler <- function(rfile, FUN, ...) {
    tokens <- .getTokens(rfile)
    sigRanges <- .findSignalerRanges(rfile, tokens)
    pasteInd <- lapply(sigRanges, FUN, tokens = tokens, ...)
    tokens <- tokens[unlist(pasteInd), , drop = FALSE]
    sprintf(
        "%s (line %d, column %d)",
        rfile, tokens[, "line1"], tokens[, "col1"]
    )
}

.findPasteInSignaler <- function(rfile) {
    .findInSignaler(rfile, .filtTokens)
}

.grepTokens <-
    function(ind, tokens, keywords = c("message", "warning", "error"))
{
    txt <- tokens[ind, , drop = FALSE]
    strs <- txt$token == "STR_CONST"
    ind <- ind[strs]
    txt <- txt[strs, "text"]
    filt <- grepl(paste0(keywords, collapse = "|"), txt, ignore.case = TRUE)
    ind[filt]
}

.findSignalerInSignaler <- function(rfile) {
    .findInSignaler(rfile, .grepTokens,
        keywords = c("message", "warning", "error"))
}

checkPasteInSignaler <- function(Rdir) {
    rfiles <- dir(Rdir, pattern = "\\.[Rr]$", full.names = TRUE)
    pasteSig <- lapply(rfiles, .findPasteInSignaler)
    pasteSig <- unlist(pasteSig)
}

checkSignalerInSignaler <- function(Rdir) {
    rfiles <- dir(Rdir, pattern = "\\.[Rr]$", full.names = TRUE)
    sisig <- lapply(rfiles, .findSignalerInSignaler)
    sisig <- unlist(sisig)
}

checkClassEqUsage <- function(pkgdir){

    regex <- "\\bclass\\s*(.*)\\s*[!=]="
    pkgdir <- sprintf("%s%s", pkgdir, .Platform$file.sep)
    # Limit to R files and vignette source files
    Rdir <- sprintf("%s%s%s", pkgdir, "R", .Platform$file.sep)
    fnd1 <-
        if(dir.exists(Rdir)){
            grepPkgDir(Rdir, paste0('-rHn "', regex, '"'),
                       full_path=TRUE)
        } else {
            character(0)
        }
    VigFiles <- getVigSources(sprintf("%s%s", pkgdir,"vignettes"))
    fnd2 <- unlist(lapply(VigFiles,
                          FUN=grepPkgDir, paste0('-rHn "', regex, '"'),
                          full_path=TRUE))
    msg_sys <- sub(c(fnd1, fnd2), pattern=pkgdir, replacement="", fixed=TRUE)
}

checkSystemCall <- function(pkgdir){

    pkgdir <- sprintf("%s%s", pkgdir, .Platform$file.sep)
    msg_sys <- grepPkgDir(pkgdir, "-rHn '^system(.*'")
}

checkExternalData <- function(Rdir) {

    rfiles <- dir(Rdir, pattern="\\.[Rr]$", full.names=TRUE)
    msg_eda <- lapply(rfiles, function(rfile) {
        tokens <- getParseData(parse(rfile, keep.source=TRUE))
        tokens <- tokens[tokens[,"token"] == "STR_CONST", ,drop=FALSE]

        platforms <- "githubusercontent|github|gitlab|bitbucket|dropbox"

        hits <- grepl(platforms, tokens[, "text"], ignore.case = TRUE)
        if (any(hits))
            tokens <- tokens[hits, , drop = FALSE]
        else
            tokens <- tokens[FALSE, , drop = FALSE]

        sprintf(
            "%s (line %d, column %d)",
            basename(rfile), tokens[,"line1"], tokens[,"col1"]
        )
    })
    unlist(msg_eda)
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
    df <- data.frame(stringsAsFactors=FALSE)
    i <- 1
    for (filename in names(parsedCode))
    {
        message(".", appendLF=FALSE)
        pc <- parsedCode[[filename]]
        filename <- mungeName(filename, pkgname)
        res <- getFunctionLengths(pc)
        for (name in names(res)) {
            x <- res[[name]]
            if (length(x))
            {
                df[i,1] <- filename
                df[i,2] <- name
                df[i,3] <- x['length']
                df[i,4] <- x['startLine']
                df[i,5] <- x['endLine']
            }
            i <- i + 1
        }
    }
    message("")
    colnames <- c("filename","functionName","length","startLine","endLine")
    if (ncol(df) == length(colnames))
    {
        colnames(df) <- colnames
        df <- df[with(df, order(-length)),]
        h <- df[df$length > 50,]
        if (nrow(h))
        {
            handleNote("Recommended function length <= 50 lines.")
            handleMessage("There are ", nrow(h), " functions > 50 lines.",
                          indent=6)
            h = head(df, n=5)
            handleMessage("The longest ", nrow(h), " functions are:",
                          indent=6)
            for (i in seq_len(nrow(h)))
            {
                row <- df[i,]
                if (grepl("\\.R$", row$filename, ignore.case=TRUE))
                {
                    handleMessage(sprintf(
                        "%s() (%s, line %s): %s lines", row$functionName,
                        row$filename, row$startLine, row$length), indent=8)
                } else {
                    handleMessage(sprintf(
                        "%s() (%s): %s lines", row$functionName, row$filename,
                        row$length), indent=8)
                }
            }
        }
    }
}

checkManDocumentation <- function(package_dir, package_name)
{
    # canned man prompts
    checkForPromptComments(package_dir)

    # non empty value section exists
    checkForValueSection(package_dir)

    # exports are documented and 80% runnable
    checkExportsAreDocumented(package_dir, package_name)

    # usage of donttest and dontrun
    checkUsageOfDont(package_dir)
}

checkForPromptComments <- function(pkgdir)
{
    manpages <- dir(file.path(pkgdir, "man"),
        pattern="\\.Rd$", ignore.case=TRUE, full.names=TRUE)

    bad <- c()
    for (manpage in manpages)
    {
        lines <- readLines(manpage, warn=FALSE)
        if (any(grepl("^%% ~", lines)))
            bad <- append(bad, basename(manpage))
    }
    if (length(bad) > 0)
    {
        handleNote(
            "Remove generated comments from man pages ",
            paste(bad, collapse=", "))
    }
}

checkForValueSection <- function(pkgdir)
{
    pkgname <- basename(pkgdir)
    manpages <- dir(file.path(pkgdir, "man"),
        pattern="\\.Rd$", ignore.case=TRUE, full.names=TRUE)
    ok <- vapply(manpages, function(manpage) {
        rd <- parse_Rd(manpage)
        tags <- tools:::RdTags(rd)

        type <- docType(rd)
        if (identical(type, "data"))
            return(TRUE)

        value <- NULL
        if ("\\usage" %in% tags && (!"\\value" %in% tags))
            return(FALSE)

        if ("\\value" %in% tags)
            value <- rd[grep("\\value", tags)]

        if ("\\usage" %in% tags && !is.null(value))
        {
            values <- paste(unlist(value), collapse='')
            tst <- (is.list(value[[1]]) && length(value[[1]]) == 0) ||
                nchar(gsub("^\\s+|\\s+$", "", values)) == 0
            if (tst)
                return(FALSE)
        }
        TRUE
    }, logical(1))
    if (!all(ok)) {
        handleWarning(
            "Add non-empty \\value sections to the following man pages: ",
            paste(mungeName(manpages[!ok], pkgname), collapse=", "))
    }
}

# Which pages document things that are exported?
checkExportsAreDocumented <- function(pkgdir, pkgname)
{
    manpages <- dir(file.path(pkgdir, "man"),
        pattern="\\.Rd$", ignore.case=TRUE, full.names=TRUE)
    exports <- getNamespaceExports(pkgname)
    badManPages <- character(0)
    exportingPagesCount <- 0L
    noExamplesCount <- 0L

    for (manpage in manpages)
    {
        rd <- parse_Rd(manpage)
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
    if (exportingPagesCount > 0
        && ratio  < (0.8 / 1.0))
    {
        handleError(
            "At least 80% of man pages documenting exported objects must ",
            "have runnable examples. The following pages do not:")
    } else if (length(badManPages) > 0) {
        handleNote(
            "Consider adding runnable examples to the following ",
            "man pages which document exported objects:")
    }
    if (length(badManPages) > 0)
        .msg(paste(badManPages, collapse=", "), indent=6)


    badManPages # for testing
}

checkUsageOfDont <- function(pkgdir)
{
    manpages <- dir(file.path(pkgdir, "man"),
        pattern="\\.Rd$", ignore.case=TRUE, full.names=TRUE)

    hasBad <- rep(FALSE, length(manpages))
    hasdontrun <- rep(FALSE, length(manpages))
    for (dx in seq_along(manpages))
    {
        manpage <- manpages[dx]
        rd <- parse_Rd(manpage)
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
                hasBad[dx] = TRUE

            if (any(dontrunVec) & !any(internalVec))
                hasdontrun[dx] = TRUE
        }
    }
    if (any(hasBad)){
        perVl <- as.character(round(length(which(hasBad))/length(hasBad)*100))
        handleNote("Usage of dontrun{} / donttest{} found in man page examples.")
        handleMessage(perVl, "% of man pages use one of these cases.", indent=6)
        handleMessage("Found in the following files:", indent=6)
        for(f in basename(manpages)[hasBad]){
            handleMessage(f, indent=8)
        }
    }
    if (any(hasdontrun)){
        handleNote("Use donttest{} instead of dontrun{}.")
        handleMessage("Found in the following files:", indent=6)
        for(f in basename(manpages)[hasdontrun]){
            handleMessage(f, indent=8)
        }
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
            "More than 1  NEWS file found.",
            "\nSee ?news for recognition ordering.",
            "\nPlease remove one of the following: ")
        handleMessage(gsub(pattern=pkgdir, replacement="", newsFnd),
                      indent=8)
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
    ## begin code stolen from tools:::.check_packages
    dir.exists <- function(x) !is.na(isdir <- file.info(x)$isdir) &
        isdir
    ## ...
    tests_dir <- file.path(pkgdir, "tests")
    cond <- length(dir(tests_dir, pattern = "\\.(R|Rin)$"))
    if (dir.exists(tests_dir) && (!cond))
    {
        handleError(
            "Add a .R or .Rin file in tests/ directory or unit tests will ",
            "not be run by R CMD check. See ",
            "http://bioconductor.org/developers/how-to/unitTesting-guidelines/")
        return()
    }
    if (!(dir.exists(tests_dir) && cond))
    ## end stolen code
    {
        msg <- paste0(
            "Consider adding unit tests. We strongly encourage them. See",
            "\n  ",
            "http://bioconductor.org/developers/how-to/unitTesting-guidelines/."
        )
        handleNote(msg)
    }
}

## check if testthat contains skip_on_bioc() and throw note of it does
checkSkipOnBioc <- function(pkgdir)
{
    pkgdir <- file.path(pkgdir, "tests", "testthat")
    if (file.exists(pkgdir)) {
        testfiles <- list.files(pkgdir, pattern = ".R$")
        testfiles_full <- file.path(pkgdir, testfiles)
        msg <- lapply(seq_along(testfiles), function(idx){
            tokens <- getParseData(parse(testfiles_full[idx], keep.source=TRUE))
            if ("skip_on_bioc" %in% unlist(tokens))
                testfiles[idx]
        })
        msg <- paste(unlist(msg), collapse = " ")
        if (msg != "") {
            handleNote("skip_on_bioc() found in testthat files: ", msg)
        }
    }
}

checkFormatting <- function(pkgdir, nlines=6)
{
    pkgname <- basename(pkgdir)
    files <- c(
        dir(file.path(pkgdir, "R"), pattern="\\.R$", ignore.case=TRUE,
            full.names=TRUE),
        file.path(pkgdir, "NAMESPACE"),
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
                mungeName(file, pkgname))
        }

        if (file.exists(file) && file.info(file)$size > 0)
        {
            lines <- readLines(file, warn=FALSE)
            totallines <- totallines + length(lines)

            n <- nchar(lines, allowNA=TRUE)
            idx <- !is.na(n) & (n > 80L)
            long <- rbind(long, Context(pkgname, file, lines, idx))

            idx <- grepl("\t", lines)
            tab <- rbind(tab, Context(pkgname, file, lines, idx))

            res <- regexpr("^([ ]+)", lines)
            match.length <- attr(res, "match.length")
            idx <- (match.length != -1L) & (match.length %% 4 != 0)
            indent <- rbind(indent, Context(pkgname, file, lines, idx))
        }
    }

    if (n <- nrow(long))
    {
        ok <- FALSE
        handleNote(sprintf(
            "Consider shorter lines; %s lines (%i%%) are > 80 characters long.",
            n, round((n / totallines) * 100)))
        handleContext(long, nlines)
    }

    if (n <- nrow(tab))
    {
        ok <- FALSE
        handleNote(sprintf(
            "Consider 4 spaces instead of tabs; %s lines (%i%%) contain tabs.",
            n, round((n / totallines) * 100)))
        handleContext(tab, nlines)
    }

    if (n <- nrow(indent))
    {
        ok <- FALSE
        handleNote(
            "Consider multiples of 4 spaces for line indents, ", n, " lines",
            "(", round((n / totallines) * 100), "%) are not.")
        handleContext(indent, nlines)
    }

    if (!ok)
    {
        handleMessage(
            "See http://bioconductor.org/developers/how-to/coding-style/")
        handleMessage(
            "See styler package: https://cran.r-project.org/package=styler ",
            "as described in the BiocCheck vignette.")
    }
}

checkIsPackageAlreadyInRepo <- function(pkgName, repo=c("CRAN", "BioCsoft",
                                                     "BioCann", "BioCexp", "BioCworkflows"))
{
    repo <- match.arg(repo)

    if (repo == "CRAN"){
        repo.url <- sprintf("%s/src/contrib/PACKAGES", BiocManager::repositories()[repo])
    }else{
        repo.url <- switch(repo,
                           BiocSoft = "http://bioconductor.org/packages/devel/bioc/VIEWS",
                           BioCann =
                               "http://bioconductor.org/packages/devel/data/annotation/VIEWS",
                           BioCexp =
                               "http://bioconductor.org/packages/devel/data/experiment/VIEWS",
                           BioCworkflows =
                               "http://bioconductor.org/packages/devel/workflows/VIEWS",
                           "http://bioconductor.org/packages/devel/bioc/VIEWS")
    }

    conn <- url(repo.url)
    dcf <- tryCatch(suppressWarnings(read.dcf(conn)), error=identity)
    close(conn)
    if (inherits(dcf, "error")) {
        handleMessage("Unable to access repository ", BiocManager::repositories()[repo])
    } else if (tolower(pkgName) %in% tolower(dcf[,"Package"])) {
        if (repo == "CRAN")
            msg <- "Package must be removed from CRAN."
        else {
            msg <- paste0("'", pkgName, "' already exists in Bioconductor.")
        }
        handleError(msg)
    }
}

checkForBiocDevelSubscription <- function(pkgdir)
{
    email <- getMaintainerEmail(pkgdir)
    if (!exists("email"))
        return()
    if (!nzchar(Sys.getenv("BIOC_DEVEL_PASSWORD")))
    {
        msg <-
            "Cannot determine whether maintainer is subscribed to the
            bioc-devel mailing list (requires admin credentials).
            Subscribe here: https://stat.ethz.ch/mailman/listinfo/bioc-devel"
        handleNote(paste(strwrap(msg), collapse="\n"))
        return()
    }
    if (tolower(email) == "maintainer@bioconductor.org")
    {
        handleMessage("Maintainer email is ok.")
        return()
    }
    response <- tryCatch({
        POST(
            "https://stat.ethz.ch/mailman/admin/bioc-devel",
            body=list(adminpw=Sys.getenv("BIOC_DEVEL_PASSWORD")))
    }, error=identity)
    if (inherits(response, "error")) {
        handleMessage(
            "Unable to connect to mailing list",
            "\n  ", conditionMessage(response))
        return()
    } else if (status_code(response) >= 300) {
        handleMessage(
            "Unable to connect to mailing list",
            "\n  status code ", status_code(response))
        return()
    }
    response2 <- POST(
        "https://stat.ethz.ch/mailman/admin/bioc-devel/members?letter=4",
        body=list(findmember=email))
    content <- content(response2, as="text")
    if(grepl(paste0(">", tolower(email), "<"), tolower(content), fixed=TRUE))
    {
        handleMessage("Maintainer is subscribed to bioc-devel.")
    } else {
        handleError(
            "Maintainer must subscribe to the bioc-devel mailing list. ",
            "Subscribe here: https://stat.ethz.ch/mailman/listinfo/bioc-devel")
    }
}

checkForSupportSiteRegistration <- function(package_dir)
{
    email <- getMaintainerEmail(package_dir)
    if (tolower(email) == "maintainer@bioconductor.org")
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

checkSupportReg <- function(email){

    url <- paste0("https://support.bioconductor.org/api/email/", email, "/")
    response <- tryCatch(GET(url), error=identity)
    if (inherits(response, "error")) {
        handleMessage(
            "Unable to connect to support site:",
            "\n  ", conditionMessage(response))
        FALSE
    } else if (suppressMessages(content(response))) {
        handleMessage("Maintainer is registered at support site.")
        TRUE
    } else {
        handleError("Maintainer must register at the support site; ",
                    "visit https://support.bioconductor.org/accounts/signup/ .")
        FALSE
    }
}

checkWatchedTag <- function(email, pkgname){

    url <- paste0("https://support.bioconductor.org/api/watched/tags/", email, "/")
    response <- tryCatch(GET(url), error=identity)
    if (inherits(response, "error")) {
        handleMessage(
            "Unable to connect to support site:",
            "\n  ", conditionMessage(response))
    } else {
        tags<-tolower(trimws(unlist(strsplit(content(response)$watched_tags, split=","))))
        if (pkgname %in% tags){
            handleMessage("Package name is in support site watched tags.")
        }else{
            handleError("Maintainer must add package name to Watched Tags on the support site; ",
                        "Edit your Support Site User Profile to add Watched Tags.")
        }
    }
}

#######################################
#
#  Checks for BiocCheckGitClone
#
#######################################


checkBadFiles <- function(package_dir){
    # taken from
    #https://github.com/wch/r-source/blob/trunk/src/library/tools/R/build.R#L462
    # and
    #https://github.com/wch/r-source/blob/trunk/src/library/tools/R/check.R#L4025
    hidden_file_ext = c(".renviron", ".rprofile", ".rproj", ".rproj.user",
                       ".rhistory", ".rapp.history",
                       ".o", ".sl", ".so", ".dylib",
                       ".a", ".dll", ".def",
                       ".ds_store", "unsrturl.bst",
                       ".log", ".aux",
                       ".backups", ".cproject", ".directory",
                       ".dropbox", ".exrc", ".gdb.history",
                       ".gitattributes", ".gitmodules",
                       ".hgtags",
                       ".project", ".seed", ".settings", ".tm_properties")

    fls <- dir(package_dir, ignore.case=TRUE, recursive=TRUE, all.files=TRUE)
    dx <- unlist(lapply(hidden_file_ext,
        FUN=function(x, suffix){
            which(endsWith(x, suffix))
        }, x=tolower(fls)))
    badFiles <- fls[dx]

    if (length(badFiles) != 0){
        handleError("System Files found that should not be git tracked:")
        for(msg in badFiles)
            handleMessage(msg, indent=8)
    }
}

.checkLicenseForRestrictiveUse <- function(license) {
    handleCheck("Checking License: for restrictive use...")

    if (length(license) != 1L || is.na(license)) {
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
    sss <- licenses[, "SSS"]
    abbrev <- licenses[, "Abbrev"]
    abbrev[!is.na(sss)] <- sss[!is.na(sss)]
    restrict <- licenses[, "Restricts_use"]
    idx <- (restrict == "yes") & !is.na(restrict) & !is.na(abbrev)

    ## PCAN/DESCRIPTION:License: CC BY-NC-ND 4.0
    ## QUBIC/DESCRIPTION:License: CC BY-NC-ND 4.0 + file LICENSE
    test0 <- any(vapply(abbrev[idx], grepl, logical(1), license, fixed = TRUE))
    test1 <-
        !any(vapply(
             abbrev[!is.na(abbrev)], grepl, logical(1), license, fixed = TRUE
         ))
    if (test0) {
        handleError("License '", license, "' restricts use")
    } else if (test1) {
        handleNote(
            "License '", license, "' unknown; licenses cannot restrict use"
        )
    }
}

checkDescription <- function(package_dir){

    handleCheck("Checking if DESCRIPTION is well formatted...")
    dcf <- tryCatch({
        read.dcf(file.path(package_dir, "DESCRIPTION"))
    }, error = function(err) {
        handleError("DESCRIPTION is malformed.")
        handleMessage(conditionMessage(err))
        return()
    })
    handleCheck("Checking for valid maintainer...")
    if (("Authors@R" %in% colnames(dcf)) & any((c("Author","Maintainer") %in% colnames(dcf)))){
        handleError("Use Authors@R field not Author/Maintainer fields. Do not use both.")
    } else {
        if (any((c("Author","Maintainer") %in% colnames(dcf))))
            handleError("Do not use Author/Maintainer fields. Use Authors@R.")
    }
}

checkDESCRIPTIONFile <- function(package_dir) {
    dcf <- read.dcf(file.path(package_dir, "DESCRIPTION"))

    .checkLicenseForRestrictiveUse(dcf[,"License"])
}

checkForCitationFile <- function(package_dir) {
    citfile_location <- file.path(package_dir, "inst", "CITATION")
    if(file.exists(citfile_location)) {
        handleCheck("Checking that provided CITATION file is correctly formatted...")
        cit <- tryCatch(
            readCitationFile(citfile_location),
            error = function(e)
                handleNote("CITATION file might be not correctly formatted"))
    }
}
