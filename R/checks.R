getVigSources <- function(dir)
{
    dir(dir,
        pattern="\\.Rmd$|\\.Rnw$|\\.Rrst$|\\.Rhtml$|\\.Rtex$",
        ignore.case=TRUE, full.names=TRUE)
}

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
                bv <- package_version(sprintf("%s.%s",
                    getRversion()$major,
                    getRversion()$minor))
                if (ver < bv)
                {
                    handleWarning(
                        "Update R version dependency from ", ver, " to ", bv,
                        ".")
                }
            }
        }
    }
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
    #
    # This appears to never get run because of pkgdir as character
    # tools::pkgVignettes
    # 'pkgVignettes' returns an object of class '"pkgVignettes"' if a
    #  vignette directory is found, otherwise 'NULL'.
    #  what is this doing?
    #
    res <- checkToolsVig(pkgdir)
    if (!res)
        return()

    desc <- file.path(pkgdir, "DESCRIPTION")
    if (file.exists(desc))
        builder <- getVigBuilder(desc)
    else
        builder <- NULL

    if (!is.null(builder)){
        checkVigBuilder(builder, vigdircontents)
    }

    checkVigEngine(builder, vigdircontents)

    checkVigTemplate(vigdircontents)

    checkVigChunkEval(vigdircontents)

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
        if (isInfrastructurePackage(pkgdir))
        {
            .msg("  Infrastructure package, vignette not required.",
                indent=2)
            return(FALSE)
        }
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

checkToolsVig <- function(pkgdir)
{
    if (file.exists(file.path("pkgdir", "DESCRIPTION")))
    {
        vigns <- tools::pkgVignettes(dir=pkgdir, check=TRUE)
        if (is.null(vigns))
        {
            handleError("No vignette found.")
            return(FALSE)
        }
        if (length(vigns$msg))
        {
            handleError(paste0(vigns$msg, collapse="\n"))
            return(FALSE)
        }
    } else {
        return(TRUE)
    }
}

getVigBuilder <- function(desc)
{
    dcf <- read.dcf(desc)
    if (!"VignetteBuilder" %in% colnames(dcf)) {
        builder <- NULL
    } else {
        builder <- strsplit(gsub(" ", "",dcf[, "VignetteBuilder"], fixed=TRUE),
                            ",")[[1]]
    }
    return(builder)
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
        handleMessage(badBuilder, indent=6)
    }
}

vigHelper <- function(vignetteFile, builder){
    lines <- readLines(vignetteFile, n=100L, warn=FALSE)
    idx <- grep(lines, pattern="VignetteEngine")
    if (length(idx) != 0){
        eng <- gsub("::.*", "", gsub(".*\\{|\\}.*", "", lines[idx]))
        return(eng %in% builder)
    } else {
        return(NA)
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
                             "-rn 'VignetteEngine{'")
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
            handleMessage(basename(names(which(!res))), indent=6)
        }
        nadx <- which(is.na(res))
        if (length(nadx) != 0L && is.null(builder)){
            handleError(
                "No VignetteEngine specified in vignette or DESCRIPTION. ",
                "Add VignetteEngine to the following files or add a default ",
                "VignetteBuilder in DESCRIPTION: ")
            files = res[nadx]
            handleMessage(basename(names(files)), indent=6)
        }
    }
}

checkVigTemplate <- function(vigdircontents)
{
    badVig <- character(0)
    for (file in vigdircontents) {
        lines <- readLines(file, n=100L, warn=FALSE)
        idx <- grep(lines, pattern="VignetteIndexEntry")
        if (length(idx) != 0L){
            title <- tolower(gsub(".*\\{|\\}.*", "", lines[idx]))
            if (title == "vignette title"){
                badVig = c(badVig, basename(file))
            }
        }
    }
    if (length(badVig) != 0L){
        handleWarning(
            "Vignette[s] still using 'VignetteIndexEntry{Vignette Title}' ",
            "Update the following files from using template values:"
        )
        handleMessage(badVig, indent=6)
    }
}

checkVigChunkEval <- function(vigdircontents)
{
    chunks <- 0
    efs <- 0
    for (file in vigdircontents)
    {
        lines <- readLines(file, warn=FALSE)
        vignetteType <- knitr:::detect_pattern(lines, tools::file_ext(file))
        if (is.null(vignetteType)) {
            chunklines <- character(0)
        } else {
            chunkPattern <- knitr::all_patterns[[vignetteType]]$chunk.begin
            chunklines <- lines[grep(chunkPattern, lines)]
        }
        chunks <- chunks + length(chunklines)

        efs <- efs +
            length(grep("eval\\s?=\\s?FALSE", chunklines))
    }

    percent <- ifelse(chunks == 0 && efs == 0, 0, (efs/chunks) * (100/1))

    handleMessage(sprintf(
        "# of chunks: %s, # of eval=FALSE: %s (%i%%)",
        chunks, efs,  as.integer(percent)))
    if (percent >= 50)
        handleWarning("Evaluate more vignette chunks.")
}

checkVigEvalAllFalse <- function(pkgdir){

    pkgdir <- file.path(pkgdir, "vignettes")
    Vigdir <- sprintf("%s%s", pkgdir, .Platform$file.sep)
    msg_eval <- grepPkgDir(Vigdir,
                           "-rn 'knitr::opts_chunk\\$set(.*eval\\s*=\\s*F'")
    msg_eval
}

checkNewPackageVersionNumber <- function(pkgdir)
{
    dcf <- read.dcf(file.path(pkgdir, "DESCRIPTION"))
    version <- dcf[, "Version"]
        if(!grepl("^0[-.]99[-.][0-9]+$", version))
            handleError(
                "New package version starting with 0.99.* (e.g., 0.99.0, ",
                "0.99.1, ...); got ",sQuote(version), ".")
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

getPkgType <- function(pkgdir)
{
    dcf <- read.dcf(file.path(pkgdir, "DESCRIPTION"))
    if (!"biocViews" %in% colnames(dcf))
    {
        return(NA)
    }
    biocViews <- dcf[, "biocViews"]
    views <- strsplit(gsub("\\s", "", biocViews), ",")[[1]]
    biocViewsVocab <- NULL ## to keep R CMD check happy
    data("biocViewsVocab", package="biocViews", envir=environment())
    if (any(!views %in% nodes(biocViewsVocab)))
        return(NA)
    parents <- c()
    for (view in views)
    {
        parents <- c(parents, getParent(view, biocViewsVocab))
    }
    u <- unique(parents)
    if (length(u)==1) return(u) else return(NA)
}

getParent <- function(view, biocViewsVocab)
{
    topLevel <- c("Software", "ExperimentData", "AnnotationData")
    if (view %in% topLevel)
        return(view)
    for (level in topLevel) {
        if (view %in% names(acc(biocViewsVocab, level)[[level]]))
            return(level)
    }
}

checkPackageSize <- function(pkg, pkgdir, size=4){
    pkgType <- getPkgType(pkgdir)
    if (is.na(pkgType) ||  pkgType == "Software") {
        maxSize <- size*10^6 ## 4MB
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
    toplevel <- c("Software", "AnnotationData", "ExperimentData")
    if (all(views %in% toplevel)) {
        handleError(
            "Add biocViews other than ", paste(unique(views), collapse=", "))
        return(TRUE)
    }

    parents <-
        unlist(lapply(views, getParent, biocViewsVocab), use.names=FALSE)

    handleCheck("Checking that biocViews come from the same category...")
    if (length(unique(parents)) > 1)
    {
        handleWarning("Use biocViews from one category only ",
            "(one of Software, ExperimentData, AnnotationData)")
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

checkBBScompatibility <- function(pkgdir)
{
    lines <- readLines(file.path(pkgdir, "DESCRIPTION"), warn=FALSE)
    handleCheck("Checking for blank lines in DESCRIPTION...")
    if (any(nchar(lines)==0))
    {
        handleError("Remove blank lines from DESCRIPTION.")
        return()
    }
    handleCheck("Checking for whitespace in DESCRIPTION field names...")
    dcf <- read.dcf(file.path(pkgdir, "DESCRIPTION"))
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
        for (person in people)
        {
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
        maintainer <- dcf[,"Maintainer"]
    } else {
        handleError("No Maintainer or Authors@R field in DESCRIPTION file.")
        return()
    }
    # now need to make sure that regexes work, a la python/BBS
    regex = '(.*\\S)\\s*<(.*)>\\s*'
    match <- regexec(regex, maintainer)[[1]]
    match.length <- attr(match, "match.length")
    #if (!  (all(match)  > 0) && (all(match.length) > 0) )
    if (match == -1 && match.length == -1)
    {
        handleError("Maintainer field in DESCRIPTION file is malformed.")
        return()
    }
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

checkCodingPractice <- function(pkgdir)
{
    Rdir <- file.path(pkgdir, "R")

    # sapply
    msg_sapply <- checkSapply(Rdir)
    if (length(msg_sapply) > 0) {
        handleNote("Avoid sapply(); use vapply() found in files:")
        for (msg in msg_sapply)
            handleMessage(msg, indent=6)
    }

    # 1:...
    msg_seq <- check1toN(Rdir)
    if (length(msg_seq) > 0) {
        handleNote(" Avoid 1:...; use seq_len() or seq_along() found in files:")
        for (msg in msg_seq)
            handleMessage(msg, indent=6)
    }

    # T/F
    res <- checkLogicalUseFiles(pkgdir)
    pkgname <- basename(pkgdir)
    res2 <- findLogicalRdir(pkgname, c("T","F"))
    if (length(c(res,res2)) > 0 ){
        handleWarning("Use TRUE/FALSE instead of T/F")
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
        handleWarning(" Avoid class()== or class()!= ; use is() or !is()")
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

    # set.seed
    res <- findLogicalRdir(pkgname, "set.seed")
    if (length(res) > 0){
        handleWarning("Remove set.seed usage in R code")
        handleMessage("Found in R/ directory functions:", indent=6)
        for (msg in res)
            handleMessage(msg, indent=8)
    }

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

checkClassEqUsage <- function(pkgdir){

    pkgdir <- sprintf("%s%s", pkgdir, .Platform$file.sep)
    fnd <- grepPkgDir(pkgdir, "-rn 'class(.*)\\s*=='")
    fnd2 <-grepPkgDir(pkgdir, "-rn 'class(.*)\\s*!='")
    msg_class <- sort(c(fnd, fnd2))
    msg_class
}

checkSystemCall <- function(pkgdir){

    pkgdir <- sprintf("%s%s", pkgdir, .Platform$file.sep)
    msg_sys <- grepPkgDir(pkgdir, "-rn '^system(.*'")
}

checkRegistrationOfEntryPoints <- function(pkgname, parsedCode)
{
    symbols <-  c(".C", ".Call", ".Fortran", ".External")
    res <- lapply(symbols, function(x) {
        findSymbolInParsedCode(parsedCode, pkgname, x, "SYMBOL_FUNCTION_CALL",
            TRUE)
    })

    if (!any(res > 0))
        return()
    d <- getLoadedDLLs()
    if (!pkgname %in% names(d))
        return()
    r <- getDLLRegisteredRoutines(pkgname)
    if (sum(lengths(r)) != 0)
        return()
    handleWarning(
        "Register native routines; see ",
        "http://cran.r-project.org/doc/manuals/R-exts.html#Registering-native-routines")
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

checkDeprecatedPackages <- function(pkgdir)
{
    if ("multicore" %in% getAllDependencies(pkgdir))
    {
        handleError("Use 'parallel' instead of 'multicore'. ",
            "'multicore' is deprecated and does not work on Windows.")
    }
}

.checkEnv <- function(env, walker) {
    ## look at all closures in 'env' using codetools-derived 'walker'
    for (n in ls(env, all.names = TRUE)) {
        v <- get(n, envir = env)
        if (typeof(v) == "closure")
            walkCode(body(v), walker)
    }
    walker
}

.colonWalker <- function() {
    ## record all pkg used as pkg::foo or pkg:::bar
    PKGS <- character()
    collector <- function(e, w)
        PKGS <<- append(PKGS, as.character(e[[2]]))
    list(handler=function(v, w) {
        switch(v, "::"=collector, ":::"=collector, NULL)
    }, call=function(e, w) {
        for (ee in as.list(e)) if (!missing(ee)) walkCode(ee, w)
    }, leaf = function(e, w) {
        NULL
    }, done = function() {
        sort(unique(PKGS))
    })
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

## Make sure this is run after pkg is installed.
checkForBadDepends <- function(pkgdir)
{
    pkgname <- strsplit(basename(pkgdir), "_")[[1]][1]
    depends <- cleanupDependency(packageDescription(pkgname)$Depends)
    depends <- append(depends,
        cleanupDependency(packageDescription(pkgname)$Imports))
    output <- getBadDeps(pkgdir)
    if (is.null(output))
        return()

    output <- unique(unlist(strsplit(output, "\n")))
    output <- output[grep("no visible", output)]
    if (length(output) == 0) return()
    res <- regexpr("'[^']*'", output)
    fns <- regexpr("^[^:]*:", output)
    if (all(res == -1L))
        return()

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

getBadDeps <- function(pkgdir)
{
    cmd <- file.path(Sys.getenv("R_HOME"), "bin", "R")
    oldquotes <- getOption("useFancyQuotes")
    on.exit(options(useFancyQuotes=oldquotes))
    options(useFancyQuotes=FALSE)
    args <- sprintf("-q --vanilla --slave -f %s --args %s",
        system.file("script", "checkBadDeps.R", package="BiocCheck"),
        dQuote(pkgdir))
    system2(cmd, args, stdout=TRUE, stderr=FALSE,
        env="R_DEFAULT_PACKAGES=NULL")
}


getFunctionLengths <- function(df)
{
    df <- df[df$terminal & df$parent > -1,]
    rownames(df) <- seq_len(nrow(df))
    max <- nrow(df)
    res <- list()
    funcRows <- df[df$token == "FUNCTION",]
    lst<-lapply(split(df, rownames(df)), as.list)
    if (nrow(funcRows))
    {
        for (i in seq_len(nrow(funcRows)))
        {
            funcRowId <- as.integer(rownames(funcRows)[i])
            funcRow <- funcRows[as.character(funcRowId),]
            funcStartLine <- funcRow$line1 # this might get updated later
            funcLines <- NULL
            funcName <- "_anonymous_"
            # attempt to get function name
            if (funcRowId >= 3)
            {
                up1 <- lst[[as.character(funcRowId -1)]]
                #up1 <- df[as.character(funcRowId - 1),]
                #up2 <- df[as.character(funcRowId - 2),]
                up2 <- lst[[as.character(funcRowId -2)]]
                if (up1$token %in% c("EQ_ASSIGN", "LEFT_ASSIGN") &&
                    up2$token == "SYMBOL")
                {
                    funcName <- up2$text
                    funcStartLine <- up2$line1
                }
            }
            j <- funcRowId + 1
            saveme <- NULL
            while (TRUE)
            {
                #thisRowId <- as.integer(rownames(df)[j])
                thisRowId <- j
                #thisRow <- df[thisRowId,]
                thisRow <- lst[[as.character(thisRowId)]]
                if (thisRowId == max || thisRow$parent > funcRow$parent)
                {
                    lineToExamine <- ifelse(thisRowId == max, max, saveme)

                    endLine <- lst[[as.character(lineToExamine)]]$line2
                    funcLines <- endLine - (funcStartLine -1)
                    if(funcName == "_anonymous_")
                        funcName <- paste0(funcName, ".", funcStartLine)
                    res[[funcName]] <- c(length=funcLines,
                        startLine=funcStartLine, endLine=endLine)
                    break
                } else {
                    if (thisRow$parent > 0)
                    {
                        saveme <- thisRowId
                    }
                }
                j <- j + 1
            }

        }
    }
    res
}

doesFileLoadPackage <- function(df, pkgname)
{
    df <- cbind(df, idx=seq_len(nrow(df)))
    res <- c()
    regex <- paste0("^['|\"]*", pkgname, "['|\"]*$")
    max <- nrow(df)
    reqs <- df[df$token == "SYMBOL_FUNCTION_CALL" &
        df$text %in% c("library","require"),]
    if (nrow(reqs))
    {
        for (i in seq_len(nrow(reqs)))
        {
            reqRow <- reqs[i,]
            currIdx <- reqs[i, "idx"]
            if ((currIdx + 1) >= max) return(res)
            i1 = df[df$idx == currIdx+1,]
            p <- i1$parent
            rowsWithThatParent <- df[df$parent == p,]
            lastRowWithThatParent <-
                rowsWithThatParent[nrow(rowsWithThatParent),]
            rowsToCheck <- df[i1$idx:lastRowWithThatParent$idx,]
            for (j in seq_len(nrow(rowsToCheck)))
            {
                curRow <- rowsToCheck[j,]
                if (curRow$token %in% c("SYMBOL", "STR_CONST") &&
                    grepl(regex, curRow$text))
                {
                    prevRow <- df[curRow$idx -1,]
                    prevPrevRow <- df[curRow$idx -2,]
                    if (!(prevRow$token == "EQ_SUB" &&
                        prevRow$text == "=" &&
                        prevPrevRow$token == "SYMBOL_SUB" &&
                        prevPrevRow$text == "help"))
                    {
                        res <- append(res, reqRow$line1)
                    }
                }
            }
        }
    res
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
        h <- head(df, n=5)
        if (nrow(h))
        {
            handleMessage(
                "The longest function is ", max(h$length) , " lines long")
            handleMessage("The longest ", nrow(h), " functions are:")
            for (i in seq_len(nrow(h)))
            {
                row <- df[i,]
                if (grepl("\\.R$", row$filename, ignore.case=TRUE))
                {
                    handleMessage(sprintf(
                        "%s() (%s, line %s): %s lines", row$functionName,
                        row$filename, row$startLine, row$length))
                } else {
                    handleMessage(sprintf(
                        "%s() (%s): %s lines", row$functionName, row$filename,
                        row$length))
                }
            }
        }
    }
}

## This needs work. Doesn't R CMD check do this anyway?
old.checkExportsAreDocumented <- function(pkgdir, pkgname)
{
    namesAndAliases <- character(0)
    manpages <- dir(file.path(pkgdir, "man"),
        pattern="\\.Rd$", ignore.case=TRUE, full.names=TRUE)
    for (manpage in manpages)
    {
        rd <- parse_Rd(manpage)
        name <-
            unlist(rd[unlist(lapply(rd, function(x)
                attr(x, "Rd_tag") == "\\name"))][[1]][1])
        aliases <- unlist(lapply(rd[unlist(lapply(rd,
            function(x) attr(x, "Rd_tag") == "\\alias"))], "[[", 1))
        namesAndAliases <- append(namesAndAliases, c(name, aliases))
    }
    exports <- getNamespaceExports(pkgname)
    bad <- (!exports %in% unique(namesAndAliases))
    exports[bad]
}

doesManPageHaveRunnableExample <- function(rd)
{
    hasExamples <- any(unlist(lapply(rd,
        function(x) attr(x, "Rd_tag") == "\\examples")))
    if (!hasExamples) return(FALSE)

    ex <- character()
    tc <- textConnection("ex", "w", local=TRUE)
    Rd2ex(rd, commentDonttest = TRUE, out = tc)
    close(tc)

    if(!length(ex))
        return(FALSE)

    parsed <- try(parse(text = ex), silent = TRUE)

    # if code contains only comments the length with be 0
    length(parsed) && !inherits(parsed, "try-error")
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

checkNEWS <- function(pkgdir)
{
    newsloc <- file.path(pkgdir, c("inst", "inst", "."),
            c("NEWS.Rd", "NEWS", "NEWS"))
    news <- head(newsloc[file.exists(newsloc)], 1)
    if (0L == length(news))
    {
        handleNote(
            "Consider adding a NEWS file, so your package news will be ",
            "included in Bioconductor release announcements.")
        return()
    }
    .build_news_db_from_package_NEWS_Rd <-
        get(".build_news_db_from_package_NEWS_Rd", getNamespace("tools"))
    .news_reader_default <-
        get(".news_reader_default", getNamespace("tools"))
    tryCatch({
        suppressWarnings({
            db <- if (grepl("Rd$", news))
                .build_news_db_from_package_NEWS_Rd(news)
            else .news_reader_default(news)
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
    }
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

checkForSupportSiteRegistration <- function(package_dir)
{
    email <- getMaintainerEmail(package_dir)
    if (tolower(email) == "maintainer@bioconductor.org")
    {
        handleMessage("Maintainer email is ok.")
        return()
    }
    url <- paste0("https://support.bioconductor.org/api/email/", email, "/")
    response <- tryCatch(GET(url), error=identity)
    if (inherits(response, "error")) {
        handleMessage(
            "Unable to connect to support site:",
            "\n  ", conditionMessage(response))
    } else if (suppressMessages(content(response))) {
        handleMessage("Maintainer is registered at support site.")
    } else {
        handleError("Maintainer must register at the support site; ",
            "visit https://support.bioconductor.org/accounts/signup/ .")
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

checkIsPackageAlreadyInRepo <- function(pkgName, repo=c("CRAN", "BioCsoft"))
{
    repo <- match.arg(repo)
    repo.url <- sprintf("%s/src/contrib/PACKAGES", BiocManager::repositories()[repo])
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

checkLogicalUseFiles <- function(pkgdir) {
    Rdir <- file.path(pkgdir, "R")
    Rfiles <- dir(pkgdir, recursive=TRUE, pattern = "\\.[rR]$",
                  full.names = TRUE)
    dx <- startsWith(Rfiles, Rdir)
    RdirFiles <- Rfiles[dx]
    Rother <- Rfiles[!dx]
    manFiles <- dir(pkgdir, recursive=TRUE, pattern = "\\.[Rr][Dd]$",
        full.names = TRUE)
    RNWFiles <- dir(pkgdir, recursive=TRUE, pattern = "\\.[Rr][Nn][wW]$",
        full.names = TRUE)
    RMDFiles <- dir(pkgdir, recursive=TRUE, pattern = "\\.[Rr][Mm][Dd]$",
        full.names = TRUE)

    allFiles <- c(Rother, RMDFiles, RNWFiles, manFiles)
    fileNames1 <- character()
    if (length(allFiles) > 0){
        convertedFiles <- unlist(lapply(FUN=makeTempRFile, allFiles))
        vl <- lapply(convertedFiles, findLogicalFile)
        names(vl) <- c(Rother, RMDFiles, RNWFiles, manFiles)
        badFiles <- Filter(length, vl)
        fileNames1 <- names(badFiles)
    }
    sub(fileNames1, pattern=paste0(pkgdir,.Platform$file.sep), replacement="")
}

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
