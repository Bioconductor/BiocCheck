.printf <- function(...) cat(noquote(sprintf(...)), "\n")

.debug <- function(...) if (getOption("Bioconductor.DEBUG", FALSE))
    .printf(...)

.msg <- function(..., appendLF=TRUE, indent=0, exdent=2)
{
    txt <- sprintf(...)
    message(paste(strwrap(txt, indent=indent, exdent=exdent), collapse="\n"),
        appendLF=appendLF)
}

.stop <- function(...) stop(noquote(sprintf(...)), call.=FALSE)

.verbatim <-
    function(..., appendLF=TRUE, indent=6, exdent=8, width=getOption("width"))
{
    ## don't wrap elements of msg; indent first line by 'indent',
    ## subsequent lines by 'exdent'
    txt <- sprintf(...)
    if (length(txt)) {
        prefix <- paste(rep(" ", indent), collapse="")
        txt[1] <- paste0(prefix, txt[1])
    }
    if (length(txt) > 1L) {
        prefix <- paste(rep(" ", exdent), collapse="")
        txt[-1] <- paste0(prefix, txt[-1])
    }
    txt <- ifelse(
        (!is.na(txt)) & (nchar(txt) > width),
        sprintf("%s...", substr(txt, 1, width - 3)),
        txt)
    message(paste(txt, collapse="\n"), appendLF=appendLF)
}

handleCheck <- function(..., appendLF=TRUE)
{
    msg <- paste0(...)
    .msg("* %s", msg, appendLF=appendLF)
}

handleError <- function(...)
{
    msg <- .error$add(...)
    .msg("* ERROR: %s", msg, indent=4, exdent=6)
}

handleWarning <- function(...)
{
    msg <- .warning$add(...)
    .msg("* WARNING: %s", msg, indent=4, exdent=6)
}

handleNote <- function(...)
{
    msg <- .note$add(...)
    .msg("* NOTE: %s", msg, indent=4, exdent=6)
}

handleMessage <- function(..., indent=4, exdent=6)
{
    msg <- paste0(...)
    .msg("  %s", msg, indent=indent, exdent=exdent)
}

handleVerbatim <- function(msg, indent=4, exdent=6, width=getOption("width"))
{
    .verbatim("%s", msg, indent=indent, exdent=exdent, width=width)
}

installAndLoad <- function(pkg)
{
    r_libs_user_old <- Sys.getenv("R_LIBS_USER")
    on.exit(do.call("Sys.setenv", list(R_LIBS_USER=r_libs_user_old)))
    r_libs_user <- paste(.libPaths(), collapse=.Platform$path.sep)
    Sys.setenv(R_LIBS_USER=r_libs_user)

    dir.create(install_dir <- tempfile())
    dir.create(libdir <- file.path(install_dir, "lib"))
    file.create(stderr <- file.path(install_dir, "install.stderr"))
    cmd <- file.path(Sys.getenv("R_HOME"), "bin", "R")
    args <- sprintf("--vanilla CMD INSTALL --no-test-load --library=%s %s",
                    libdir, shQuote(pkg))
    res <- system2(cmd, args, stdout=NULL, stderr=stderr)
    if (res != 0)
    {
        cat("  cmd: ", cmd,
            "\n  args: ", args,
            "\n  stderr:",
            "\n  ", paste(readLines(stderr), collapse="\n  "),
            "\n", sep="")
        handleError(pkg, " must be installable.")
    }
    pkgname <- strsplit(basename(pkg), "_")[[1]][1]
    args <- list(package=pkgname, lib.loc=libdir)
    if (paste0("package:",pkgname) %in% search())
        suppressWarnings(unloadNamespace(pkgname))

    suppressPackageStartupMessages(do.call(library, args))
    install_dir
}

# Takes as input the value of an Imports, Depends,
# or LinkingTo field and returns a named character
# vector of Bioconductor dependencies, where the names
# are version specifiers or blank.
cleanupDependency <- function(input, remove.R=TRUE)
{
    if (is.null(input)) return(character(0))
    if (!nchar(input)) return(character(0))
    output <- gsub("\\s", "", input)
    raw_nms <- output
    nms <- strsplit(raw_nms, ",")[[1]]
    namevec <- vector(mode = "character", length(nms))
    output <- gsub("\\([^)]*\\)", "", output)
    res <- strsplit(output, ",")[[1]]
    for (i in seq_along(nms))
    {
        if(grepl(">=", nms[i], fixed=TRUE))
        {
            tmp <- gsub(".*>=", "", nms[i])
            tmp <- gsub(")", "", tmp, fixed=TRUE)
            namevec[i] <- tmp
        } else {
            namevec[i] = ''
        }
    }
    names(res) <- namevec
    if (remove.R)
        res <- res[which(res != "R")]
    res
}

getAllDependencies <- function(pkgdir)
{
    dcf <- read.dcf(file.path(pkgdir, "DESCRIPTION"))
    fields <- c("Depends", "Imports", "Suggests", "Enhances", "LinkingTo")
    out <- c()
    for (field in fields)
    {
        if (field %in% colnames(dcf))
            out <- append(out, cleanupDependency(dcf[, field]))
    }
    out
}

getAllDeprecatedPkgs <- function()
{
    # our best guess at deprecated packages
    # if write_views wasn't updated to manual add missing package info
    # a more complete would be to scrap the build report
    # html = htmlParse("http://bioconductor.org/checkResults/devel/bioc-LATEST/")
    # depdevel = xpathSApply(html, "//s", xmlValue)

    con <- url("https://bioconductor.org/packages/release/bioc/VIEWS")
    views_release <- read.dcf(con, all=TRUE)
    close(con)
    con <- url("https://bioconductor.org/packages/devel/bioc/VIEWS")
    views_devel <- read.dcf(con, all=TRUE)
    close(con)
    pkgs <- unique(c(
        views_release[["Package"]][which(views_release[["PackageStatus"]]
                                         == "Deprecated")],
        views_devel[["Package"]][which(views_devel[["PackageStatus"]]
                                       == "Deprecated")]
        ))
    pkgs
}

parseFile <- function(infile, pkgdir)
{
    ## FIXME - use purl to parse RMD and RRST
    ## regardless of VignetteBuilder value
    dir.create(parse_dir <- tempfile())
    if (grepl("\\.Rnw$|\\.Rmd|\\.Rrst|\\.Rhtml$|\\.Rtex", infile, TRUE))
    {
        outfile <- NULL
        desc <- file.path(pkgdir, "DESCRIPTION")
        dcf <- read.dcf(desc)
        if ("VignetteBuilder" %in% colnames(dcf) &&
            dcf[,"VignetteBuilder"] == "knitr")
        {
            if (!requireNamespace("knitr")) {
                stop("'knitr' package required to check knitr-based vignettes")
            }
            outfile <- file.path(parse_dir, "parseFile.tmp")
            # copy file to work around https://github.com/yihui/knitr/issues/970
            # which is actually fixed but not in CRAN yet (3/16/15)
            tmpin <- file.path(parse_dir, basename(infile))
            file.copy(infile, tmpin)
            suppressWarnings(suppressMessages(capture.output({
                knitr::purl(input=tmpin, output=outfile, documentation=0L)
            })))
            file.remove(tmpin)
        } else {
            full.infile <- normalizePath(infile)
            oof <- file.path(parse_dir, basename(infile))
            oof <- vapply(strsplit(oof, "\\."),
                function(x) paste(x[seq_len(length(x)-1)], collapse="."),
                character(1))
            outfile <- paste0(oof, '.R')
            suppressWarnings(suppressMessages(capture.output({
                    oldwd <- getwd()
                    on.exit(setwd(oldwd))
                    setwd(parse_dir)
                    Stangle(full.infile)
                    badname <- paste0(basename(infile), ".R")
                    if (file.exists(badname))
                        file.rename(badname, outfile)
            })))
        }

    } else if (grepl("\\.Rd$", infile, TRUE))
    {
        rd <- parse_Rd(infile)
        outfile <- file.path(parse_dir, "parseFile.tmp")
        code <- capture.output(Rd2ex(rd))
        cat(code, file=outfile, sep="\n")
    } else if (grepl("\\.R$", infile, TRUE)) {
        outfile <- infile
    }
    p <- parse(outfile, keep.source=TRUE)
    getParseData(p)
}

parseFiles <- function(pkgdir)
{
    parsedCode <- list()
    dir1 <- dir(file.path(pkgdir, "R"), pattern="\\.R$", ignore.case=TRUE,
        full.names=TRUE)
    dir2 <- dir(file.path(pkgdir, "man"), pattern="\\.Rd$", ignore.case=TRUE,
        full.names=TRUE)
    dir3 <- dir(file.path(pkgdir, "vignettes"),
        pattern="\\.Rnw$|\\.Rmd$|\\.Rrst$|\\.Rhtml$|\\.Rtex$",
        ignore.case=TRUE, full.names=TRUE)
    files <- c(dir1, dir2, dir3)
    for (file in files)
    {
        df <- parseFile(file, pkgdir)
        if (nrow(df))
            parsedCode[[file]] <- df
    }
    parsedCode
}

findSymbolInParsedCode <- function(parsedCode, pkgname, symbolName,
    token, silent=FALSE)
{
    matches <- list()
    for (filename in names(parsedCode))
    {
        df <- parsedCode[[filename]]
        matchedrows <- df[which(df$token == token & df$text == symbolName),]
        if (nrow(matchedrows) > 0)
        {
            matches[[filename]] <- matchedrows[, c(1,2)]
        }
    }
    if (token == "SYMBOL_FUNCTION_CALL")
        parens="()"
    else
        parens=""
    for (name in names(matches))
    {
        x <- matches[[name]]
        for (i in nrow(x))
        {
            if (!silent)
            {
                if (grepl("\\.R$", name, ignore.case=TRUE))
                    handleMessage(sprintf(
                        "Found %s%s in %s (line %s, column %s)", symbolName,
                        parens, mungeName(name, pkgname), x[i,1], x[i,2]))
                else
                    handleMessage(sprintf(
                        "Found %s%s in %s", symbolName, parens,
                        mungeName(name, pkgname))) # FIXME test this
            }
        }
    }
    length(matches) # for tests
}

mungeName <- function(name, pkgname)
{
    twoseps <- paste0(rep.int(.Platform$file.sep, 2), collapse="")
    name <- gsub(twoseps, .Platform$file.sep, name, fixed=TRUE)
    pos <- regexpr(pkgname, name)
    substr(name, pos+1+nchar(pkgname), nchar(name))
}

isInfrastructurePackage <- function(pkgDir)
{
    if (!file.exists(file.path(pkgDir, "DESCRIPTION")))
        return(FALSE)
    dcf <- read.dcf(file.path(pkgDir, "DESCRIPTION"))
    if (!"biocViews" %in% colnames(dcf))
    {
        return(FALSE)
    }
    biocViews <- dcf[, "biocViews"]
    views <- strsplit(gsub("\\s", "", biocViews), ",")[[1]]
    "Infrastructure" %in% views
}

getMaintainerEmail <- function(pkgdir)
{
    # Eventually update this to just look at Authors@R
    # Since the intention is to possible start running
    # this on the daily builder, leave Maintainer field
    # check. This is used to check for mailing list registration

    dcf <- read.dcf(file.path(pkgdir, "DESCRIPTION"))
    if ("Maintainer" %in% colnames(dcf))
    {
        m <- dcf[, "Maintainer"]
        ret <- regexec("<([^>]*)>", m)[[1]]
        ml <- attr(ret, "match.length")
        email <- substr(m, ret[2], ret[2]+ml[2]-1)
    } else if ("Authors@R" %in% colnames(dcf)) {
        ar <- dcf[, "Authors@R"]
        env <- new.env(parent=emptyenv())
        env[["c"]] = c
        env[["person"]] <- utils::person
        pp <- parse(text=ar, keep.source=TRUE)
        tryCatch(people <- eval(pp, env),
            error=function(e) {
                # could not parse Authors@R
                return()
            })
        for (person in people)
        {
            if ("cre" %in% person$role)
            {
                email <- person$email
            }
        }
    }
    return(email)
}

docType <- function(rd) {
    tags <- tools:::RdTags(rd)
    if (any(tags == "\\docType"))
        as.character(rd[tags == "\\docType"][[1L]])
}


findLogicalFile <- function(fl) {
    env <- new.env()
    tryCatch(source(fl, local = env),
             error = function(err){
                 return(character())
             })
    objs = ls(env, all.names=TRUE)
    for (obj in objs){
      if (!is.function(env[[obj]])){
           rm(list = obj, envir = env)
      }
    }
    globals <- eapply(env, safeFindGlobals)
    if (length(globals) != 0) {
        names(which(unlist(lapply(globals, function(x) {
            any(c("T","F") %in% x)
        }))))
    } else {
      character()
    }
}

safeFindGlobals <- function(env, ...) {
    tryCatch({
        findGlobals(env, ...)
    }, error = warning)
}

findLogicalRdir <- function(pkgname, symbol){

    env <- getNamespace(pkgname)
    objs <- ls(env, all.names=TRUE)
    objs <- objs[grep("^.__[CTM]__", objs, invert=TRUE)]
    globals <- lapply(objs, function(obj) {
        value = env[[obj]];
        if (identical(typeof(value), "closure")) {
            findGlobals(value)
        } else character(0)
    })
    names(globals) <- objs
    if (length(globals) != 0) {
        found <- vapply(
            globals, function(x, symbol) any(symbol %in% x), logical(1), symbol
        )
        funName <- names(globals)[found]
        if (length(funName) > 0) {
            paste0(funName, "()")
        } else character()
    } else {
      character()
    }
}

makeTempRFile <- function(infile){
    ext <- tolower(tools::file_ext(infile))
    dir.create(tempr_dir <- tempfile())
    outfile <- file.path(tempr_dir, paste0(basename(infile), ".R"))
    if(ext == 'rnw' & isTRUE(vigHelper(infile, "knitr")[1])){
        ext <- 'rmd'
    }
    switch(ext,
           r = {
               code <- readLines(infile)
               validFile <- TRUE
           },
           rd = {
               tempfile <- tools::parse_Rd(infile)
               code <- capture.output(tools::Rd2ex(tempfile))
               validFile <- TRUE
           },
           rnw = {
               Stangle(infile, output=outfile, quiet=TRUE)
               code <- readLines(outfile)
               validFile <- TRUE
           },
           rmd = {
               knitr::purl(input=infile, output=outfile, quiet=TRUE)
               code <- readLines(outfile)
               validFile = TRUE
           },
           {
               validFile = FALSE
               code <- NA
           })

    if (validFile){
        cat("dummyTest <- function(){", file=outfile, sep="\n")
        cat(code, file=outfile, sep="\n", append =TRUE)
        cat("}", file=outfile, sep="\n", append =TRUE)
        outfile
    } else {
        character(0)
    }
}

grepPkgDir <- function(pkgdir, greparg, full_path=FALSE){
    args <- sprintf("%s %s*", greparg, pkgdir)
    fnd <- tryCatch(
        system2("grep", args, stdout=TRUE),
        warning=function(w){character()},
        error=function(e){character(0)})
    msg_files <- vapply(fnd,
                        FUN=function(x, pkgdir){
                            vl = strsplit(x, split=":")
                            filename =
                                if(full_path){
                                    vl[[1]][1]
                                } else {
                                    sub(vl[[1]][1], pattern=pkgdir,
                                        replacement="", fixed=TRUE)
                                }
                            lineNum = vl[[1]][2]
                            if (tolower(.Platform$OS.type) == "windows"){
                                filename =
                                    if(full_path){
                                        paste(vl[[1]][1], vl[[1]][2], sep=":")
                                    }else {
                                        sub(
                                            paste(vl[[1]][1], vl[[1]][2], sep=":"),
                                            pattern=pkgdir, replacement="",
                                            fixed=TRUE)
                                    }
                                lineNum = vl[[1]][3]
                            }
                            sprintf("%s (line %s)", filename, lineNum)},
                        FUN.VALUE = character(1),
                        c(pkgdir=pkgdir),
                        USE.NAMES=FALSE)
    msg_files
}

getVigSources <- function(dir)
{
    dir(dir,
        pattern="\\.Rmd$|\\.Rnw$|\\.Rrst$|\\.Rhtml$|\\.Rtex$",
        ignore.case=TRUE, full.names=TRUE)
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

getVigEngine <- function(vignetteFile){
    lines <- readLines(vignetteFile, n=100L, warn=FALSE)
    idx <- grep(lines, pattern="VignetteEngine")
    if (length(idx) != 0){
        eng <- gsub("::.*", "", gsub(".*\\{|\\}.*", "", lines[idx]))
        return(eng)
    } else {
        return(NA)
    }
}

vigHelper <- function(vignetteFile, builder){
    eng <- getVigEngine(vignetteFile)
    if (all(is.na(eng))){
        return(NA)
    } else {
        return(all(eng %in% builder))
    }
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
    topLevel <- c("Software", "ExperimentData", "AnnotationData", "Workflow")
    if (view %in% topLevel)
        return(view)
    for (level in topLevel) {
        if (view %in% names(acc(biocViewsVocab, level)[[level]]))
            return(level)
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

doesManPageHaveRunnableExample <- function(rd)
{
    hasExamples <- any(unlist(lapply(rd,
        function(x) attr(x, "Rd_tag") == "\\examples")))
    if (!hasExamples) return(FALSE)

    ex <- character()
    tc <- textConnection("ex", "w", local=TRUE)
    Rd2ex(rd, commentDontrun = TRUE, commentDonttest = TRUE, out = tc)
    close(tc)

    if(!length(ex))
        return(FALSE)

    parsed <- try(parse(text = ex), silent = TRUE)

    # if code contains only comments the length with be 0
    length(parsed) && !inherits(parsed, "try-error")
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
    sub(fileNames1, pattern=paste0(pkgdir,.Platform$file.sep),
        replacement="", fixed=TRUE)
}
