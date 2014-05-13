

getVigSources <- function(dir)
{
    dir(dir,
        pattern="\\.Rmd$|\\.Rnw$|\\.Rrst$|\\.Rhtml$|\\.Rtex$",
        ignore.case=TRUE, full.names=TRUE)
}

checkVignetteDir <- function(pkgdir, checkingDir)
{
    vigdir <- file.path(pkgdir, "vignettes")
    instdocdir <- file.path(pkgdir, "inst", "doc")
    if (!file.exists(vigdir))
    {
        handleRequired("'vignettes' directory!")
        return()
    }
    vigdircontents <- getVigSources(vigdir)
    if (length(vigdircontents)==0)
    {
        handleRequired("vignette sources in vignettes/ directory.")
        return()
    }
    instdocdircontents <- getVigSources(instdocdir)
    if (length(instdocdircontents) > 0)
    {
        if (checkingDir)
        {
            handleRecommended(paste0(
                "Remove vignette sources from inst/doc;",
                " they belong in vignettes/."))

        } else {
            # Do we really want to emit a NOTE
            # for a situation that will almost always be true?
            # I think not, commenting out for now.
            # Commenting out associated unit test as well.
#            handleConsideration(paste0(
#                "There are vignette sources in inst/doc;",
#                " probably put there by R CMD build."))
        }
    }

    chunks <- 0
    efs <- 0
    for (file in vigdircontents)
    {
        lines <- readLines(file, warn=FALSE)
        chunklines <- lines[grep(">>=|```\\{r|.. \\{r", lines)]
        chunks <- chunks + length(chunklines)

        efs <- efs + 
            length(grep("eval\\s?=\\s?FALSE", chunklines))
    }

    percent <- ifelse(chunks == 0 && efs == 0, 0, (efs/chunks) * (100/1))

    handleMessage(sprintf(
        "# of chunks: %s, # of eval=FALSE: %s (%i%%)",
        chunks, efs,  as.integer(percent)))
    if (percent >= 50)
        handleRecommended("Evaluate more vignette chunks.")
}

checkNewPackageVersionNumber <- function(pkgdir)
{
    dcf <- read.dcf(file.path(pkgdir, "DESCRIPTION"))
    version <- dcf[, "Version"]
        if(!grepl("^0[-.]99[-.][0-9]+$", version))
            handleRequired(sprintf
                ("New package version starting with 0.99; got %s.",version))

}

checkVersionNumber <- function(pkgdir, new_package=FALSE)
{
    dcf <- read.dcf(file.path(pkgdir, "DESCRIPTION"))
    version <- dcf[, "Version"]
    regex <- "^[0-9]+[-\\.]([0-9]+)[-\\.][0-9]+$"
    if(!grepl(regex, version))
    {
        handleRequired(paste0("Valid package Version, see",
            " http://www.bioconductor.org/developers/how-to/version-numbering/"))
        return()
    }
    tryCatch({
        pv <- package_version(version)
        y <- pv$minor
        mod <- y %% 2
        biocY <- packageVersion("BiocInstaller")$minor
        bioc.mod <- biocY %% 2
        isDevel <- (bioc.mod == 1)
        if (mod != bioc.mod)
        {
            shouldBe <- ifelse(isDevel, "odd", "even")
            vers <- ifelse(isDevel, "devel", "release")
            handleRecommended(sprintf("y of x.y.z version should be %s in %s",
                    shouldBe, vers))
        }

        },
        error=function(e) handleRequired("Valid package version"))
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
    if (!all(views %in% nodes(biocViewsVocab)))
    {
        badViews <- paste(views[!(views %in% nodes(biocViewsVocab))],
            collapse=", ")
        views <- views[!views %in% badViews]
        if (length(views) == 0) return(NA)
    }
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
    for (level in topLevel) {
        if (view %in% names(acc(biocViewsVocab, level)[[level]]))
            return(level)
    }
}


checkBiocViews <- function(pkgdir)
{
    dcf <- read.dcf(file.path(pkgdir, "DESCRIPTION"))
    if (!"biocViews" %in% colnames(dcf))
    {
        handleRecommended("Add some biocViews!")
        return()
    } else {
        biocViews <- dcf[, "biocViews"]
        views <- strsplit(gsub("\\s*,\\s*", ",", biocViews), ",")[[1]]
        biocViewsVocab <- NULL ## to keep R CMD check happy
        data("biocViewsVocab", package="biocViews", envir=environment())
        if (!all(views %in% nodes(biocViewsVocab)))
        {
            badViews <- paste(views[!(views %in% nodes(biocViewsVocab))],
                collapse=", ")
            handleRecommended(paste("Use valid biocViews. Invalid ones:",
                badViews))
        }
    }

    parents <- c()
    for (view in views)
    {
        parents <- c(parents, getParent(view, biocViewsVocab))
    }
    if (length(unique(parents)) > 1)
    {
        handleRecommended(paste0("Use biocViews from one category only",
            " (one of Software, ExperimentData, AnnotationData)"))
    }
}

checkBBScompatibility <- function(pkgdir)
{
    lines <- readLines(file.path(pkgdir, "DESCRIPTION"), warn=FALSE)
    if (any(nchar(lines)==0))
    {
        handleRequired("Remove blank lines from DESCRIPTION!")
        return()
    }
    dcf <- read.dcf(file.path(pkgdir, "DESCRIPTION"))
    if (any(grepl("\\s", colnames(dcf))))
    {
        handleRequired("Remove whitespace from DESCRIPTION field names.")
        return()
    }
    segs <- strsplit(pkgdir, .Platform$file.sep)[[1]]
    pkgNameFromDir <- segs[length(segs)]
    if (dcf[, "Package"] != pkgNameFromDir)
    {
        handleRequired(sprintf(
            "Package dir %s must match Package: field ( got %s)!",
            pkgNameFromDir, dcf[, "Package"]))
            return()
    }
    if (!"Version" %in% colnames(dcf))
    {
        handleRequired("Version field in DESCRIPTION!")
        return()
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
                handleRequired("AuthorsR@ field must be valid R code!")
            })
        if (!exists("people")) return()
        if (!"person" %in% class(people))
        {
            handleRequired("Authors@R must evaluate to 'person' object!")
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
            handleRequired("One author with maintainer (cre) role.")
            return()
        }
    } else if ("Maintainer" %in% colnames(dcf)) {
        maintainer <- dcf[,"Maintainer"]
    } else {
        handleRequired("Maintainer or Authors@R field in DESCRIPTION file!")
        return()
    }
    # now need to make sure that regexes work, a la python/BBS 
    regex = '(.*\\S)\\s*<(.*)>\\s*'
    match <- regexec(regex, maintainer)[[1]]
    match.length <- attr(match, "match.length")
    #if (!  (all(match)  > 0) && (all(match.length) > 0) )
    if (match == -1 && match.length == -1)
    {
        handleRequired("Email address in Maintainer field.")
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
    if (!(dir.exists(tests_dir) && length(dir(tests_dir, pattern = "\\.(R|Rin)$"))))
    ## end stolen code
    {
        msg <- paste0("Adding unit tests.\n",
            "  We strongly recommend them. See\n",
            "  http://www.bioconductor.org/developers/how-to/unitTesting-guidelines/."
            )
        handleConsideration(msg)
    }
}

checkRegistrationOfEntryPoints <- function(pkgname, parsedCode)
{
    symbols <-  c(".C", ".Call", ".Fortran", ".External")
    res <- lapply(symbols, function(x) {
        findSymbolInParsedCode(parsedCode, pkgname, x, "SYMBOL_FUNCTION_CALL",
            TRUE)
    })

    if (any(res > 0)) 
    {
        d <- getLoadedDLLs()
        if (pkgname %in% names(d))
        {
            r <- getDLLRegisteredRoutines(pkgname)
            if (sum(sapply(r, length)) == 0)
            {
                handleRecommended(
                    paste0("Register native routines!",
                        " see http://cran.r-project.org/doc/manuals/R-exts.html#Registering-native-routines"))
            }
        }
    }
}

checkImportSuggestions <- function(pkgname)
{
    suggestions <- NULL
    tryCatch({
        suppressMessages({
            suppressWarnings({
                suggestions <- 
                    capture.output(codetoolsBioC::writeNamespaceImports(pkgname))
            })
        })
    },
        error=function(e){
            suggestions <- "ERROR"
            handleMessage("Could not get namespace suggestions.")
    })


    if(length(suggestions) && (!is.null(suggestions)) &&
        (suggestions != "ERROR"))
    {
            handleMessage("Namespace import suggestions are:")
            message(paste(suggestions, collapse="\n"))
            handleMessage("--END of namespace import suggestions.")
    }

    if ((!is.null(suggestions)) && (!length(suggestions)))
    {
        message("  No suggestions.")
    }

    suggestions
}

checkDeprecatedPackages <- function(pkgdir)
{
    if ("multicore" %in% getAllDependencies(pkgdir))
    {
        handleRequired(paste("Use 'parallel' instead of 'multicore'.",
            " 'multicore' is deprecated and does not work on Windows."))
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
        handleRecommended(sprintf(
            "Import %s in NAMESPACE as well as DESCRIPTION.",
            paste(badones, collapse=", ")))
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
            handleRecommended(sprintf(
                "Import %s in DESCRIPTION as well as NAMESPACE.",
                paste(badones, collapse=", ")))
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
    if (!is.null(output))
    {
        output <- unique(unlist(strsplit(output, "\n")))
        output <- output[grep("no visible", output)]
        if (length(output) == 0) return()
        res <- regexpr("'[^']*'$", output)
        fns <- regexpr("^[^:]*:", output)
        fmatch.length <- attr(fns, "match.length")
        if (any(res != -1))
        {
            res <- substr(output, res, nchar(output))
            fns <- unique(substr(output, fns, fmatch.length-1))
            res <- gsub("'", "", fixed=TRUE, res)
            badFunctions <- paste(fns, collapse=", ")
            badObjects <- paste(res, collapse=", ")

            errObjects <- c()
            errFunctions <- c()
            errPkgs <- c()
            noteObjects <- c()
            noteFunctions <- c()

            for (i in 1:length(fns))
            {
                found <- FALSE
                sym <- res[i]
                func <- fns[i]
                if (length(depends) >  0)
                {
                    for (j in 1:length(depends))
                    {
                        dep <- depends[j]
                        if (sym %in% getNamespaceExports(dep))
                        {
                            errObjects <- append(errObjects, sym)
                            errFunctions <- append(errFunctions, func)
                            errPkgs <- append(errPkgs, dep)
                            found <- TRUE
                        }
                    }
                }
                if (!found)
                {
                    noteObjects <- append(noteObjects, sym)
                    noteFunctions <- append(noteFunctions, func)
                }
            }


            if (length(errObjects) > 0)
            {
                msg <- sprintf(paste(
                    ## FIXME show the actual package names?
                    "Packages (%s) which provide %s", 
                    "(used in %s)",
                    "should be imported in the NAMESPACE file,",
                    "otherwise packages that import %s could fail."),
                    paste(errPkgs, collapse=", " ),
                    paste(errObjects, collapse=", "), 
                    paste(errFunctions, collapse=", "), pkgname)
                handleRequired(msg)
            }

            if (length(noteObjects) > 0)
            {
                if (length(noteObjects) == 1)
                {
                    grammar <- list(object_objects="object", was_were="was",
                        itis_theyare="it is")
                } else {
                    grammar <- list(object_objects="objects", was_were="were",
                        itis_theyare="they are")
                }

                msg <- sprintf(paste(
                    "Clarifying how %s %s (used in %s)",
                    "%s initialized. Maybe %s part of a",
                    "data set loaded with data(), or perhaps part of",
                    "an object referenced in with() or within()."),
                    grammar$object_objects,
                    paste(noteObjects, collapse=", "),
                    paste(noteFunctions, collapse=", "),
                    grammar$was_were,
                    grammar$itis_theyare)

                handleConsideration(msg)
            }

        }
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
    system2(cmd, args, stdout=TRUE, stderr=FALSE)
}


getFunctionLengths <- function(df)
{
    df <- df[df$terminal & df$parent > -1,]
    rownames(df) <- seq_along(1:nrow(df))
    max <- nrow(df)
    res <- list()
    funcRows <- df[df$token == "FUNCTION",]
    lst<-lapply(split(df, rownames(df)), as.list)
    if (nrow(funcRows))
    {
        for (i in 1:nrow(funcRows))
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
                    #endLine <- df[rownames(df) == as.character(lineToExamine), "line2"]
                    endLine <- lst[[as.character(lineToExamine)]]$line2
                    funcLines <- endLine - (funcStartLine -1)
                    if(funcName == "_anonymous_") funcName <- paste0(funcName, ".",
                        funcStartLine)
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
    df <- cbind(df, idx=seq_along(1:nrow(df)))
    res <- c()
    regex <- paste0("^['|\"]*", pkgname, "['|\"]*$")
    max <- nrow(df)
    reqs <- df[df$token == "SYMBOL_FUNCTION_CALL" &
        df$text %in% c("library","require"),]
    if (nrow(reqs))
    {
        for (i in 1:nrow(reqs))
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
            for (j in 1:nrow(rowsToCheck))
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
                msg <- sprintf("Don't call library or require on %s in file %s",
                    pkgname, mungeName(filename, pkgname))
                if (grepl("\\.R$", filename, ignore.case=TRUE))
                {
                    msg <- sprintf("%s, line %s", msg, 
                        paste(res, collapse=","))
                }
                msg <- sprintf("%s; this is not necessary.", msg)
                handleRecommended(msg)
            }
        }
    }
}

checkFunctionLengths <- function(parsedCode, pkgname)
{
    df <- data.frame(stringsAsFactors=FALSE)
    i <- 1
    for (filename in names(parsedCode))
    {
        #.debug("filename is %s", filename)
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
            handleMessage(sprintf(
                "  The longest function is %s lines long", max(h$length)))
            handleMessage(sprintf("  The longest %s functions are:", nrow(h)))
            for (i in 1:nrow(h))
            {
                row <- df[i,]
                if (grepl("\\.R$", row$filename, ignore.case=TRUE))
                {
                    handleMessage(sprintf("    %s() (%s, line %s): %s lines",
                        row$functionName, row$filename, row$startLine, row$length))
                } else {
                    handleMessage(sprintf("    %s() (%s): %s lines",
                        row$functionName, row$filename, row$length))
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
    ex <- capture.output(Rd2ex(rd))
    ex <- grep("^\\s*$", ex, invert=TRUE, value=TRUE)
    ex <- grep("^\\s*#", value=TRUE, ex, invert=TRUE)
    ex <- ex[nchar(ex) > 0]
    as.logical(length(ex))
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

    if (exportingPagesCount > 0 
        && (noExamplesCount / exportingPagesCount) >= (0.8 / 1.0))
    {
        handleRequired(paste0("At least 80% of man pages documenting ",
            "exported objects must have runnable examples.",
            "The following pages do not:"))
    } else {
        if (length(badManPages) > 0)
        handleConsideration(paste0("Adding runnable examples to the following ",
            "man pages which document exported objects:"))
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
        handleConsideration(paste0("Adding a NEWS file, so your ",
            "package news will be included",
            " in Bioconductor release announcements."))
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
        handleRecommended(sprintf(paste0("Fix formatting of %s!",
            " Malformed package NEWS will not be included ",
            "in Bioconductor release announcements."), basename(news)))
    })
}

checkFormatting <- function(pkgdir)
{
    files <- c(file.path(pkgdir, c("DESCRIPTION",
        "NAMESPACE")),
        dir(file.path(pkgdir, "man"), pattern="\\.Rd$", ignore.case=TRUE,
        full.names=TRUE),
        dir(file.path(pkgdir, "vignettes"), full.names=TRUE,
            pattern="\\.Rnw$|\\.Rmd$|\\.Rrst$|\\.Rhtml$|\\.Rtex$",
            ignore.case=TRUE),
        dir(file.path(pkgdir, "R"), pattern="\\.R$", ignore.case=TRUE,
            full.names=TRUE)
        )
    longlines <- 0L
    totallines <- 0L
    tablines <- 0L
    badindentlines <- 0L
    ok <- TRUE

    for (file in files)
    {

        if (file.exists(file) && file.info(file)$size == 0)
        {
            pkgname <- getPkgNameFromPkgDir(pkgdir)
            handleConsideration(sprintf("Adding content to the empty file %s.",
                mungeName(file, pkgname)))
        }

        if (file.exists(file) && file.info(file)$size > 0)
        {
            lines <- readLines(file, warn=FALSE)
            totallines <- totallines + length(lines)
            n <- nchar(lines)

            names(n) <- seq_along(1:length(n))
            long <- n[n > 80]
            if (length(long))
            {
                ## TODO/FIXME We could tell the user here which lines are long
                ## in which files. 
                longlines <- longlines + length(long)
            }

            tabs <- grepl("\t", lines)
            if (any(tabs))
            {
                tablines <- tablines + length(which(tabs))
            }

            res <- regexpr("^([ ]+)", lines)
            if (any(res > -1))
            {
                match.length <- attr(res, "match.length")
                indents <- match.length[match.length > -1]
                badindentlinesinthisfile <- length(which(indents %% 4 != 0))
                badindentlines <- badindentlines + badindentlinesinthisfile
            }

        }
    }
    if (longlines > 0)
    {
        ok <- FALSE
        handleConsideration(sprintf(
            "Shortening lines; %s lines (%i%%) are > 80 characters long.",
            longlines, as.integer((longlines/totallines) * (100/1) )))
    }
    if (tablines > 0)
    {
        ok <- FALSE
        handleConsideration(sprintf(
            "Replacing tabs with 4 spaces; %s lines (%i%%) contain tabs.",
            tablines, as.integer((tablines/totallines) * (100/1) )))
    }
    if (badindentlines > 0)
    {
        ok <- FALSE
        handleConsideration(sprintf(paste0(
            "Indenting lines with a multiple of 4 spaces;",
            " %s lines (%i%%) are not."),
            badindentlines,
            as.integer((badindentlines/totallines) * (100/1) )))

    }

    if (!ok)
    {
        message("  See http://bioconductor.org/developers/how-to/coding-style/")
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
        if (any(grepl("^%%  ~~", lines)))
            bad <- append(bad, basename(manpage))
    }
    if (length(bad) > 0)
    {
        handleConsideration(sprintf(
            "Removing generated comments from man pages %s ",
            paste(bad, collapse=", ")))
    }
}

checkForBiocDevelSubscription <- function(pkgdir)
{
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
    if (!exists("email"))
        return()
    if (tolower(email) == "maintainer@bioconductor.org")
    {
        handleMessage("Maintainer email is ok.")
        return()
    }
    y <- POST("https://stat.ethz.ch/mailman/admin/bioc-devel",
        body=list(adminpw=Sys.getenv("BIOC_DEVEL_PASSWORD")))
    l <- content(y, as="text")
    if(grepl("Authorization\\s+failed\\.", l))
    {
        # couldn't log in...
        return()
    }
    z <- POST("https://stat.ethz.ch/mailman/admin/bioc-devel/members?letter=4",
        body=list(findmember=email))
    l <- content(z, as="text")
    if(grepl(paste0(">", tolower(email), "<"), tolower(l), fixed=TRUE))
    {
        handleMessage("Maintainer is subscribed to bioc-devel!")
    } else {
        handleRecommended(paste0(
            "Maintainer should subscribe to the bioc-devel mailing list.",
            " Subscribe here: ",
            "https://stat.ethz.ch/mailman/listinfo/bioc-devel"))
    }
}