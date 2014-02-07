

getVigSources <- function(dir)
{
    dir(dir,
        pattern="\\.Rmd$|\\.Rnw$|\\.Rrst$",
        ignore.case=TRUE, full.names=TRUE)
}

checkVignetteDir <- function(pkgdir)
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
        handleRecommended(paste0(
            "Remove vignette sources from inst/doc/;",
            " they belong in vignettes/."))
    }

    chunks <- 0
    efs <- 0
    for (file in vigdircontents)
    {
        chunks <- chunks +
            length(grep(">>=|```\\{r|.. \\{r", readLines(file,
            warn=FALSE)))
        efs <- efs + 
            length(grep("eval\\s?=\\s?FALSE", readLines(file,
            warn=FALSE)))
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
        handleRequired(paste0("Valid package Version, see\n",
            "  http://www.bioconductor.org/developers/how-to/version-numbering/"))
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

checkBiocViews <- function(pkgdir)
{
    dcf <- read.dcf(file.path(pkgdir, "DESCRIPTION"))
    if (!"biocViews" %in% colnames(dcf))
    {
        handleRecommended("Add some biocViews!")
        return()
    } else {
        biocViews <- dcf[, "biocViews"]
        views <- strsplit(gsub("\\s", "", biocViews), ",")[[1]]
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

    getParent <- function(view, biocViewsVocab)
    {
        topLevel <- c("Software", "ExperimentData", "AnnotationData")
        for (level in topLevel) {
            if (view %in% names(acc(biocViewsVocab, level)[[level]]))
                return(level)
        }
    }
    parents <- c()
    for (view in views)
    {
        parents <- c(parents, getParent(view, biocViewsVocab))
    }
    if (length(unique(parents)) > 1)
    {
        handleRecommended(paste0("Use biocViews from one category only\n",
            "  (one of Software, ExperimentData, AnnotationData)"))
    }
}

checkBBScompatibility <- function(pkgdir)
{
    dcf <- read.dcf(file.path(pkgdir, "DESCRIPTION"))
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
        env[["person"]] <- person
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
        msg <- paste0("Looks like this package contains no unit tests.\n",
            "  We strongly recommend them. See\n",
            "  http://www.bioconductor.org/developers/how-to/unitTesting-guidelines/."
            )
        handleNote(msg)
    }
}

checkRegistrationOfEntryPoints <- function(pkgname)
{
    d <- getLoadedDLLs()
    if (pkgname %in% names(d))
    {
        r <- getDLLRegisteredRoutines(pkgname)
        # FIXME What's a better way to determine that there's nothing in r?
        x <- capture.output(r)
        if (length(x) == 1)
        {
            handleRecommended(
                paste0("Register native routines!\n",
                    "  see http://cran.r-project.org/doc/manuals/R-exts.html#Registering-native-routines"))
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
        handleRequired(paste("Use 'parallel' instead of 'multicore'.\n",
            "  'multicore' is deprecated and does not work on Windows."))
    }
}






checkDescriptionNamespaceConsistency <- function(pkgname)
{
    dImports <- cleanupDependency(packageDescription(pkgname)$Imports)
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
        handleRecommended(sprintf(
            "Import %s in DESCRIPTION as well as NAMESPACE.",
            paste(badones, collapse=", ")))

    }
}

## Make sure this is run after pkg is installed.
checkForBadDepends <- function(pkgdir)
{
    pkgname <- strsplit(basename(pkgdir), "_")[[1]][1]
    output <- getBadDeps(pkgdir)

    if (!is.null(output))
    {
        output <- strsplit(output, "\n")[[1]]
        res <- regexpr("'[^']*'$", output)
        fns <- regexpr("^[^:]*:", output)
        fmatch.length <- attr(fns, "match.length")
        if (any(res != -1))
        {
            res <- substr(output, res, nchar(output))
            fns <- unique(substr(output, fns, fmatch.length-1))
            res <- gsub("'", "", fixed=TRUE, res)
            res <- unique(res)
            badFunctions <- paste(fns, collapse=", ")
            badObjects <- paste(res, collapse=", ")
            msg <- sprintf(paste0(
                "Packages that provide %s\n",
                "  (used in %s)\n",
                "  should be in Imports, not Depends (and imported in NAMESPACE),\n",
                "  otherwise packages that import %s could fail."),
                badObjects, badFunctions, pkgname)
            handleRequired(msg)
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


## FIXME - turns out this is slow when run against
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
    msgs <- character(0)
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
        if (length(exportedTopics) && 
            !doesManPageHaveRunnableExample(rd)) 
        {
            msg <- sprintf(paste0("Man page %s documents exported\n", 
                "  topic(s) %s\n  but has no runnable examples."),
                basename(manpage), paste(exportedTopics, collapse=", "))
            msgs <- append(msgs, msg)
        }

    }
    if (length(msgs))
    {
        handleNote("Man pages of exported objects had no running examples:")
        for (msg in msgs)
        {
            handleMessage(msg)
        }
    }
    msgs # for testing
}

checkNEWS <- function(pkgdir)
{
    newsloc <- file.path(pkgdir, c("inst", "inst", "."), 
            c("NEWS.Rd", "NEWS", "NEWS"))
    news <- head(newsloc[file.exists(newsloc)], 1)
    if (0L == length(news)) 
    {
        handleNote(paste0("No NEWS file. Package news will not be included\n",
            "  in Bioconductor release announcements."))
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
        handleRecommended(sprintf(paste0("Fix formatting of %s!\n",
            "  Malformed package NEWS will not be included ",
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
            pattern="\\.Rnw$|\\.Rmd$|\\.Rrst$", ignore.case=TRUE),
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
        if (file.exists(file))
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
        handleNote(sprintf(" %s lines (%i%%) are > 80 characters long!",
            longlines, as.integer((longlines/totallines) * (100/1) )))
    }
    if (tablines > 0)
    {
        ok <- FALSE
        handleNote(sprintf(" %s lines (%i%%) contain tabs!",
            tablines, as.integer((tablines/totallines) * (100/1) )))
    }
    if (badindentlines > 0)
    {
        ok <- FALSE
        handleNote(sprintf(" %s lines (%i%%) are not indented by a multiple of 4 spaces!",
            badindentlines,
            as.integer((badindentlines/totallines) * (100/1) )))

    }

    if (!ok)
    {
        message("  See http://bioconductor.org/developers/how-to/coding-style/")
    }
}