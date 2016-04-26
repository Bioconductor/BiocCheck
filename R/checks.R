

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
        handleRequired(paste("Version number in tarball",
            "filename must match Version field in DESCRIPTION.",
            "(Tip: create tarball with R CMD build)"))
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
                    handleRecommended(sprintf(
                        "Update R version dependency from %s to %s.",
                        ver, bv))
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
        handleConsideration(paste("Using accessors;",
            "don't access S4 class slots via",
            "'@' in examples/vignettes."))
    }
}

checkVignetteDir <- function(pkgdir, checkingDir)
{
    vigdir <- file.path(pkgdir, "vignettes")
    instdocdir <- file.path(pkgdir, "inst", "doc")
    if (!file.exists(vigdir))
    {
        if (isInfrastructurePackage(pkgdir))
        {
            .msg("  Infrastructure package, vignette not required.",
                indent=2)
            return()
        }
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

    if (file.exists(file.path("pkgdir", "DESCRIPTION")))
    {
        vigns <- tools:::pkgVignettes(dir=pkgdir, check=TRUE)
        if (is.null(vigns))
        {
            handleRequired("No vignettes!")
            return()
        }
        if (length(vigns$msg))
        {
            handleRequired(paste(vigns$msg, collapse="\n"))
            return()
        }

    }


    chunks <- 0
    efs <- 0
    for (file in vigdircontents)
    {
        lines <- readLines(file, warn=FALSE)
        vignetteType <- knitr:::detect_pattern(lines, knitr:::file_ext(file))
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

    message(sprintf(
        "      # of chunks: %s, # of eval=FALSE: %s (%i%%)",
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
                ("New package version starting with 0.99.* (e.g., 0.99.0, 0.99.1, ...); got '%s'.",version))

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


checkBiocViews <- function(pkgdir)
{
    dirty <- FALSE
    dcf <- read.dcf(file.path(pkgdir, "DESCRIPTION"))
    handleMessage("Checking that biocViews are present...")
    if (!"biocViews" %in% colnames(dcf))
    {
        handleRequired("Add some biocViews!")
        return(TRUE)
    }
    biocViews <- dcf[, "biocViews"]
    views <- strsplit(gsub("\\s*,\\s*", ",", biocViews), ",")[[1]]
    views <- gsub("\\s", "", views)
    data("biocViewsVocab", package="biocViews", envir=environment())

    handleMessage("Checking for non-trivial biocViews...")
    toplevel <- c("Software", "AnnotationData", "ExperimentData")
    if (all(views %in% toplevel)) {
        handleRequired(sprintf("Add biocViews other than %s",
                               paste(unique(views), collapse=", ")))
        return(TRUE)
    }

    parents <- c()
    for (view in views)
    {
        parents <- c(parents, getParent(view, biocViewsVocab))
    }
    handleMessage("Checking that biocViews come from the same category...")
    if (length(unique(parents)) > 1)
    {
        handleRecommended(paste0("Use biocViews from one category only",
            " (one of Software, ExperimentData, AnnotationData)"))
        return(TRUE)
    }
    branch <- unique(parents)


#    biocViewsVocab <- NULL ## to keep R CMD check happy
    if (interactive())
        env <- environment()
    else
        env <- .GlobalEnv

    handleMessage("Checking biocViews validity...")
    if (!all(views %in% nodes(biocViewsVocab)))
    {
        badViews <- paste(views[!(views %in% nodes(biocViewsVocab))],
            collapse=", ")
        handleRecommended(paste("Use valid biocViews. Invalid ones:",
            badViews))
        dirty <- TRUE
    }

    if (packageVersion("biocViews") < package_version("1.33.9"))
    {
        if (branch == "Software")
        {
            branch = "software"
        } else if (branch == "AnnotationData")
        {
            branch = "annotation"
        } else if (branch == "ExperimentData")
        {
            branch = "experiment"
        }
    }



    rec <- NULL
    handleMessage("Checking for recommended biocViews...")
    tryCatch(suppressMessages(
        suppressWarnings(rec <- recommendBiocViews(pkgdir, branch))),
        error=function(e){
        })

    if (!is.null(rec))
    {
        if (length(rec$recommended) == 1 && rec$recommended == "")
        {
        } else {
            handleConsideration(paste(
                "Adding some of these automatically suggested biocViews: ",
                rec$recommended))
            dirty <- TRUE
        }


    }
    return(dirty)
}

checkBBScompatibility <- function(pkgdir)
{
    lines <- readLines(file.path(pkgdir, "DESCRIPTION"), warn=FALSE)
    handleMessage("Checking for blank lines in DESCRIPTION...")
    if (any(nchar(lines)==0))
    {
        handleRequired("Remove blank lines from DESCRIPTION!")
        return()
    }
    handleMessage("Checking for whitespace in DESCRIPTION field names...")
    dcf <- read.dcf(file.path(pkgdir, "DESCRIPTION"))
    if (any(grepl("\\s", colnames(dcf))))
    {
        handleRequired("Remove whitespace from DESCRIPTION field names.")
        return()
    }
    segs <- strsplit(pkgdir, .Platform$file.sep)[[1]]
    pkgNameFromDir <- segs[length(segs)]
    handleMessage("Checking that Package field matches dir/tarball name...")
    if (dcf[, "Package"] != pkgNameFromDir)
    {
        handleRequired(sprintf(
            "Package dir %s must match Package: field ( got %s)!",
            pkgNameFromDir, dcf[, "Package"]))
            return()
    }
    handleMessage("Checking for Version field...")
    if (!"Version" %in% colnames(dcf))
    {
        handleRequired("Version field in DESCRIPTION!")
        return()
    }
    handleMessage("Checking for valid maintainer...")
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
    cond <- length(dir(tests_dir, pattern = "\\.(R|Rin)$"))
    if (dir.exists(tests_dir) && (!cond))
    {
        handleRequired(paste("Add a .R or .Rin file in tests/ directory",
            "or unit tests will not be run by R CMD check! See",
            "http://bioconductor.org/developers/how-to/unitTesting-guidelines/",
            "for details on configuring unit tests."))
        return()
    }
    if (!(dir.exists(tests_dir) && cond))
    ## end stolen code
    {
        msg <- paste0("Adding unit tests.\n",
            "  We strongly encourage them. See\n",
            "  http://bioconductor.org/developers/how-to/unitTesting-guidelines/."
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
                paste(unique(badones), collapse=", ")))
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

            handleMessage("Checking if other packages can import this one...")

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

            handleMessage("Checking to see if we understand object initialization....")


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
                    sQuote(paste(trimws(noteObjects), collapse=", ")),
                    paste(trimws(noteFunctions), collapse=", "),
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
    system2(cmd, args, stdout=TRUE, stderr=FALSE,
        env="R_DEFAULT_PACKAGES=NULL")
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
        handleRecommended(msg)
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
            message(sprintf(
                "  The longest function is %s lines long", max(h$length)))
            message(sprintf("  The longest %s functions are:", nrow(h)))
            for (i in 1:nrow(h))
            {
                row <- df[i,]
                if (grepl("\\.R$", row$filename, ignore.case=TRUE))
                {
                    message(sprintf("      %s() (%s, line %s): %s lines",
                        row$functionName, row$filename, row$startLine, row$length))
                } else {
                    message(sprintf("      %s() (%s): %s lines",
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

    tc <- textConnection("ex", "w")
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
    manpages <- dir(file.path(pkgdir, "man"),
        pattern="\\.Rd$", ignore.case=TRUE, full.names=TRUE)
    badpages <- c()
    for (manpage in manpages)
    {
        rd <- parse_Rd(manpage)
        tags <- tools:::RdTags(rd)

        value <- NULL
        if ("\\usage" %in% tags && (!"\\value" %in% tags))
        {
            badpages <- append(badpages, basename(manpage))
            next
        }

        if ("\\value" %in% tags)
            value <- rd[grep("\\value", tags)]


        if ("\\usage" %in% tags && !is.null(value))
        {
            if ((is.list(value[[1]]) && length(value[[1]]) == 0) ||
                nchar(gsub("^\\s+|\\s+$", "", paste(unlist(value), collapse='')))==0 )
            {
                badpages <- append(badpages, basename(manpage))
            }
        }

    }
    if (length(badpages)) {
        handleRecommended(paste("Add non-empty \\value sections to the",
            "following man pages:"))
        .msg(paste(badpages, collapse=", "), indent=6, exdent=6)

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
        && ratio  <= (0.8 / 1.0))
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
    files <- c(file.path(pkgdir, "NAMESPACE"),
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
            n <- nchar(lines, allowNA=TRUE)
            n <- n[!is.na(n)]

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
        if (any(grepl("^%% ~", lines)))
            bad <- append(bad, basename(manpage))
    }
    if (length(bad) > 0)
    {
        handleConsideration(sprintf(
            "Removing generated comments from man pages %s ",
            paste(bad, collapse=", ")))
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
    url <- paste0("https://support.bioconductor.org/api/email/",
        email, "/")
    if(suppressMessages(content(GET(url))))
    {
        handleMessage("Maintainer is registered at support site!")
    } else {
        handleRequired(paste0("Maintainer must register at the support site;",
            " visit https://support.bioconductor.org/accounts/signup/ ."))
    }

}


checkForBiocDevelSubscription <- function(pkgdir)
{
    email <- getMaintainerEmail(pkgdir)
    if (!exists("email"))
        return()
    if (tolower(email) == "maintainer@bioconductor.org")
    {
        handleMessage("Maintainer email is ok.")
        return()
    }
    response <- POST("https://stat.ethz.ch/mailman/admin/bioc-devel",
        body=list(adminpw=Sys.getenv("BIOC_DEVEL_PASSWORD")))
    if (status_code(response) >= 300)
    {
        warning(immediate. = TRUE,
             "An error occurred communicating with mailing list: ",
             "'https://stat.ethz.ch/mailman/admin/bioc-devel'. ",
             "Server returned status: ", status_code(response))
        return()
    }
    response2 <- POST("https://stat.ethz.ch/mailman/admin/bioc-devel/members?letter=4",
        body=list(findmember=email))
    content <- content(response2, as="text")
    if(grepl(paste0(">", tolower(email), "<"), tolower(content), fixed=TRUE))
    {
        handleMessage("Maintainer is subscribed to bioc-devel!")
    } else {
        handleRequired(paste0(
            "Maintainer should subscribe to the bioc-devel mailing list.",
            " Subscribe here: ",
            "https://stat.ethz.ch/mailman/listinfo/bioc-devel"))
    }
}

checkIsPackageAlreadyInRepo <- function(pkgName,
    repo=c("CRAN", "Bioconductor"))
{
    if (repo=="CRAN")
        repo.url <- "http://cran.fhcrc.org/src/contrib/PACKAGES"
    else
        repo.url <- sprintf("%s/src/contrib/PACKAGES",
            biocinstallRepos()["BioCsoft"])
    conn <- url(repo.url)
    dcf <- read.dcf(conn)
    close(conn)
    if (tolower(pkgName) %in% tolower(dcf[,"Package"]))
    {
        if(repo=="CRAN")
            msg <- "Package must be removed from CRAN."
        else
            msg <- sprintf("Rename your package (%s already exists in Bioconductor).",
                pkgName)
        handleRequired(msg)
    }
}

checkIsVignetteBuilt <- function(package_dir, build_output_file)
{
    if (!file.exists(build_output_file))
    {
        stop(sprintf("build output file '%s' does not exist!", build_output_file))
    }
    lines <- readLines(build_output_file)
    if (!any(grepl("^\\* creating vignettes \\.\\.\\.", lines)))
    {
        msg <- "Vignette must be built by
        'R CMD build'. Please see the `Vignette Checks` section of
        the BiocCheck vignette."
        handleRequired(msg)
    }
}
