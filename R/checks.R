

getVigSources <- function(dir)
{
    dir(dir,
        pattern="\\.Rmd$|\\.Rnw$|\\.Rrst$",
        ignore.case=TRUE)
}

checkVignetteDir <- function(pkgdir)
{
    vigdir <- file.path(pkgdir, "vignettes")
    instdocdir <- file.path(pkgdir, "inst", "doc")
    if (!file.exists(vigdir))
    {
        handleError("No 'vignettes' directory!")
        return()
    }
    vigdircontents <- getVigSources(vigdir)
    if (length(vigdircontents)==0)
    {
        handleError("No vignette sources in vignettes/ directory.")
        return()
    }
    instdocdircontents <- getVigSources(instdocdir)
    if (length(instdocdircontents) > 0)
    {
        handleWarning("Vignette sources exist in inst/doc/; they belong in vignettes/.")
    }


}

checkNewPackageVersionNumber <- function(pkgdir)
{
    dcf <- read.dcf(file.path(pkgdir, "DESCRIPTION"))
    version <- dcf[, "Version"]
        if(!grepl("^0[-.]99[-.][0-9]+$", version))
            handleError(sprintf
                ("Version %s is wrong for new package; should start with 0.99.",version))

}

checkVersionNumber <- function(pkgdir, new_package=FALSE)
{
    dcf <- read.dcf(file.path(pkgdir, "DESCRIPTION"))
    version <- dcf[, "Version"]
    regex <- "^[0-9]+[-\\.]([0-9]+)[-\\.][0-9]+$"
    if(!grepl(regex, version))
    {
        handleError("Invalid package Version")
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
            handleWarning(sprintf("y of x.y.z version should be %s in %s",
                    shouldBe, vers))
        }

        },
        error=function(e) handleError("Invalid package version"))
}

checkBiocViews <- function(pkgdir)
{
    dcf <- read.dcf(file.path(pkgdir, "DESCRIPTION"))
    if (!"biocViews" %in% colnames(dcf))
    {
        handleWarning("No biocViews found!")
        return()
    } else {
        biocViews <- dcf[, "biocViews"]
        views <- strsplit(gsub("\\s", "", biocViews), ",")[[1]]
        library(biocViews)
        data(biocViewsVocab)
        if (!all(views %in% nodes(biocViewsVocab)))
        {
            badViews <- paste(views[!(views %in% nodes(biocViewsVocab))],
                collapse=", ")
            handleWarning(paste("Some biocViews are invalid:",
                badViews))
        }
    }

    getParent <- function(view)
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
        parents <- c(parents, getParent(view))
    }
    if (length(unique(parents)) > 1)
    {
        handleWarning(paste("Including biocViews from more than one category",
            "(Software, ExperimentData, AnnotationData)"))
    }
}

checkBBScompatibility <- function(pkgdir)
{
    dcf <- read.dcf(file.path(pkgdir, "DESCRIPTION"))
    segs <- strsplit(pkgdir, .Platform$file.sep)[[1]]
    pkgNameFromDir <- segs[length(segs)]
    if (dcf[, "Package"] != pkgNameFromDir)
    {
        handleError(sprintf(
            "Package dir %s does not match Package: field %s!",
            pkgNameFromDir, dcf[, "Package"]))
            return()
    }
    if (!"Version" %in% colnames(dcf))
    {
        handleError("Version field not found in DESCRIPTION!")
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
                handleError("Failed to evaluate Authors@R field!")
            })
        if (!exists("people")) return()
        if (!"person" %in% class(people))
        {
            handleError("Authors@R does not evaluate to 'person' object!")
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
            handleError("No author with maintainer (cre) role.")
            return()
        }
    } else if ("Maintainer" %in% colnames(dcf)) {
        maintainer <- dcf[,"Maintainer"]
    } else {
        handleError("No Maintainer or Authors@R field in DESCRIPTION file!")
        return()
    }
    # now need to make sure that regexes work, a la python/BBS 
    regex = '(.*\\S)\\s*<(.*)>\\s*'
    match <- regexec(regex, maintainer)[[1]]
    match.length <- attr(match, "match.length")
    #if (!  (all(match)  > 0) && (all(match.length) > 0) )
    if (match == -1 && match.length == -1)
    {
        handleError("Couldn't get email address from Maintainer field.")
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
        # This is stupid and may fail in other locales.
        x <- capture.output(r)
        if (length(x) == 1)
        {
            handleWarning("Package has a DLL but no registered routines!")
        }
    }
}

checkImportSuggestions <- function(pkgname)
{
    suggestions <- NULL
    tryCatch({
        suppressMessages({
            suggestions <- capture.output(writeNamespaceImports(pkgname))
        })
    },
        error=function(e){
            handleMessage("Could not get namespace suggestions.")
            })
    if(!is.null(suggestions))
    {
            handleMessage("Namespace import suggestions are:")
            cat(paste(suggestions, collapse="\n"))
            handleMessage("--END of namespace import suggestions.")
    }
}

checkDeprecatedPackages <- function(pkgdir)
{
    if ("multicore" %in% getAllDependencies(pkgdir))
    {
        handleError(paste("'multicore' is deprecated and does not work on",
            "Windows. Use 'parallel' instead."))
    }
}


checkTorF <- function(parsedCode)
{
    t <- list()
    f <- c()
    for (filename in names(parsedCode))
    {
        df <- parsedCode[[filename]]
        trows <- df[which(df$token == "SYMBOL" & df$text =="T"),]
        frows <- df[which(df$token == "SYMBOL" & df$text =="F"),]
        if (nrow(trows) > 0) 
        hasT <- dim(trows)[1] > 0
        hasF <- dim(frows)[1] > 0 
        if (hasT) t <- append(t, filename)
        if (hasF) f <- append(f, filename)

    }
    #FIXME - print output
    list(t=t, f=f) # for tests
}


mungeName <- function(name, pkgname)
{
    pos <- regexpr(pkgname, name)
    substr(name, pos+1+nchar(pkgname), nchar(name))
}


checkForDotC <- function(parsedCode, pkgname)
{
    dotc <- list()
    for (filename in names(parsedCode))
    {
        df <- parsedCode[[filename]]
        dotcrows <- df[which(df$token == "SYMBOL_FUNCTION_CALL" & df$text ==".C"),]
        if (nrow(dotcrows) > 0)
        {
            dotc[[filename]] <- dotcrows[, c(1,2)]
        }
    }
    for (name in names(dotc))
    {
        x <- dotc[[name]]
        for (i in nrow(x))
        {
            if (grepl("\\.R$", name, ignore.case=TRUE))
                message(sprintf("Found .C in %s (line %s, column %s)",
                    mungeName(name, pkgname), x[i,1], x[i,2]))
            else
                message(sprintf("Found .C in %s", name)) # FIXME test this

        }
    }
    dotc # for tests
}

checkParsedFiles <- function(pkgdir)
{
    handleLoop <- function(loop)
    {
        for (item in loop)
        {
            message(item, appendLF=TRUE)
            message(" ", appendLF=TRUE)
        }
        message("")
    }

    t <- c()
    f <- c()
    dotc <- c()
    callbacks <- list(
        # check for T or F
        function(df, filename) {
            trows <- df[which(df$token == "SYMBOL" & df$text =="T"),]
            frows <- df[which(df$token == "SYMBOL" & df$text =="F"),]
            hasT <- dim(trows)[1] > 0
            hasF <- dim(frows)[1] > 0 
            if (hasT) t <- append(t, filename)
            if (hasF) f <- append(f, filename)
        },
        # check for .C
        function(df, filename)
        {
            dotcrows <- df[which(df$token == "SYMBOL_FUNCTION_CALL" & df$text ==".C"),]
            hasdotc <- dim(dotcrows)[1] > 0
            if (hasdotc) dotc <- append(dotc, filename)
        }
    )
    parseFiles(pkgdir, callbacks)
    if (length(t))
    {
        handleWarning("Symbol T found (use TRUE instead) in files:")
        message("* ", appendLF=FALSE)
        handleLoop(t)
    }
    if (length(f))
    {
        handleWarning("Symbol F found (use FALSE instead) in files:")
        message("* ", appendLF=FALSE)
        handleLoop(t)
    }
    if (length(dotc))
    {
        handleMessage(".C found in files:")
        message("* ", appendLF=FALSE)
        handleLoop(dotc)
    }
}

checkDescriptionNamespaceConsistency <- function(pkgname)
{
#     if (!all(names(getNamespaceImports(pkgname)) %in% loadedNamespaces()))
#    {
#        handleWarning("Not all packages imported in NAMESPACE are in Description:Imports") #??
#        # FIXME be specific
#    }
    dImports <- cleanupDependency(packageDescription(pkgname)$Imports)
    nImports <- names(getNamespaceImports(pkgname))
    nImports <- nImports[which(nImports != "base")]

    if(!(all(dImports %in% nImports)))
    {
        badones <- dImports[!dImports %in% nImports]
        handleWarning(sprintf("%s imported in DESCRIPTION but not NAMESPACE",
            paste(badones, collapse=", ")))
    }
    if (!all (nImports %in% dImports))
    {
        badones <- nImports[!nImports %in% dImports]
        handleWarning(sprintf(
            "%s imported in NAMESPACE but not in DESCRIPTION:Imports",
            paste(badones, collapse=", ")))

    }
}