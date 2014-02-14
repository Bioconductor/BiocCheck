.printf <- function(...) cat(noquote(sprintf(...)), "\n")

.debug <- function(...) if (getOption("Bioconductor.DEBUG", FALSE))
    .printf(...)
.msg <- function(..., appendLF=TRUE)
{
    txt <- sprintf(...)
    message(paste(strwrap(txt, indent=0, exdent=2), collapse="\n"),
        appendLF=appendLF)
}
.stop <- function(...) stop(noquote(sprintf(...)), call.=FALSE)


handleMessage <- function(msg, appendLF=TRUE)
{
    .msg("* %s", msg, appendLF=appendLF)
}

handleRequired <- function(msg)
{
    .requirements$add(msg)
    .msg("* REQUIRED: %s", msg)
    #.stop(msg)
}

handleRecommended <- function(msg)
{
    .recommendations$add(msg)
    .msg("* RECOMMENDED: %s", msg)
}

handleNote <- function(msg)
{
    .notes$add(msg)
    .msg("* NOTE: %s", msg)
}

installAndLoad <- function(pkg)
{
    libdir <- file.path(tempdir(), "lib")
    unlink(libdir, recursive=TRUE)
    dir.create(libdir, showWarnings=FALSE)
    stderr <- file.path(tempdir(), "install.stderr")
    res <- system2(file.path(Sys.getenv("R_HOME"), "bin", "R"),
        sprintf("--vanilla CMD INSTALL --no-test-load --library=%s %s",
        libdir, pkg),
        stdout=NULL, stderr=stderr)
    if (res != 0) 
    {
        cat(paste(readLines(stderr), collapse="\n"))
        handleRequired(sprintf("%s must be installable!", pkg))

    }
    pkgname <- strsplit(basename(pkg), "_")[[1]][1]
    args <- list(package=pkgname, lib.loc=libdir)
    if (paste0("package:",pkgname) %in% search())
        suppressWarnings(unload(file.path(libdir, pkgname)))

    suppressPackageStartupMessages(do.call(library, args))
}

cleanupDependency <- function(input)
{
    if (is.null(input)) return(NULL)
    output <- gsub("\\s", "", input)
    output <- gsub("\\([^)]*\\)", "", output)
    res <- strsplit(output, ",")[[1]]
    res[which(res != "R")]
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

parseFile <- function(infile, pkgdir)
{
    # FIXME - use purl to parse RMD and RRST
    # regardless of VignetteBuilder value
    if (grepl("\\.Rnw$|\\.Rmd|\\.Rrst", infile, TRUE))
    {
        dcf <- read.dcf(file.path(pkgdir, "DESCRIPTION"))
        if ("VignetteBuilder" %in% colnames(dcf) &&
            dcf[, "VignetteBuilder"]=="knitr")
        {
            outfile <- file.path(tempdir(), "parseFile.tmp")
            suppressMessages(capture.output(
                purl(infile, outfile, documentation=0L)
            ))
        } else {
            oldwd <- getwd()
            on.exit(setwd(oldwd))
            setwd(tempdir())
            outfile <- file.path(tempdir(),
                sub("\\.Rnw$", ".R", basename(infile), ignore.case=TRUE))
            capture.output(Stangle(infile))
        }
    } else if (grepl("\\.Rd", infile, TRUE)) 
    {
        rd <- parse_Rd(infile)
        outfile <- file.path(tempdir(), "parseFile.tmp")
        code <- capture.output(Rd2ex(rd))
        cat(code, file=outfile, sep="\n")
    } else if (grepl("\\.R", infile, TRUE)) {
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
        pattern="\\.Rnw$|\\.Rmd$|\\.Rrst", ignore.case=TRUE, full.names=TRUE)
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
    token)
{
    matches <- list()
    for (filename in names(parsedCode))
    {
        df <- parsedCode[[filename]]
        matchedrows <- 
            df[which(df$token == 
                token & df$text == symbolName),]
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
            if (grepl("\\.R$", name, ignore.case=TRUE))
                message(sprintf("  Found %s%s in %s (line %s, column %s)",
                    symbolName, parens,
                    mungeName(name, pkgname), x[i,1], x[i,2]))
            else
                message(sprintf("  Found %s%s in %s",
                    symbolName, parens,
                    mungeName(name, pkgname))) # FIXME test this

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


