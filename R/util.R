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


handleMessage <- function(msg, appendLF=TRUE)
{
    .msg("* %s", msg, appendLF=appendLF)
}

handleRequired <- function(msg)
{
    .requirements$add(msg)
    .msg("* REQUIRED: %s", msg, indent=4, exdent=6)
    #.stop(msg)
}

handleRecommended <- function(msg)
{
    .recommendations$add(msg)
    .msg("* RECOMMENDED: %s", msg, indent=4, exdent=6)
}

handleConsideration <- function(msg)
{
    .considerations$add(msg)
    .msg("* CONSIDER: %s", msg, indent=4, exdent=6)
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
    if (is.null(input)) return(character(0))
    output <- gsub("\\s", "", input)
    output <- gsub("\\([^)]*\\)", "", output)
    res <- strsplit(output, ",")[[1]]
    unique(res[which(res != "R")])
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
    if (grepl("\\.Rnw$|\\.Rmd|\\.Rrst|\\.Rhtml$|\\.Rtex", infile, TRUE))
    {
        outfile <- NULL
        desc <- file.path(pkgdir, "DESCRIPTION")
        dcf <- read.dcf(desc)
        if ("VignetteBuilder" %in% colnames(dcf) && dcf[,"VignetteBuilder"] == "knitr")
        {
            outfile <- file.path(tempdir(), "parseFile.tmp")
            suppressWarnings(suppressMessages(capture.output({
                purl(infile, outfile, documentation=0L)
            })))
            
        } else {
            oof <- file.path(tempdir(), basename(infile))
            segs <- strsplit(oof, '.', TRUE)[[1]]
            outfile <- paste0(paste(segs[1:length(segs)-1]), '.R')
            suppressWarnings(suppressMessages(capture.output({
                    oldwd <- getwd()
                    on.exit(setwd(oldwd))
                    setwd(tempdir())
                    Stangle(infile)
            })))
        }

    } else if (grepl("\\.Rd$", infile, TRUE)) 
    {
        rd <- parse_Rd(infile)
        outfile <- file.path(tempdir(), "parseFile.tmp")
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
            if (!silent)
            {
                if (grepl("\\.R$", name, ignore.case=TRUE))
                    message(sprintf("      Found %s%s in %s (line %s, column %s)",
                        symbolName, parens,
                        mungeName(name, pkgname), x[i,1], x[i,2]))
                else
                    message(sprintf("      Found %s%s in %s",
                        symbolName, parens,
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

getPkgNameFromPkgDir <- function(pkgdir)
{
    t <- tempdir()
    ret <- sub(t, "", pkgdir)
    ret <- gsub(.Platform$file.sep, "", ret)
    ret
}

loadRefClasses <- function()
{
    assign(".requirements", new("MsgClass", msg=character(0)),
        envir=.GlobalEnv)
    assign(".recommendations", new("MsgClass", msg=character(0)),
        envir=.GlobalEnv)
    assign(".considerations", new("MsgClass", msg=character(0)),
        envir=.GlobalEnv)
}