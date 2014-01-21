.printf <- function(...) cat(noquote(sprintf(...)), "\n")

.msg <- function(...) message(noquote(sprintf(...)))
.stop <- function(...) stop(noquote(sprintf(...)), call.=FALSE)


handleMessage <- function(msg)
{
    .msg("* %s", msg)
}

handleError <- function(msg)
{
    .errors$add(msg)
    .msg("* REQUIRED: %s", msg)
    #.stop(msg)
}

handleWarning <- function(msg)
{
    .warnings$add(msg)
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
        handleError(sprintf("Failed to install %s!", pkg))

    }
    pkgname <- strsplit(basename(pkg), "_")[[1]][1]
    args <- list(package=pkgname, lib.loc=libdir)
    if (paste0("package:",pkgname) %in% search())
        unload(file.path(libdir, pkgname))
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
    if (grepl("\\.Rnw$|\\.Rmd|\\.Rrst", infile, TRUE))
    {
        dcf <- read.dcf(file.path(pkgdir, "DESCRIPTION"))
        if ("VignetteBuilder" %in% colnames(dcf))
        {
            outfile <- file.path(tempdir(), "parseFile.tmp")
            suppressMessages(capture.output(
                purl(infile, outfile, documentation=0L)
            ))
        } else {
            oldwd <- getwd()
            on.exit(setwd(oldwd))
            setwd(tempdir())
            outfile <- file.path(tempdir(), sub("\\.Rnw$", ".R", TRUE), basename(infile))
            Stangle(infile)
        }
    } else if (grepl("\\.Rd", infile, TRUE)) 
    {
        rd <- parse_Rd(infile)
        outfile <- file.path(tempdir(), "parseFile.tmp")
        capture.output(code <- Rd2ex(rd))
        cat(code, file=outfile)
    } else if (grepl("\\.R", infile, TRUE)) {
        #message(sprintf("infile is %s", infile))
        outfile <- infile
    }
    p <- parse(outfile)
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
        parsedCode[[file]] <- df
    }
    parsedCode
}
