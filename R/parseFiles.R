parseFile <- function(.BiocPackage, infile) {
    dir.create(parse_dir <- tempfile())
    outfile <- file.path(parse_dir, "parseFile.tmp")
    if (grepl("\\.R$", infile, TRUE))
        outfile <- infile
    if (grepl("\\.Rnw$|\\.Rmd|\\.qmd|\\.Rrst|\\.Rhtml$|\\.Rtex", infile, TRUE))
    {
        vigBuilder <- .BiocPackage$VigBuilder
        if ("knitr" %in% vigBuilder)
            checkInstalled("knitr")
        suppressWarnings(suppressMessages(
            capture.output({
                try_purl_or_tangle(
                    input=infile, output=outfile, quiet = TRUE, documentation=0L
                )
            })
        ))
    } else if (grepl("\\.Rd$", infile, TRUE)) {
        rd <- .parse_Rd_pack(infile, usesRdpack = .BiocPackage$usesRdpack)
        code <- capture.output(tools::Rd2ex(rd))
        writeLines(code, con=outfile, sep="\n")
    }
    p <- parse(outfile, keep.source=TRUE)
    getParseData(p)
}

parseFiles <- function(.BiocPackage)
{
    rfiles <- .BiocPackage$RSources
    manfiles <- .BiocPackage$manSources
    vigfiles <- .BiocPackage$VigSources
    files <- c(rfiles, manfiles, vigfiles)
    parsedCode <- structure(vector("list", length(files)), names = files)
    for (file in files)
    {
        df <- parseFile(.BiocPackage, file)
        if (nrow(df))
            parsedCode[[file]] <- df
    }
    Filter(Negate(is.null), parsedCode)
}
