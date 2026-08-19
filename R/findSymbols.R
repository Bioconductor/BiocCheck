.filter_lookback <- function(index, parsedDF, lookback, text) {
    vapply(index, function(idx) {
        rangeLookback <- seq(idx - length(lookback), idx)
        setequal(
            parsedDF$text[rangeLookback], c(lookback, text)
        )
    }, logical(1L))
}

.getTokenTextCode <- function(
    parsedf, token, text, notLookback = character(0), hasLookback = character(0)
) {
    cond <- parsedf$token %in% token & parsedf$text %in% text
    if (length(notLookback) && any(cond)) {
        cond[cond] <- !.filter_lookback(which(cond), parsedf, notLookback, text)
    }
    if (length(hasLookback) && any(cond)) {
        cond[cond] <- .filter_lookback(which(cond), parsedf, hasLookback, text)
    }
    parsedf[
        cond,
        c("line1", "col1", "token", "text"),
        drop = FALSE
    ]
}

.grepTokenTextCode <- function(parsedf, token, text) {
    parsedf[
        parsedf$token %in% token & grepl(text, parsedf$text),
        c("line1", "col1", "token", "text"),
        drop = FALSE
    ]
}

findSymbolInParsedCode <-
    function(parsedCode, pkgname, symbolName, token, silent=FALSE)
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
        parens <- "()"
    else
        parens <- ""
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
                        parens, .getDirFiles(name), x[i,1], x[i,2]))
                else
                    handleMessage(sprintf(
                        "Found %s%s in %s", symbolName, parens,
                        .getDirFiles(name))) # FIXME test this
            }
        }
    }
    length(matches) # for tests
}

findSymbolsInParsedCode <-
    function(
        parsedCodeList, symbolNames, tokenTypes,
        FUN = .getTokenTextCode, fun = TRUE, ...
    )
{
    matches <- structure(vector("list", length(parsedCodeList)),
        names = names(parsedCodeList))
    allcombos <- expand.grid(
        tokenTypes = tokenTypes,
        symbolNames = symbolNames,
        stringsAsFactors = FALSE
    )
    tokenTypes <- allcombos[["tokenTypes"]]
    symbolNames <- allcombos[["symbolNames"]]

    for (filename in names(parsedCodeList)) {
        df <- parsedCodeList[[filename]]
        res <- Map(
            function(x, y) {
                FUN(parsedf = df, token = x, text = y, ...)
            },
            x= tokenTypes, y = symbolNames
        )
        res <- do.call(rbind.data.frame, res)
        matches[[filename]] <- res
    }

    matches <- Filter(nrow, matches)
    if (!length(matches))
        return(character(0L))

    matches <- lapply(names(matches), function(nm) {
        dframe <- matches[[nm]]
        dframe[["text"]] <- paste0(dframe$text,
            ifelse(dframe$token == "SYMBOL_FUNCTION_CALL", "()", ""))
        dframe[["filename"]] <- nm
        dframe
    })

    matches <- do.call(
        function(...) rbind.data.frame(..., make.row.names = FALSE),
        matches
    )
    matches[] <- lapply(matches, as.character)
    apply(matches, 1L, function(rowdf) {
        fmttxt <- "%s (line %s, column %s)"
        formt <- if (fun) paste0(rowdf["text"], " in ", fmttxt) else fmttxt
        sprintf(formt, .getDirFiles(rowdf["filename"]),
            rowdf["line1"], rowdf["col1"]
        )
    })
}

findSymbolsInRFiles <-
    function(.BiocPackage, Symbols, tokenType, fun = TRUE, ...)
{
    rfiles <- .BiocPackage$RSources
    parsedCodes <- lapply(
        structure(rfiles, names = rfiles), parseFile,
        .BiocPackage = .BiocPackage
    )
    msg_res <- findSymbolsInParsedCode(
        parsedCodeList = parsedCodes,
        symbolNames = Symbols,
        tokenTypes = tokenType,
        fun = fun, ...
    )
    unlist(msg_res)
}

.VigCodeChunkNo <- function(parsedf, tokens) {
    if (!nrow(tokens))
        return(integer(0L))
    sChunksInd <- .grepTokenTextCode(
        parsedf, "COMMENT", "^## -|^### code chunk"
    )[["line1"]]
    chunkNo <- vector("integer", nrow(tokens))
    for (i in seq_len(nrow(tokens))) {
        numrow <- tokens[["line1"]][i]
        chunkInd <- max(sChunksInd[sChunksInd < numrow])
        chunkNo[[i]] <- match(chunkInd, sChunksInd, nomatch = 0L)
    }
    chunkNo
}

findSymbolsInVignettes <-
    function(.BiocPackage, Symbols, tokenTypes, FUN = .getTokenTextCode, ...)
{
    vigfiles <- .BiocPackage$VigSources
    shortnames <- .getDirFiles(vigfiles)
    viglist <- structure(vector("list", length(vigfiles)), names = shortnames)
    for (i in seq_along(vigfiles)) {
        shortName <- shortnames[i]
        tempR <- tempfile(fileext=".R")
        try_purl_or_tangle(
            input = vigfiles[i],
            output = tempR,
            quiet = TRUE,
            documentation = 2L
        )
        pfile <- parseFile(.BiocPackage, tempR)
        tokens <- FUN(pfile, tokenTypes, Symbols, ...)
        chunkNos <- .VigCodeChunkNo(pfile, tokens)
        viglist[[shortName]] <- sprintf(
            "%s (chunk no. %d, line %d, column %d)",
            shortName, chunkNos, tokens[,"line1"], tokens[,"col1"]
        )
    }
    Filter(length, viglist)
}
