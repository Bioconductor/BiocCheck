# check R coding practice -------------------------------------------------

checkCodingPractice <- function(.BiocPackage, parsedCode)
{
    package_name <- .BiocPackage$packageName
    pkgdir <- .BiocPackage$sourceDir
    Rdir <- file.path(pkgdir, "R")

    # sapply
    msg_sapply <- checkSapply(.BiocPackage)
    if (length(msg_sapply)) {
        handleNoteFiles(
            " Avoid sapply(); use vapply()",
            messages = msg_sapply
        )
    }

    # 1:...
    msg_seq <- check1toN(.BiocPackage)
    if (length(msg_seq)) {
        handleNoteFiles(
            " Avoid 1:...; use seq_len() or seq_along()",
            messages = msg_seq
        )
    }

    # pkg:fun...
    msg_sc <- checkSingleColon(.BiocPackage)
    if (length(msg_sc)) {
        handleErrorFiles(
            " Use double colon for qualified imports: 'pkg::foo()'",
            messages = msg_sc
        )
    }

    # cat() and print()
    msg_cat <- checkCatInRCode(.BiocPackage)
    if (length(msg_cat)) {
        handleNoteFiles(
            " Avoid 'cat' and 'print' outside of 'show' methods",
            messages = msg_cat
        )
    }

    # assignment with =
    msg_eq <- checkEqInAssignment(.BiocPackage)
    if (length(msg_eq)) {
        handleNoteFiles(
            " Avoid using '=' for assignment and use '<-' instead",
            messages = msg_eq
        )
    }

    # message(paste(...))
    msg_mp <- checkPasteInSignaler(.BiocPackage)
    if (length(msg_mp)) {
        handleNoteFiles(
            " Avoid the use of 'paste' in condition signals",
            messages = msg_mp
        )
    }

    # stop("Error: ")
    msg_ss <- checkSignalerInSignaler(.BiocPackage)
    if (length(msg_ss)) {
        handleNoteFiles(
            "Avoid redundant 'stop' and 'warn*' in signal conditions",
            messages = msg_ss
        )
    }

    # T/F
    msg_tf <- findSymbolsInRFiles(
        .BiocPackage, c("T", "F"), "SYMBOL", notLookback = "$"
    )
    if (length(msg_tf)) {
        handleWarning(
            " Avoid T/F variables; If logical, use TRUE/FALSE ",
            help_text = paste("Found", length(msg_tf), "times:"),
            messages = msg_tf
        )
    }

    # class() ==
    msg_class <- checkClassNEEQLookup(.BiocPackage)
    if (length(msg_class)) {
        handleWarningFiles(
            " Avoid class membership checks with class() / is() and == / !=",
            "; Use is(x, 'class') for S4 classes",
            messages = msg_class
        )
    }

    # system() vs system2()
    msg_sys <- findSymbolsInRFiles(
        .BiocPackage, "system", "SYMBOL_FUNCTION_CALL"
    )
    if(length(msg_sys)) {
        handleNoteFiles(
            " Avoid system() ; use system2()",
            messages = msg_sys
        )
    }

    # external data
    msg_eda <- checkExternalData(.BiocPackage)
    if (length(msg_eda)) {
        handleErrorFiles(
            " Avoid references to external hosting platforms",
            messages = msg_eda
        )
    }

    # download / download.file in .onAttach / .onLoad
    msg_dl <- checkOnAttachLoadCalls(.BiocPackage)
    if (length(msg_dl)) {
        handleErrorFiles(
            " Avoid downloads in '.onAttach' or '.onLoad' functions",
            messages = msg_dl
        )
    }

    # set.seed
    msg_seed <-
        findSymbolsInRFiles(.BiocPackage, "set.seed", "SYMBOL_FUNCTION_CALL")
    if (length(msg_seed)){
        handleWarning(
            " Remove set.seed usage (found ", length(msg_seed), " times)",
            messages = msg_seed
        )
    }

    # .Deprecated / .Defunct usage should be updated after every release
    msg_depr <- findSymbolsInRFiles(
        .BiocPackage,
        c(
            ".Deprecated", ".Defunct", "lifeCycle",
            "deprecate_warn", "deprecate_stop"
        ),
        "SYMBOL_FUNCTION_CALL"
    )
    if (length(msg_depr)) {
        handleWarning(
            ".Deprecated / .Defunct usage (found ",
            length(msg_depr), " times)",
            messages = msg_depr
        )
    }

    handleCheck("Checking parsed R code in R directory, examples, vignettes...")

    # direct slot access
    checkForDirectSlotAccess(parsedCode, package_name)

    # browser() calls
    msg_b <-
        findSymbolsInRFiles(.BiocPackage, "browser", "SYMBOL_FUNCTION_CALL")
    if (length(msg_b)) {
        handleWarning(
            "Remove browser() statements (found ", length(msg_b), " times)",
            messages = msg_b
        )
    }

    # <<-
    msg_da <- findSymbolsInRFiles(.BiocPackage, "<<-", "LEFT_ASSIGN")
    if (length(msg_da)) {
        handleNote(
            "Avoid '<<-' if possible (found ", length(msg_da), " times)",
            messages = msg_da
        )
    }

    # Sys.setenv calls
    msg_env <-
        findSymbolsInRFiles(.BiocPackage, "Sys.setenv", "SYMBOL_FUNCTION_CALL")
    if (length(msg_env)) {
        handleError(
            "Avoid 'Sys.setenv' (found ", length(msg_env), " times)",
            messages = msg_env
        )
    }

    # suppressWarnings/Messages calls
    msg_supp <- findSymbolsInRFiles(
        .BiocPackage,
        c("suppressWarnings", "suppressMessages"),
        "SYMBOL_FUNCTION_CALL"
    )
    if (length(msg_supp)) {
        handleNote(
            "Avoid 'suppressWarnings'/'*Messages' if possible (found ",
            length(msg_supp), " times)",
            messages = msg_supp
        )
    }
}

checkSapply <- function(.BiocPackage) {
    msg_sapply <- findSymbolsInRFiles(
        .BiocPackage, "sapply", "SYMBOL_FUNCTION_CALL", FALSE
    )
}

check1toN <- function(.BiocPackage) {
    rfiles <- .BiocPackage$RSources
    msg_seq <- lapply(rfiles, function(rfile) {
        tokens <- getParseData(parse(rfile, keep.source=TRUE))
        tokens <- tokens[tokens[,"token"] != "expr", ,drop=FALSE]
        colons <- which(tokens[,"text"] == ":") - 1
        colons <- colons[tokens[colons, "text"] == "1"]
        tokens <- tokens[colons, , drop=FALSE]
        tokens <- tokens[ tokens[,"text"] == "1", , drop=FALSE]
        sprintf(
            "%s (line %d, column %d)",
            basename(rfile), tokens[,"line1"], tokens[,"col1"]
        )
    })
    msg_seq <- unlist(msg_seq)
}

checkSingleColon <- function(.BiocPackage, avail_pkgs = character(0L)) {

    rfiles <- .BiocPackage$RSources
    names(rfiles) <- basename(rfiles)
    colon_pres <- lapply(rfiles, function(rfile) {
        tokens <- getParseData(parse(rfile, keep.source = TRUE))
        tokens <- tokens[tokens[,"token"] != "expr", ,drop=FALSE]
        colons <- which(tokens[,"text"] == ":") - 1
        colons <- colons[grepl("[[:alpha:]]", tokens[colons, "text"])]
        tokens[colons, , drop = FALSE]
    })
    colon_pres <- Filter(nrow, colon_pres)
    if (length(colon_pres))
        avail_pkgs <- BiocManager::available()
    msg_sc <- lapply(names(colon_pres), function(rfile, framelist) {
        tokens <- framelist[[rfile]]
        tokens <- tokens[tokens[, "text"] %in% avail_pkgs, , drop = FALSE]
        sprintf(
            "%s (line %d, column %d)",
            rfile, tokens[, "line1"], tokens[, "col1"]
        )
    }, framelist = colon_pres)
    msg_sc <- unlist(msg_sc)
}

.filtTokens <-
    function(ind, tokens, keywords = c("paste0", "paste"))
{
    txt <- tokens[ind, "text"]
    filt <- tolower(txt) %in% keywords
    #filt <- grepl(txt, keywords, ignore.case = ignore.case)
    if (any(filt) && "collapse" %in% txt)
        filt <- FALSE
    ind[filt]
}

.getTokens <- function(rfile) {
    tokens <- getParseData(parse(rfile, keep.source = TRUE))
    tokens[tokens[,"token"] != "expr", ,drop=FALSE]
}

.grepSymbolRanges <- function(
    tokens, patterns, tokenType = "SYMBOL_FUNCTION_CALL", isExp = FALSE
) {
    txt <- tokens[, "text"]
    found <- lapply(patterns, function(pattern) grepl(pattern, txt))
    found <- Reduce(`|`, found)
    hits <- which(found & tokens[, "token"] == tokenType)
    openBracket <- if (isExp) "{" else "("
    opar <- which(txt == openBracket)
    startHit <- vapply(hits, function(x) min(opar[opar > x]), numeric(1L))
    parnum <- tokens[startHit, "parent"]
    endHit <- nrow(tokens) - match(parnum, rev(tokens[, "parent"]))
    Map(seq, startHit, endHit)
}

.findMinNext <- function(tokens, after, cname = c("token", "text"), cvalue) {
    cname <- match.arg(cname)
    which(
        seq_len(nrow(tokens)) > after &
            tokens[, cname] == cvalue
    ) |>
        min()
}

.getSigRange <- function(tokens, signalers, bracket) {
    opar <- which(tokens[, "text"] == bracket)
    lapply(
        signalers, function(x) {
            next_opar <- opar[opar > x]
            if (length(next_opar)) {
                startSig <- min(next_opar)
                parnum <- tokens[startSig, "parent"]
                endSig <- nrow(tokens) - match(parnum, rev(tokens[, "parent"]))
            } else {
                sigFun <- .findMinNext(
                    tokens, x, "token", "FUNCTION"
                )
                parnum <- tokens[sigFun, "parent"]
                endFun <- nrow(tokens) - match(parnum, rev(tokens[, "parent"]))

                startSig <- .findMinNext(
                    tokens, after = endFun, "token", "SYMBOL_FUNCTION_CALL"
                ) |>
                    .findMinNext(
                        tokens, after = _, "text", "("
                    )
                parnum <- tokens[startSig, "parent"]
                endSig <- nrow(tokens) - match(parnum, rev(tokens[, "parent"]))
            }
            seq(startSig, endSig)
        }
    )
}

.findSymbolRanges <-
    function(tokens, symbols, tokenType = "SYMBOL_FUNCTION_CALL", isExp = FALSE)
{
    txt <- tokens[, "text"]
    signalers <- which(
        txt %in% symbols & tokens[, "token"] == tokenType
    )
    openBracket <- if (isExp) "{" else "("
    .getSigRange(
        tokens, signalers, openBracket
    )
}

.findInSignaler <- function(rfile, symbols, FUN, ...) {
    tokens <- .getTokens(rfile)
    sigRanges <- .findSymbolRanges(tokens, symbols)
    pasteInd <- lapply(sigRanges, FUN, tokens = tokens, ...)
    tokens <- tokens[unlist(pasteInd), , drop = FALSE]
    rfile <- paste0("R/", basename(rfile))
    sprintf(
        "%s (line %d, column %d)",
        rfile, tokens[, "line1"], tokens[, "col1"]
    )
}

.SIGNALERS_TXT <- c("message", "warning", "stop")

.findPasteInSignaler <- function(rfile, symbols = .SIGNALERS_TXT) {
    .findInSignaler(rfile, symbols, .filtTokens)
}

.filtersetMethodRanges <- function(tokens) {
    excl <- .findSymbolRanges(tokens, "setMethod")
    if (length(excl)) {
        showHits <- vapply(excl,
            function(x) '"show"' %in% tokens[x, "text"], logical(1))
        negind <- unlist(lapply(excl[showHits], `-`))
        tokens <- tokens[negind, ]
    }
    tokens
}

.filterS3printRanges <- function(tokens) {
    excl <- .grepSymbolRanges(tokens, "^print\\..*", tokenType = "SYMBOL")
    if (length(excl)) {
        showHits <- vapply(excl,
            function(x) "cat" %in% tokens[x, "text"], logical(1)
        )
        negind <- unlist(lapply(excl[showHits], `-`))
        tokens <- tokens[negind, ]
    }
    tokens
}

checkCatInRCode <-
    function(.BiocPackage, symbols = c("cat", "print"))
{
    rfiles <- .BiocPackage$RSources
    parsedCodes <- lapply(
        structure(rfiles, .Names = rfiles), parseFile,
        .BiocPackage = .BiocPackage
    )
    parsedCodes <- lapply(parsedCodes, .filtersetMethodRanges)
    parsedCodes <- lapply(parsedCodes, .filterS3printRanges)
    msg_res <- findSymbolsInParsedCode(
        parsedCodeList = parsedCodes,
        symbolNames = symbols,
        tokenTypes = c("SYMBOL_FUNCTION_CALL", "SYMBOL")
    )
    unlist(msg_res)
}

checkEqInAssignment <-
    function(.BiocPackage, symbol = "=", tokenType = "EQ_ASSIGN")
{
    rfiles <- .BiocPackage$RSources
    parsedCodes <- lapply(
        structure(rfiles, .Names = rfiles), parseFile,
        .BiocPackage = .BiocPackage
    )
    msg_res <- findSymbolsInParsedCode(
        parsedCodeList = parsedCodes,
        symbolNames = symbol,
        tokenTypes = tokenType,
        fun = FALSE
    )
    unlist(msg_res)
}

.grepTokens <-
    function(ind, tokens, keywords)
{
    txt <- tokens[ind, , drop = FALSE]
    strs <- txt$token == "STR_CONST"
    ind <- ind[strs]
    txt <- txt[strs, "text"]
    filt <- grepl(paste0(keywords, collapse = "|"), txt, ignore.case = TRUE)
    ind[filt]
}

.findSignalerInSignaler <- function(rfile, symbols) {
    .findInSignaler(rfile, symbols, .grepTokens,
        keywords = c("message", "warn", "error"))
}

checkPasteInSignaler <- function(.BiocPackage) {
    rfiles <- .BiocPackage$RSources
    pasteSig <- lapply(rfiles, .findPasteInSignaler)
    pasteSig <- unlist(pasteSig)
}

checkSignalerInSignaler <- function(.BiocPackage) {
    rfiles <- .BiocPackage$RSources
    sisig <- lapply(rfiles, .findSignalerInSignaler, symbols = .SIGNALERS_TXT)
    sisig <- unlist(sisig)
}

.checkValidNEEQPattern <- function(tokens, eqnums) {
    tokens[["rowID"]] <- seq_len(nrow(tokens))
    unlist(lapply(eqnums, function(eq) {
        parnum <- tokens[eq, "parent"]
        hits <- which(tokens[, "parent"] %in% parnum)
        if (!length(hits)) { return(NULL) }
        startEQ <- min(hits)
        endEQ <- max(hits)
        EQblock <- tokens[startEQ:endEQ, ]
        hasIS <- EQblock[, "token"] == "SYMBOL_FUNCTION_CALL" &
            EQblock[, "text"] %in% c("is", "class")
        if (
            any(hasIS) &&
            "STR_CONST" %in% EQblock[EQblock[["rowID"]] > eq, "token"]
        )
            eq
        else
            NULL
    }))
}

getClassNEEQLookup <- function(rfile) {
    tokens <- getParseData(parse(rfile, keep.source = TRUE))
    eqtoks <- which(tokens[, "token"] %in% c("NE", "EQ"))
    eqtoks <- .checkValidNEEQPattern(tokens, eqtoks)
    tokens[eqtoks, , drop = FALSE]
}

checkClassNEEQLookup <- function(.BiocPackage) {
    rfiles <- .BiocPackage$RSources
    names(rfiles) <- basename(rfiles)
    NEEQ_pres <- lapply(rfiles, getClassNEEQLookup)
    NEEQ_pres <- Filter(nrow, NEEQ_pres)
    msg_neeq <- lapply(names(NEEQ_pres), function(rfile, framelist) {
        tokens <- framelist[[rfile]]
        sprintf(
            "%s (line %d, column %d)",
            rfile, tokens[, "line1"], tokens[, "col1"]
        )
    }, framelist = NEEQ_pres)
    unlist(msg_neeq)
}

checkExternalData <- function(.BiocPackage) {
    rfiles <- .BiocPackage$RSources
    msg_eda <- lapply(rfiles, function(rfile) {
        tokens <- getParseData(parse(rfile, keep.source=TRUE))
        tokens <- tokens[tokens[,"token"] == "STR_CONST", ,drop=FALSE]

        platforms <- paste0(
            "githubusercontent|github.*[^html\"]$|",
            "gitlab|bitbucket|[^\\.]dropbox"
        )
        txtkns <- tokens[, "text"]
        hits <- grepl(platforms, txtkns, ignore.case = TRUE) &
            grepl("dl|\\.\\w+\"$", txtkns)
        tokens <- tokens[hits, , drop = FALSE]

        sprintf(
            "%s (line %d, column %d)",
            basename(rfile), tokens[,"line1"], tokens[,"col1"]
        )
    })
    unlist(msg_eda)
}

checkOnAttachLoadCalls <- function(.BiocPackage) {
    rfiles <- .BiocPackage$RSources
    parsedCodes <- lapply(
        structure(rfiles, .Names = rfiles), parseFile,
        .BiocPackage = .BiocPackage
    )
    parsedCodes <- lapply(parsedCodes, function(tokens) {
        tokens <- tokens[!tokens[, "token"] %in% c("expr", "COMMENT"), ]
        incl <- .findSymbolRanges(
            tokens, c(".onLoad", ".onAttach"), "SYMBOL", TRUE
        )
        tokens[unlist(incl), ]
    })
    parsedCodes <- Filter(nrow, parsedCodes)
    msg_dl <- findSymbolsInParsedCode(
        parsedCodeList = parsedCodes,
        symbolNames = "download.*",
        tokenTypes = "SYMBOL_FUNCTION_CALL",
        FUN = .grepTokenTextCode
    )
    unlist(msg_dl)
}

checkForDirectSlotAccess <- function(parsedCode, package_name)
{
    idx <- grepl("\\.R$", names(parsedCode), ignore.case=TRUE)
    parsedCode <- parsedCode[!idx]
    res <- findSymbolInParsedCode(parsedCode, package_name, "@", "'@'")
    if (res > 0)
    {
        handleNote(
            "Use accessors; don't access S4 class slots via ",
            "'@' in examples/vignettes.")
    }
}
