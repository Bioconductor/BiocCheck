# Vignette Checks ---------------------------------------------------------

checkVignetteDir <- function(.BiocPackage)
{
    pkgdir <- .BiocPackage$sourceDir
    vigdir <- .BiocPackage$vignettesDir

    if (!dir.exists(vigdir)) {
        handleError("No 'vignettes' directory.")
        return()
    }

    vigdircontents <- .BiocPackage$VigSources
    if (!length(vigdircontents)) {
        handleError("No vignette sources in vignettes/ directory.")
        return()
    }

    checkInstContents(.BiocPackage)

    checkVigFiles(.BiocPackage)

    checkVigBuilder(.BiocPackage)

    checkVigMetadata(vigdircontents)

    checkVigTypeRNW(.BiocPackage)

    checkVigTypeQMD(.BiocPackage)

    checkVigEngine(.BiocPackage)

    checkVigSuggests(.BiocPackage)

    checkVigTemplate(vigdircontents)

    checkVigChunkEval(vigdircontents)

    checkDupChunkLabels(vigdircontents)

    checkChunkLabels(vigdircontents)

    checkVigBiocInst(.BiocPackage)

    checkVigInstalls(.BiocPackage)

    checkVigClassUsage(.BiocPackage)

    checkTFSymbolUsage(.BiocPackage)

    checkVigSessionInfo(.BiocPackage)

    checkVigEvalAllFalse(.BiocPackage)
}

checkInstContents <- function(.BiocPackage)
{
    instdocdir <- file.path(.BiocPackage$sourceDir, "inst", "doc")
    contents <- list.files(
        instdocdir,
        pattern = "\\.Rmd$|\\.Rnw$|\\.Rrst$|\\.Rhtml$|\\.Rtex$",
        ignore.case = TRUE, full.names = TRUE
    )
    if (length(contents) && .BiocPackage$isSourceDir)
        handleWarning(
            "Remove vignette sources from inst/doc; ",
            "they belong in vignettes/."
        )
}

checkVigFiles <- function(.BiocPackage) {
    vigdir <- .BiocPackage$vignettesDir
    vigdircontents <- .BiocPackage$VigSources
    vigs <- tolower(basename(vigdircontents))
    allvigfiles <- setdiff(
        tolower(
            list.files(
                vigdir, all.files = TRUE, ignore.case = TRUE, recursive = TRUE
            )
        ),
        vigs
    )

    if (length(allvigfiles) != 0) {
        badFiles <- unlist(lapply(vigs,
                           FUN = function(x, allvigfiles){
                               vl <- tools::file_path_sans_ext(x)
                               badext <- c(".tex", ".html", ".pdf",
                                           ".aux", ".log")
                               ext <- paste0(vl, badext)
                               fnd <- intersect(allvigfiles, ext)
                               fnd
                           },
                           allvigfiles = allvigfiles))
        if (length(badFiles) != 0){
            handleNote(
                "Potential intermediate files found:",
                messages = paste0("vignettes/", badFiles)
            )
        }
    }
}

checkVigBuilder <- function(.BiocPackage)
{
    builders <- .BiocPackage$VigBuilder
    builders <- builders[builders != "Sweave"]
    if (!length(builders))
        return()
    vigdircontents <- .BiocPackage$VigSources
    # check DESCRIPTION is in at least one vignette
    vigExt <- tolower(tools::file_ext(vigdircontents))
    vigdircontents <- vigdircontents[vigExt != "rnw"]

    badBuilder <- character(0)
    for (builder in builders) {
        res <- vapply(
            vigdircontents, isEngineInBuilder, logical(1), builder = builder
        )
        if (any(!res, na.rm=TRUE)) {
            badBuilder <- c(badBuilder, builder)
        }
    }
    # check if a listed builder is not found in any vignette
    if (length(badBuilder)) {
        handleError(
            "'VignetteBuilder' listed in DESCRIPTION but not ",
            "found as 'VignetteEngine' in any vignettes:",
            messages = badBuilder
        )
    }
}

checkVigTypeRNW <- function(.BiocPackage) {
    vigdircontents <- .BiocPackage$VigSources
    vigExt <- tolower(tools::file_ext(vigdircontents))
    isRNW <- vigExt == "rnw"
    vigNames <- basename(vigdircontents[isRNW])
    if (length(vigNames))
        handleWarning(
            "Use RMarkdown instead of Sweave 'Rnw' vignettes.",
            help_text = "Rnw vignette(s) found:",
            messages = vigNames
        )
}

checkVigTypeQMD <- function(.BiocPackage) {
    vigdircontents <- .BiocPackage$VigSources
    vigExt <- tolower(tools::file_ext(vigdircontents))
    isQMD <- vigExt == "qmd"
    vigNames <- basename(vigdircontents[isQMD])
    desc <- .BiocPackage$DESCRIPTION
    if (length(vigNames)) {
        if (!"SystemRequirements" %in% colnames(desc))
            handleWarning(
                "Quarto vignette found but 'SystemRequirements'",
                " field not in DESCRIPTION."
            )
        else if (!grepl("quarto", desc[, "SystemRequirements"]))
            handleWarning(
                "Quarto vignette found but 'SystemRequirements'",
                " does not list 'quarto'."
            )
    }
}

checkVigMetadata <- function(vigdircontents)
{
    badVig <- character(0)
    vigExt <- tolower(tools::file_ext(vigdircontents))
    dx <- which(!vigExt %in% c("rnw", "rhtml"))
    vigdircontents <- vigdircontents[dx]
    for (file in vigdircontents) {
        lines <- readLines(file, n=100L, warn=FALSE)
        idx <- grep(lines, pattern="vignette:")
        if (length(idx) == 0L)
            badVig <- c(badVig, basename(file))
    }
     if (length(badVig) != 0L){
        handleWarning(
            "Vignette(s) missing Vignette metadata. See ",
            "http://r-pkgs.had.co.nz/vignettes.html",
            help_text = "Update the following files:",
            messages = badVig
        )
    }
}

checkVigEngine <- function(.BiocPackage)
{
    builder <- .BiocPackage$VigBuilder
    vigdircontents <- .BiocPackage$VigSources
    # check Engines are in DESCRIPTION
    vigExt <- tolower(tools::file_ext(vigdircontents))
    vigdircontents <- vigdircontents[vigExt != "rnw"]

    # check for very rare case that multiple build
    # engines specified in vignette
    res <- lapply(vigdircontents, getVigEngine)
    if (any(lengths(res) > 1L)) {
        handleErrorFiles(
            "More than one VignetteEngine specified.",
            messages = names(which(lengths(res) > 1L))
        )
    }
    # check for missing engine
    if (any(!lengths(res))) {
        handleError(
            "No 'VignetteEngine' specified in vignette.",
            help_text = "Add 'VignetteEngine' to the following files:",
            messages = names(res[!lengths(res)])
        )
    }
    vigdircontents <- vigdircontents[lengths(res) == 1L]
    if (length(vigdircontents)) {
        res <- vapply(
            vigdircontents, isEngineInBuilder, logical(1), builder = builder
        )
        # check for missing engine in DESCRIPTION
        if (any(!res)) {
            handleError(
                "'VignetteEngine' specified but not in the DESCRIPTION.",
                help_text =
                    "Add 'VignetteEngine' to DESCRIPTION from the following:",
                messages = names(res[!res])
            )
        }
        # check for missing engine in vignette
        if (anyNA(res)) {
            nadx <- which(is.na(res))
            files <- names(res[nadx])
            if (is.null(builder))
                files <- c(files, "DESCRIPTION")
            handleError(
                "No 'VignetteEngine' specified in vignette or DESCRIPTION. ",
                help_text = paste(
                    "Add a 'VignetteEngine' to the following files or",
                    "a default 'VignetteBuilder' in DESCRIPTION: "
                ),
                messages = files
            )
        }
    }
}

checkVigSuggests <- function(.BiocPackage)
{
    builder <- .BiocPackage$VigBuilder
    vigdircontents <- .BiocPackage$VigSources
    vigExt <- tolower(tools::file_ext(vigdircontents))
    res <- lapply(vigdircontents, getVigEnginePkg)
    lst <- unique(c(unlist(unname(res)), builder))
    if (any(is.na(lst)))
        lst <- lst[!is.na(lst)]
    dep <- .BiocPackage$dependencies
    if (!all(lst %in% dep)){
        handleWarning(
            "Package listed as VignetteEngine or VignetteBuilder ",
            "but not currently Suggested. ",
            help_text = "Add the following to Suggests in DESCRIPTION:",
            messages = lst[!(lst %in% dep)]
        )
    }
}

checkVigTemplate <- function(vigdircontents)
{
    badVig <- character(0)
    badVig2 <- character(0)
    for (file in vigdircontents) {
        lines <- readLines(file, warn=FALSE)
        if (tolower(tools::file_ext(file)) %in% c("rmd", "qmd"))
            lines <- .getYAMLfront(lines)
        idx <- grep(lines, pattern="VignetteIndexEntry", fixed = TRUE)
        if (length(idx)) {
            title <- tolower(gsub(".*\\{|\\}.*", "", lines[idx]))
            if (identical(title, "vignette title"))
                badVig <- c(badVig, basename(file))
        }
        if (!length(idx))
            badVig2 <- c(badVig2, basename(file))
    }
    if (length(badVig))
        handleWarning(
            "Vignette(s) still using 'VignetteIndexEntry{{Vignette Title}}' ",
            help_text = "The following files use template defaults:",
            messages = badVig
        )
    if (length(badVig2))
        handleWarning(
            "Vignette(s) missing '\\%VignetteIndexEntry{{Vignette Title}}'. ",
            help_text = "Update the following files:",
            messages = badVig2
        )
}

detect_non_eval_chunks <- function(lines, vignetteType) {
    non_eval_pattern <- switch(
        vignetteType,
        qmd = "^[\t >]*```+\\{\\{r\\s*\\w*\\}\\}$",
        rmd = "^[\t >]*```+\\s*$",
        rnw = "\\\\begin\\{verbatim\\}",
        rhtml = "^<!--\\s*begin\\.rcode.*eval\\s*=\\s*F(ALSE)?"
    )
    non_eval_chunk_lines <- grep(non_eval_pattern, lines)
    chunk_patterns_end <- switch(
        vignetteType,
        qmd = ,
        rmd = knitr::all_patterns[["md"]]$chunk.end,
        rnw = knitr::all_patterns[["rnw"]]$chunk.end,
        rhtml = knitr::all_patterns[["html"]]$chunk.end
    )
    chunk_ends <- grep(chunk_patterns_end, lines)

    for (i in seq_along(non_eval_chunk_lines)) {
        next_end_index <-
            which(chunk_ends > non_eval_chunk_lines[i])[1L]
        if (!is.na(next_end_index))
            chunk_ends <- chunk_ends[-next_end_index]
    }

    chunk_patterns_start <- switch(
        vignetteType,
        qmd =  "^[\t >]*```+\\s*\\{r",
        rmd = knitr::all_patterns[["md"]]$chunk.begin,
        rnw = knitr::all_patterns[["rnw"]]$chunk.begin,
        rhtml = knitr::all_patterns[["html"]]$chunk.begin
    )
    chunk_starts <- grep(chunk_patterns_start, lines)

    for (i in seq_along(chunk_starts)) {
        next_end_index <- which(chunk_ends > chunk_starts[i])[1L]
        if (!is.na(next_end_index))
            chunk_ends <- chunk_ends[-next_end_index]
    }

    if (identical(vignetteType, "qmd")) {
        eval_false_lines <- grep("#\\|\\s*eval\\s*:\\s*F(ALSE)?", lines, TRUE)
    } else {
        eval_false_lines <- non_eval_chunk_lines
    }

    list(
        total = sum(length(chunk_starts), non_eval_chunk_lines),
        eval_false = sum(length(eval_false_lines), non_eval_chunk_lines),
        non_eval = sum(non_eval_chunk_lines)
    ) |>
        lapply(as.integer)
}

.CHUNKS_SENTINEL <- list(
    total = 0L,
    eval_false = 0L,
    non_eval = 0L
)

checkVigChunkEval <- function(vigdircontents)
{
    results <- lapply(
        vigdircontents,
        function(file) {
            lines <- readLines(file, warn=FALSE)
            vigExt <- tolower(tools::file_ext(file))
            if (!vigExt %in% c("rmd", "qmd", "rnw", "rhtml"))
                .CHUNKS_SENTINEL
            else
                detect_non_eval_chunks(lines, vigExt)
        }
    )

    combined <- Reduce(
        function(x, y) {
            list(
                total = x$total + y$total,
                eval_false = x$eval_false + y$eval_false,
                non_eval = x$non_eval + y$non_eval
            )
        },
        results,
        init = .CHUNKS_SENTINEL
    )

    totnon <- combined$eval_false + combined$non_eval
    percent <-
        if (!combined$total && !totnon)
            0L
        else
            as.integer((totnon * 100 / combined$total))

    if (percent >= 50) {
        handleWarning("Evaluate more vignette chunks.")
        msg <- sprintf(
            "%s out of %s code chunks = %i%% unevaluated",
            totnon,
            combined$total,
            percent
        )
        handleMessage(msg, indent = 8)
        handleMessage(
            sprintf(
                "%s non-exec code chunk(s) (e.g., '```r')",
                combined$noneval
            ),
            indent = 8
        )
    }
}

checkVigEvalAllFalse <- function(.BiocPackage) {
    vigfiles <- .BiocPackage$VigSources
    shortnames <- .getDirFiles(vigfiles)
    viglist <- structure(
        vector("logical", length(vigfiles)), .Names = shortnames
    )
    for (i in seq_along(vigfiles)) {
        shortName <- shortnames[i]
        tempR <- tempfile(fileext=".R")
        try_purl_or_tangle(input = vigfiles[i], output = tempR, quiet = TRUE)
        pfile <- parseFile(.BiocPackage, tempR)
        symbolOK <- .getTokenTextCode(
            parsedf = pfile,
            token = "SYMBOL_FUNCTION_CALL",
            text = "set",
            hasLookback = c("opts_chunk", "$")
        )
        if (nrow(symbolOK)) {
            setRange <- .findSymbolRanges(pfile, "set", "SYMBOL_FUNCTION_CALL")
            if (length(setRange) > 1L)
                warning("More than one `opts_chunk$set()` in ", vigfiles[i])
            datarange <- pfile[unlist(setRange), ]
            viglist[[shortName]] <-
                grepl("eval=F", paste(datarange$text, collapse = ""))
        }
    }
    if (any(viglist)) {
        handleWarningFiles(
            " Vignette set global option 'eval=FALSE'",
            messages = names(viglist[viglist])
        )
    }
}

checkDupChunkLabels <- function(vigfiles) {
    viglist <- structure(
        vector("logical", length(vigfiles)),
        .Names = vigfiles
    )
    for (vfile in vigfiles) {
        tempR <- tempfile(fileext=".R")
        tryCatch({
            quiet_knitr_purl(input = vfile, output = tempR, quiet = TRUE)
        }, error = function(e) {
            viglist[[vfile]] <<- grepl(
                "Duplicate chunk label", conditionMessage(e), fixed = TRUE
            )
            if (viglist[[vfile]])
                invisible(NULL)
            else
                warning(e)
        })
    }
    if (any(viglist))
        handleErrorFiles(
            " Vignette(s) found with duplicate chunk labels",
            messages = basename(vigfiles[viglist])
        )
}

.hasAllChunkLabels <- function(
    viglines, type = c("rmd", "rnw", "qmd", "rhtml")
) {
    type <- match.arg(type)
    pattern <- switch(
        type,
        qmd = ,
        rmd = "^```\\{r",
        rnw = "^<<([^,>\\s]+)",
        rhtml = "^<!--\\s*begin.rcode"
    )
    sub <- switch(
        type,
        rmd = "```\\{r\\s([^,\\}]+).*\\}",
        rnw = "^<<\\s*([\\w-]+)\\s*(?:,.*)?>>=$",
        qmd = "#\\| label:",
        rhtml = "^<!--\\s*begin\\.rcode\\s+(?![^,]*=)([a-zA-Z0-9_-]+),?\\s*"
    )
    matches <- grep(pattern, viglines, value = TRUE)
    if (!length(matches))
        return(TRUE)
    if (identical(type, "qmd")) {
        labelIdx <- grep(sub, viglines)
        length(labelIdx) >= length(matches)
    } else {
        all(
            grepl(sub, matches, perl = TRUE)
        )
    }
}

checkChunkLabels <- function(vigfiles) {
    viglist <- structure(
        vector("logical", length(vigfiles)),
        .Names = vigfiles
    )
    for (vfile in vigfiles) {
        viglines <- readLines(vfile, warn = FALSE)
        vigext <- tolower(tools::file_ext(vfile))
        viglist[[vfile]] <- .hasAllChunkLabels(viglines, type = vigext)
    }
    if (!all(viglist))
        handleNoteFiles(
            " Vignette(s) found with missing chunk labels",
            messages = basename(vigfiles[!viglist])
        )
}

.OLD_INSTALL_CALLS <-
    c("BiocInstaller", "biocLite", "useDevel", "biocinstallRepos")

checkVigBiocInst <- function(.BiocPackage) {
    msg_return <- findSymbolsInVignettes(
        .BiocPackage,
        Symbols = .OLD_INSTALL_CALLS,
        tokenTypes = c("COMMENT", "SYMBOL_FUNCTION_CALL")
    )
    if (length(msg_return)) {
        handleWarningFiles(
            " BiocInstaller code found in vignette(s)",
            messages = msg_return
        )
    }
}

checkVigInstalls <- function(.BiocPackage) {
    match_return <- findSymbolsInVignettes(
        .BiocPackage,
        Symbols = .BAD_INSTALL_CALLS,
        tokenTypes = "SYMBOL_FUNCTION_CALL"
    )
    if (length(match_return))
        handleErrorFiles(
            "Package installation calls found in vignette(s)",
            messages = unlist(match_return, use.names = FALSE)
        )
    grep_return <- findSymbolsInVignettes(
        .BiocPackage,
        Symbols = ".*install[^ed].*",
        tokenTypes = "SYMBOL_FUNCTION_CALL",
        FUN = .grepTokenTextCode
    )
    if (length(grep_return))
        handleWarningFiles(
            "Potential package installation calls found in vignette(s)",
            messages = unlist(grep_return, use.names = FALSE)
        )
}

checkTFSymbolUsage <- function(.BiocPackage) {
    viglist <- findSymbolsInVignettes(.BiocPackage, c("T", "F"), "SYMBOL")
    if (length(viglist)) {
        handleWarningFiles(
            " Avoid T/F variables; If logical, use TRUE/FALSE",
            messages = unlist(viglist, use.names = FALSE)
        )
    }
}

## Completely suppress spurious knitr:::remind_sweave() warning that shows up
## in TclTk popup window in addition to the usual text-only warning.
quiet_knitr_purl <- function(...)
{
    args <- list(...)
    callr::r(
        function(...) suppressWarnings(knitr::purl(...)),
        args = args,
        env = c(CI = "true")
    )
}

purl_or_tangle <- function(input, output, quiet, ...) {
    vigEng <- getVigEnginePkg(input)
    vigExt <- tolower(tools::file_ext(input))
    if (!identical(vigExt, "rnw") || vigEng %in% c("knitr", "quarto"))
        quiet_knitr_purl(input = input, output = output, quiet = quiet, ...)
    else
        utils::Stangle(file = input, output = output, quiet = quiet)
}

try_purl_or_tangle <- function(input, output, quiet, ...) {
    tryCatch({
        purl_or_tangle(input = input, output = output, quiet = quiet, ...)
    }, error = function(e) {
        hasDups <- grepl(
            "Duplicate chunk label", conditionMessage(e), fixed = TRUE
        )
        if (hasDups) {
            file.create(output)
            invisible(NULL)
        } else {
            stop(e)
        }
    })
}

checkVigClassUsage <- function(.BiocPackage) {
    vigfiles <- .BiocPackage$VigSources
    viglist <- structure(
        vector("list", length(vigfiles)), .Names = basename(vigfiles)
    )
    for (vfile in vigfiles) {
        tempR <- tempfile(fileext=".R")
        try_purl_or_tangle(input = vfile, output = tempR, quiet = TRUE)
        tokens <- getClassNEEQLookup(tempR)
        viglist[[basename(vfile)]] <- sprintf(
            "%s (code line %d, column %d)",
            basename(vfile), tokens[,"line1"], tokens[,"col1"]
        )
    }
    viglist <- Filter(length, viglist)
    if (length(viglist)) {
        handleWarningFiles(
            " Avoid class membership checks with class() / is() and == / !=",
            "; Use is(x, 'class') for S4 classes",
            messages = unlist(viglist, use.names = FALSE)
        )
    }
}

checkVigSessionInfo <- function(.BiocPackage) {
    vigfiles <- .BiocPackage$VigSources
    notFoundVig <- structure(
        vector("logical", length(vigfiles)), .Names = vigfiles
    )
    for (vfile in vigfiles) {
        pc <- structure(
            list(parseFile(.BiocPackage, vfile)), .Names = vfile
        )
        if (nrow(pc[[vfile]])) {
            res <- findSymbolsInParsedCode(
                parsedCodeList = pc,
                symbolNames = c("sessionInfo", "session_info"),
                tokenTypes = "SYMBOL_FUNCTION_CALL"
            )
            if (!length(res))
                notFoundVig[[vfile]] <- TRUE
        }
    }
    if (any(notFoundVig)) {
        handleNote(
            " 'sessionInfo' not found in vignette(s)",
            help_text = "Missing from file(s):",
            messages = .getDirFiles(vigfiles[notFoundVig])
        )
    }
}
