#' @importFrom graph nodes acc
#' @importFrom tools Rd2ex
#' @importFrom utils Stangle
#' @importFrom codetools walkCode findGlobals
NULL

.msg <- function(..., appendLF = TRUE, indent = 0, exdent = 2)
{
    contents <- list(...)
    txt <- if (length(contents) != 1L) do.call(sprintf, contents) else contents
    message(
        paste(strwrap(txt, indent = indent, exdent = exdent), collapse = "\n"),
        appendLF = appendLF
    )
}

.stop <- function(...) stop(noquote(sprintf(...)), call. = FALSE)

handleCondition <-
    function(
        ...,
        condition,
        help_text = character(0L),
        messages = character(0L),
        nframe = 2L
    )
{
    msg <- list(paste0(...))
    if (!tolower(condition) %in% c("warning", "error", "note"))
        stop(
            "<Internal> Designate input with 'warning', 'error', or 'note'."
        )
    cl <- sys.call(sys.parent(n = nframe))[[1L]]
    ml <- structure(msg, .Names = tail(as.character(cl), 1L))
    .BiocCheck$add(
        ml,
        condition = condition,
        help_text = help_text,
        messages = messages
    )
    .BiocCheck$log
}

#' @importFrom cli symbol
handleCheck <- function(..., appendLF = TRUE)
{
    msg <- paste0(...)
    .BiocCheck$setCheck(msg)
    if (!interactive())
        cli::cli_text(paste("*", msg))
    else
        cli::cli_progress_step(msg = msg)
}

handleError <- function(...)
{
    handleCondition(..., condition = "error")
}

handleErrorFiles <- function(..., help_text = "Found in files:") {
    handleCondition(..., help_text = help_text, condition = "error")
}

handleWarning <- function(...)
{
    handleCondition(..., condition = "warning")
}

handleWarningFiles <- function(..., help_text = "Found in files:") {
    handleCondition(..., help_text = help_text, condition = "warning")
}

handleNote <- function(...)
{
    handleCondition(..., condition = "note")
}

handleNoteFiles <- function(..., help_text = "Found in files:") {
    handleCondition(..., help_text = help_text, condition = "note")
}

handleMessage <- function(..., indent = 4, exdent = 6)
{
    msg <- paste0(...)
    cli::cli_alert_info(msg, wrap = TRUE)
}

.tryInstallwLoad <- function(.BiocPackage, install_dir = tempfile()) {
    pkgpath <- .BiocPackage$sourceDir
    pkgname <- .BiocPackage$packageName
    if (!dir.exists(install_dir))
        dir.create(install_dir)
    dir.create(libdir <- file.path(install_dir, "lib"))
    file.create(stderr <- file.path(install_dir, "install.stderr"))

    r_libs_user <- paste(c(libdir, .libPaths()), collapse = .Platform$path.sep)
    lpath <- paste0("--library=", libdir)
    res <- callr::rcmd_safe(
        "INSTALL",
        c(
            "--use-vanilla",
            lpath,
            pkgpath
        ),
        env = c(callr::rcmd_safe_env(), R_LIBS_USER = r_libs_user)
    )

    if (!identical(res[["status"]], 0L))
        handleError(pkgpath, " must be installable and loadable.")

    install_dir
}

# Takes as input the value of an Imports, Depends,
# or LinkingTo field and returns a named character
# vector of Bioconductor dependencies, where the names
# are version specifiers or blank.
cleanupDependency <- function(input, remove.R = TRUE)
{
    if (is.null(input) || !nchar(input))
        return(character(0))
    output <- gsub("\\s", "", input)
    raw_nms <- output
    nms <- strsplit(raw_nms, ",")[[1]]
    namevec <- vector(mode = "character", length(nms))
    output <- gsub("\\([^)]*\\)", "", output)
    res <- strsplit(output, ",")[[1]]
    for (i in seq_along(nms))
    {
        if (grepl(">=", nms[i], fixed = TRUE))
        {
            tmp <- gsub(".*>=", "", nms[i])
            tmp <- gsub(")", "", tmp, fixed = TRUE)
            namevec[i] <- tmp
        } else {
            namevec[i] <- ''
        }
    }
    names(res) <- namevec
    if (remove.R)
        res <- res[which(res != "R")]
    res
}

get_deprecated_status_db_url <- function(version) {
    sprintf(
        "https://bioconductor.org/checkResults/%s/bioc-LATEST/meat-index.dcf",
        version
    )
}

#' @importFrom BiocFileCache BiocFileCache bfcquery bfcneedsupdate bfcdownload
#'   bfcrpath
get_status_file_cache <- function(url) {
    cache <- tools::R_user_dir("BiocCheck", "cache")
    bfc <- BiocFileCache(cache, ask = FALSE)

    bquery <- bfcquery(bfc, url, "rname", exact = TRUE)
    if (identical(nrow(bquery), 1L) && bfcneedsupdate(bfc, bquery[["rid"]]))
        bfcdownload(x = bfc, rid = bquery[["rid"]], ask = FALSE)

    bfcrpath(
        bfc,
        rnames = url,
        exact = TRUE,
        download = TRUE,
        rtype = "web"
    )
}

.STATUS_FILE_FIELDS <- c(
    "Package",
    "Version",
    "Maintainer",
    "MaintainerEmail",
    "PackageStatus",
    "UnsupportedPlatforms"
)

.SENTINEL_PACKAGE_STATUS <- matrix(
    ncol = length(.STATUS_FILE_FIELDS),
    dimnames = list(NULL, .STATUS_FILE_FIELDS)
)

.try_read_dcf <- function(file) {
    pkg_status <- try(
        {
            read.dcf(
                file,
                all = TRUE,
                fields = .STATUS_FILE_FIELDS
            )
        },
        silent = TRUE
    )
    if (is(pkg_status, "try-error")) .SENTINEL_PACKAGE_STATUS else pkg_status
}

get_status_from_dcf <- function(status_file) {
    pkg_status <- .try_read_dcf(status_file)
    is_deprecated <- pkg_status[, "PackageStatus"] == "Deprecated" &
        !is.na(pkg_status[, "PackageStatus"])
    names(is_deprecated) <- pkg_status[, "Package"]
    is_deprecated
}

get_deprecated_status <- function(version) {
    if (version %in% c("release", "devel"))
        version <- BiocManager:::.version_bioc(version)
    status_file_url <- get_deprecated_status_db_url(version)
    status_file <- get_status_file_cache(status_file_url)
    get_status_from_dcf(status_file)
}

getAllDeprecatedPkgs <- function()
{
    ## use the more complete BiocPkgTools::biocBuildReport to identify
    ## deprecated packages rather than using the VIEWS files
    deps_release <- get_deprecated_status("release")
    deps_devel <- get_deprecated_status("devel")

    union(
        names(deps_release[deps_release]),
        names(deps_devel[deps_devel])
    )
}

.getDirFiles <- function(fpaths) {
    if (!BiocBaseUtils::isCharacter(fpaths, zchar = TRUE, na.ok = TRUE))
        stop("<internal> 'fpaths' input must be a character vector")
    vapply(
        fpaths,
        function(fpath) {
            if (nzchar(fpath) && !is.na(fpath))
                fpath <- file.path(basename(dirname(fpath)), basename(fpath))
            fpath
        },
        character(1L)
    )
}

.RdTags <- tools:::RdTags

docType <- function(rd, tags) {
    if (missing(tags))
        tags <- .RdTags(rd)
    .tagsExtract(rd, tags, "\\docType")
}

getVigEngine <- function(vignetteFile) {
    lines <- readLines(vignetteFile, n = 100L, warn = FALSE)
    vigEngine <- grep(lines, pattern = "VignetteEngine", value = TRUE)
    vigEngine <- trimws(vigEngine)
    gsub("%\\s*\\\\VignetteEngine\\{(.*)\\}", "\\1", vigEngine)
}

getVigEnginePkg <- function(vignetteFile) {
    vigEngineField <- getVigEngine(vignetteFile)
    if (length(vigEngineField))
        head(strsplit(vigEngineField, "::", fixed = TRUE)[[1L]], 1L)
    else
        NA_character_
}

isEngineInBuilder <- function(vignetteFile, builder) {
    eng <- getVigEnginePkg(vignetteFile)
    !is.na(eng) && eng %in% builder
}

.load_data <- function(dataname, package) {
    env <- new.env(parent = emptyenv())
    data(list = dataname, package = package, envir = env)
    env[[dataname]]
}

getParent <- function(view, biocViewsVocab)
{
    topLevel <- c("Software", "ExperimentData", "AnnotationData", "Workflow")
    if (view %in% topLevel)
        return(view)
    parent <- ""
    for (level in topLevel) {
        if (view %in% names(acc(biocViewsVocab, level)[[level]])) {
            parent <- level
            break
        }
    }
    parent
}

.FUNCTION_LENGTHS_SENTINEL <- list(
    data.frame(
        length = integer(0L),
        startLine = integer(0L),
        endLine = integer(0L),
        codingLines = integer(0L)
    )
)

getFunctionLengths <- function(df) {
    df <- df[df$terminal & df$parent > -1, ]

    # Identify comment-only lines
    is_comment_only_line <- df$token == "COMMENT" &
        !(duplicated(df$line1) | duplicated(df$line1, fromLast = TRUE))

    # Create a lookup table for comment-only lines
    comment_lines <- unique(df$line1[is_comment_only_line])

    all_lines <- unique(df$line1)

    rownames(df) <- NULL
    max <- nrow(df)
    res <- list()
    funcRows <- df[df$token %in% c("FUNCTION", "'\\\\'"), ]
    lst <- lapply(split(df, rownames(df)), as.list)

    if (!nrow(funcRows))
        return(.FUNCTION_LENGTHS_SENTINEL)

    for (i in seq_len(nrow(funcRows))) {
        funcRowId <- as.integer(rownames(funcRows)[i])
        funcRow <- funcRows[as.character(funcRowId), ]
        funcStartLine <- funcRow$line1 # this might get updated later
        funcLines <- NULL
        funcName <- "_anonymous_"

        # attempt to get function name
        if (funcRowId >= 3) {
            up1 <- lst[[as.character(funcRowId - 1)]]
            up2 <- lst[[as.character(funcRowId - 2)]]
            if (
                up1$token %in%
                    c("EQ_ASSIGN", "LEFT_ASSIGN", "EQ_SUB") &&
                    up2$token %in% c("SYMBOL", "SYMBOL_SUB")
            ) {
                funcName <- up2$text
                funcStartLine <- up2$line1
            }
        }

        findFunctionEnd <- function() {
            parent_level <- funcRow$parent
            last_valid_row <- NULL

            for (j in seq((funcRowId + 1), max)) {
                curr_row <- lst[[as.character(j)]]
                if (curr_row$parent > parent_level) {
                    return(j - 1)
                }
                if (curr_row$parent > 0) {
                    last_valid_row <- j
                }
                if (j == max) {
                    return(max)
                }
            }
            return(last_valid_row)
        }

        # Get end line and calculate metrics
        end_row_id <- findFunctionEnd()
        end_row <- lst[[as.character(end_row_id)]]
        endLine <- end_row$line2
        funcLines <- endLine - (funcStartLine - 1)

        # Count coding lines
        function_lines <-
            all_lines[all_lines >= funcStartLine & all_lines <= endLine]
        function_comment_lines <- comment_lines[
            comment_lines >= funcStartLine & comment_lines <= endLine
        ]
        coding_line_count <- length(
            setdiff(function_lines, function_comment_lines)
        )

        # Store results
        if (funcName == "_anonymous_")
            funcName <- paste0(funcName, ".", funcStartLine)

        res[[funcName]] <- c(
            length = funcLines,
            startLine = funcStartLine,
            endLine = endLine,
            codingLines = coding_line_count
        )
    }
    res
}

doesManPageHaveRunnableExample <- function(rd)
{
    hasExamples <-
        lapply(rd, function(x) attr(x, "Rd_tag") == "\\examples") |>
        unlist() |>
        any()

    if (!hasExamples)
        return(FALSE)

    ex <- character()
    tc <- textConnection("ex", "w", local = TRUE)
    tools::Rd2ex(rd, commentDontrun = TRUE, commentDonttest = TRUE, out = tc)
    close(tc)

    if (!length(ex))
        return(FALSE)

    parsed <- try(parse(text = ex), silent = TRUE)

    # if code contains only comments the length with be 0
    length(parsed) && !inherits(parsed, "try-error")
}

.getYAMLfront <- function(lines) {
    fm_idx <- grep("^---\\s*$", lines)
    if (length(fm_idx) && length(fm_idx) >= 2L)
        lines[seq(fm_idx[1L], fm_idx[2L])]
}

.isNULLorFALSE <- function(x) {
    is.null(x) || isFALSE(x)
}

is_valid_author_tree <- function(x) {
    if (is.null(x) || is.atomic(x))
        return(TRUE)

    if (is.symbol(x) && !nzchar(as.character(x)))
        return(TRUE)

    if (is.symbol(x))
        return(as.character(x) %in% c("c", "person"))

    if (is.call(x)) {
        func_name <-
            if (is.call(x[[1L]])) deparse(x[[1L]]) else as.character(x[[1L]])

        if (!func_name %in% c("c", "person"))
            return(FALSE)

        args <- as.list(x[-1L])
        return(
            all(
                vapply(args, is_valid_author_tree, logical(1L))
            )
        )
    }
    FALSE
}
