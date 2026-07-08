#' @importFrom tools file_path_sans_ext file_ext parse_Rd Rd2ex
#' @importFrom utils capture.output data getParseData head packageDescription
#' @importFrom utils packageVersion globalVariables readCitationFile
#' @importFrom stringdist stringdistmatrix
#' @importFrom knitr purl
#' @importFrom BiocManager available install repositories version
#' @import biocViews methods

# Checks for BiocCheck ----------------------------------------------------

checkDeprecatedPackages <- function(.BiocPackage)
{
    allDepends <- .BiocPackage$dependencies
    allDeprecated <- getAllDeprecatedPkgs()
    if ("multicore" %in% allDepends)
    {
        handleError("Use 'BiocParallel' instead of 'multicore'. ",
            "'multicore' is deprecated and does not work on Windows."
        )
    }
    logVec <- allDeprecated %in% allDepends
    if (any(logVec)) {
        handleError(
            "Package dependency in the DESCRIPTION is 'Deprecated'.",
            help_text = "Do not use the following package(s):",
            messages =  allDeprecated[logVec]
        )
    }
}

checkPackageSize <- function(.BiocPackage, size = 10L) {
    pkg <- .BiocPackage$sourceDir
    pkgType <- .BiocPackage$packageType
    if (is.na(pkgType) ||  pkgType == "Software") {
        mb <- 1e+6L
        maxSize <- size * mb ## in MB
        pkgSize <- file.size(pkg)
        if (pkgSize > maxSize) {
            msgs <- c(
                paste0("Package Size: ",
                       as.character(round(pkgSize / mb, 2L)), " MB"),
                paste0("Size Requirement: ",
                       sprintf("%.2f", round(maxSize / mb, 2L)), " MB")
            )
            handleError(
                "Package tarball exceeds the Bioconductor size requirement.",
                messages = msgs
            )
        }
    }
}

.MAX_FILE_SIZE <- 5e+6L ## 5MB in bytes
.DATA_DIRS <- c("data", file.path("inst", "extdata"), "data-raw")

.filter_data <- function(filedf, for_data = FALSE) {
    files <- filedf[["path"]]
    data_dirs <- paste0(.DATA_DIRS, "/")
    hitmat <- vapply(
        data_dirs,
        grepl,
        logical(length(files)),
        x = files,
        fixed = TRUE
    )
    hits <- as.logical(rowSums(hitmat))
    decision <- if (for_data) force else `!`
    filedf[decision(hits), , drop = FALSE]
}

.in_data <- function(f) { f %in% .DATA_DIRS }

.findLargeFiles <- function(.BiocPackage, data_only) {
    isSourceDir <- .BiocPackage$isSourceDir
    pkgdir <- .BiocPackage$sourceDir
    if (requireNamespace("gert", quietly = TRUE) && .BiocPackage$isGitClone) {
        fileinfo <- gert::git_ls(repo = pkgdir)
        fileinfo <- .filter_data(fileinfo, for_data = data_only)
        files <- unlist(
            fileinfo[fileinfo[["filesize"]] > .MAX_FILE_SIZE, "path"]
        )
        file.path(pkgdir, files)
    } else if (isSourceDir) {
        folders <- list.dirs(pkgdir, full.names = FALSE, recursive = TRUE)
        decision <- if (data_only) force else Negate
        folders <- Filter(decision(.in_data), folders)
        files <- list.files(
            file.path(pkgdir, folders), full.names = TRUE, recursive = TRUE
        )
        filesizes <- file.size(files)
        files[filesizes > .MAX_FILE_SIZE]
    }
}

checkIndivFileSizes <- function(.BiocPackage)
{
    largefiles <- .findLargeFiles(.BiocPackage, data_only = FALSE)
    if (length(largefiles))
        handleWarning(
            "Package files exceed the 5MB size limit.",
            help_text = "Files over the limit: ",
            messages = largefiles
        )
}

checkDataFileSizes <- function(.BiocPackage) {
    largedata <- .findLargeFiles(.BiocPackage, data_only = TRUE)
    if (length(largedata))
        handleWarning(
            "Data files exceed the 5MB size limit.",
            help_text =
                "Use 'ExperimentHub' or 'AnnotationHub' for the files: ",
            messages = largedata
        )
}

.testRbuildignore <- function(text) {
    entries <- Filter(nchar,
        grep("^#", trimws(text), value = TRUE, invert = TRUE)
    )
    grepl("^\\^?(long)?tests[\\$\\/]?$", entries)
}

checkRbuildignore <- function(.BiocPackage) {
    pkgdir <- .BiocPackage$sourceDir
    rbuildfile <- file.path(pkgdir, ".Rbuildignore")
    if (file.exists(rbuildfile)) {
        rbuild <- readLines(rbuildfile)
        testIgnore <- .testRbuildignore(rbuild)
        if (any(testIgnore))
            handleError(
                ".Rbuildignore includes 'tests' or 'longtests' folder."
            )
    }
}

checkBiocCheckOutputFolder <- function(.BiocPackage) {
    pkgdir <- .BiocPackage$sourceDir
    pkg_name <- .BiocPackage$packageName
    alldirs <- list.dirs(pkgdir, full.names = FALSE)
    bioccheck_out_dir <- paste(pkg_name, "BiocCheck", sep = ".")
    if (bioccheck_out_dir %in% alldirs)
        handleError(
            "Remove '", bioccheck_out_dir, "' from the package directory"
        )
}

checkInstDocFolder <- function(.BiocPackage) {
    instDoc <- file.path(.BiocPackage$sourceDir, "inst", "doc")
    if (dir.exists(instDoc) && length(list.files(instDoc)))
        handleError(
            "Remove 'doc' folder from the package source"
        )
}

checkBiocViews <- function(.BiocPackage)
{
    invalid <- FALSE
    handleCheck("Checking that biocViews are present...")
    views <- .BiocPackage$getBiocViews()
    if (identical(length(views), 1L) && !nzchar(views)) {
        handleError("No biocViews terms found.")
        return(TRUE)
    }

    biocViewsVocab <- .load_data("biocViewsVocab", "biocViews")

    handleCheck("Checking package type based on biocViews...")
    type <- guessPackageType(views)
    cli::cli_alert(type)
    handleCheck("Checking for non-trivial biocViews...")
    toplevel <- c("Software", "AnnotationData", "ExperimentData", "Workflow")
    if (all(views %in% toplevel)) {
        handleError(
            "Add biocViews other than ",
            paste(unique(views), collapse = ", ")
        )
        return(TRUE)
    }

    parents <- vapply(views, getParent, character(1L), biocViewsVocab)
    parents <- Filter(nzchar, parents)

    handleCheck("Checking that biocViews come from the same category...")
    if (length(unique(parents)) > 1L)
    {
        handleWarning("Use biocViews from one category only ",
            "(one of Software, ExperimentData, AnnotationData, Workflow)")
        return(TRUE)
    }
    branch <- unique(parents)

    handleCheck("Checking biocViews validity...")
    if (!all(views %in% nodes(biocViewsVocab)))
    {
        badViews <- views[!(views %in% nodes(biocViewsVocab))]

        terms <- c(badViews, nodes(biocViewsVocab))
        distmat <- stringdistmatrix(terms, useNames = "strings", method = "lv")
        distmat <- as.matrix(distmat)
        distmat <- distmat > 0L & distmat < 3L
        distmat[badViews, badViews] <- FALSE

        suggestedViews <- vapply(badViews, function(view) {
            alt <- colnames(distmat)[distmat[view, ]]
            msg <- shQuote(view)
            if (length(alt)) {
                alt <- shQuote(alt)
                oneof <- if (length(alt) > 1L) "one of" else ""
                alt <- paste(oneof, paste(alt, collapse = ", "))
                msg <- paste0(msg, ": Did you mean", alt, "?")
            }
            msg
        }, character(1L))

        handleWarning(
            "Invalid BiocViews term(s):", messages = unlist(suggestedViews)
        )
        invalid <- TRUE
    }

    if (packageVersion("biocViews") < package_version("1.33.9")) {
        ## conditional to keep the logic the same
        if (branch %in% c("Software", "AnnotationData", "ExperimentData"))
            branch <- gsub("data", "", tolower(branch))
    }

    handleCheck("Checking for recommended biocViews...")
    pkgdir <- .BiocPackage$sourceDir
    recommended <- try({
        recommendBiocViews(pkgdir, branch)[["recommended"]]
    }, silent = TRUE)

    if (!inherits(recommended, "try-error") && nzchar(recommended))
    {
        handleNote(
            "Consider adding these automatically suggested biocViews: ",
            recommended)
        invalid <- TRUE
    }
    invalid
}

.checkORCID <- function(orcid)
{
    re <- "^[0-9]{4}-[0-9]{4}-[0-9]{4}-[0-9]{3}[0-9X]$"
    grepl(re, orcid)
}

checkBBScompatibility <- function(.BiocPackage)
{
    dcf <- .BiocPackage$DESCRIPTION
    pkgdir <- .BiocPackage$sourceDir

    pkgNameFromDir <- basename(pkgdir)
    handleCheck("Checking if 'Package:' field matches directory / tarball...")
    if (dcf[, "Package"] != pkgNameFromDir)
    {
        handleError(
            "Package directory '", pkgNameFromDir, "' must match Package: ",
            "field (got '", dcf[, "Package"], "')."
        )
        return()
    }
    handleCheck("Checking for Version: field...")
    if (!"Version" %in% colnames(dcf))
    {
        handleError("No 'Version:' field in DESCRIPTION.")
        return()
    }
    validMaintainer(.BiocPackage)

    if ("Authors@R" %in% colnames(dcf)) {
        people <- .PersonsFromDCF(dcf)
        if (is.null(people) || !inherits(people, "person")) {
            handleError("Authors@R field must be valid 'person' object.")
            return()
        }
        maint <- vapply(
            people, function(person) { "cre" %in% person$role }, logical(1L)
        )
        if (sum(maint) > 1L) {
            handleError("Designate only one maintainer with Authors@R [cre].")
        }
        if (!any(maint)) {
            handleError("No Authors@R maintainer [cre] field in DESCRIPTION.")
            return()
        }
        for (person in people) {
            if ("cre" %in% person$role) {
                if (!"ORCID" %in% names(person$comment)) {
                    handleNote(
                        "Consider adding the maintainer's ORCID iD in",
                        " 'Authors@R' with 'comment=c(ORCID=\"...\")'"
                    )
                }
                email <- person$email
                if (is.null(email)) {
                    handleError(
                        "No email address for Authors@R maintainer [cre] field."
                    )
                    return()
                }
                fullname <- paste(person$given, person$family, collapse = " ")
                if (!nzchar(fullname))
                    return()
                break
            }
        }
    } else if ("Maintainer" %in% colnames(dcf)) {
        handleError("Remove Maintainer field. Use Authors@R [cre] designation.")
        return()
    } else {
        handleError(
            "No Authors@R [cre] or Maintainer field in DESCRIPTION file."
        )
        return()
    }
}

checkIsVignetteBuilt <- function(build_output_file)
{
    if (!file.exists(build_output_file))
    {
        stop("build output file '", build_output_file, "' does not exist.")
    }
    lines <- readLines(build_output_file)
    if (!any(grepl("^\\* creating vignettes \\.\\.\\.", lines)))
    {
        msg <- "Vignette must be built by
        'R CMD build'. Please see the `Vignette Checks` section of
        the BiocCheck vignette."
        handleError(msg)
    }
}

.BAD_INSTALL_CALLS <- c("biocLite", "install.packages", "install_packages",
    "update.packages", "install")

checkPkgInstallCalls <- function(.BiocPackage) {
    msg_installs <- findSymbolsInRFiles(
        .BiocPackage, .BAD_INSTALL_CALLS, "SYMBOL_FUNCTION_CALL"
    )
    if (length(msg_installs))
        handleErrorFiles(
            "Avoid using install, biocLite, install.packages,",
            " or update.packages",
            messages = msg_installs
        )
}

checkForLibraryRequire <- function(.BiocPackage) {
    msg_lib <- findSymbolsInRFiles(
        .BiocPackage,
        Symbols = c("library", "require"),
        tokenType = "SYMBOL_FUNCTION_CALL"
    )
    if (length(msg_lib)) {
        handleWarningFiles(
            " Avoid the use of 'library' or 'require' in R code",
            messages = msg_lib
        )
    }
    # for unit testing
    invisible(msg_lib)
}

.nline_report <- function(data) {
    fnoun <- "functions"
    plural <- !identical(nrow(data), 1L)
    mverb <- if (plural) "are" else "is"
    if (!plural)
        fnoun <- substr(fnoun, 1L, nchar(fnoun) - 1L)
    paste(
        "There", mverb, nrow(data), fnoun, "greater than 50 lines."
    )
}

checkFunctionLengths <- function(parsedCode, pkgname)
{
    parsedCode <- parsedCode[grepl("\\.[Rr]$", names(parsedCode))]
    if (!length(parsedCode))
        return(invisible())
    fileNames <- .getDirFiles(names(parsedCode))
    names(parsedCode) <- fileNames

    dflist <- lapply(
        fileNames,
        function(filename) {
            res <- getFunctionLengths(parsedCode[[filename]])
            functionNames <- names(res)
            if (!length(functionNames))
                return(NULL)
            mt <- do.call(rbind, res)
            cbind.data.frame(
                filename = filename,
                functionName = functionNames,
                mt,
                row.names = NULL
            )
        }
    )

    dflist <- Filter(Negate(is.null), dflist)
    if (!length(dflist))
        return(invisible())

    df <- do.call(rbind, dflist)
    if (nrow(df)) {
        df <- df[order(-df[["length"]]), ]
        h <- df[df[["length"]] > 50L, ]
        if (nrow(h)) {
            fn_msg <- apply(head(h, n = 5L), 1L, function(row) {
                sprintf(
                    "%s() (%s): %s lines",
                    row['functionName'], row['filename'], row['length']
                )
            })
            handleNote(
                "The recommended function length is 50 lines or less. ",
                .nline_report(h),
                help_text = "The longest 5 functions are:",
                messages = fn_msg
            )
        }
    }
}

checkNEWS <- function(pkgdir)
{
    newsFnd <- list.files(
        path = pkgdir, pattern = "NEWS[\\.Rd|\\.md]*$",
        recursive = TRUE, full.names = TRUE, include.dirs = FALSE
    )
    if (!length(newsFnd)) {
        handleNote(
            "Consider adding a NEWS file, so your package news will be ",
            "included in Bioconductor release announcements.")
        return()
    }
    newsPath <- file.path(basename(dirname(newsFnd)), basename(newsFnd))
    if (length(newsFnd) > 1L) {
        handleNote(
            "More than one NEWS file found.",
            "See ?news for recognition ordering.",
            help_text = "Please remove one of the following: ",
            messages = newsPath
        )
    }
    news <- head(newsFnd, 1L)
    newsext <- tools::file_ext(news)
    newsextract <- switch(
        newsext,
        Rd = tools:::.build_news_db_from_package_NEWS_Rd,
        md = tools:::.build_news_db_from_package_NEWS_md,
        tools:::.news_reader_default
    )
    tryCatch({
        res <- suppressWarnings(newsextract(news))
        if (is.null(res) || !inherits(res, "news_db"))
            stop("news() failed to parse news file: ", newsPath)
    }, error=function(e) {
        handleWarning(
            "news(package='", basename(pkgdir), "') failed with news file: ",
            newsPath, ":", e$message,
            "\nRefer to https://contributions.bioconductor.org/news.html",
            " to be included in Bioconductor release announcements."
        )
    })
}

## This could maybe be more comprehensive, but
## it's what R CMD check does to decide whether
## to run tests.
## OOPS - R CMD check is looking at the INSTALLED directory
checkUnitTests <- function(pkgdir)
{
    tests_dir <- file.path(pkgdir, "tests")
    cond <- length(dir(tests_dir, pattern = "\\.(R|Rin)$"))
    if (dir.exists(tests_dir) && (!cond))
    {
        handleError(
            "Add a .R or .Rin file in 'tests' directory or unit tests will ",
            "not be run by R CMD check. See ",
            "https://contributions.bioconductor.org/tests.html")
        return()
    }
    if (!(dir.exists(tests_dir) && cond))
    ## end stolen code
    {
        msg <- paste0(
            "Consider adding unit tests. We strongly encourage them. See",
            "\n  ",
            "https://contributions.bioconductor.org/tests.html"
        )
        handleNote(msg)
    }
}

## check if testthat contains skip_on_bioc() and throw note of it does
checkSkipOnBioc <- function(pkgdir)
{
    testdir <- file.path(pkgdir, "tests", "testthat")
    if (!file.exists(testdir))
        return()

    testfiles <- dir(testdir, pattern = "\\.[Rr]$", full.names = TRUE)
    msg <- vapply(testfiles, function(testfile) {
        tokens <- getParseData(parse(testfile, keep.source = TRUE))
        if ("skip_on_bioc" %in% unlist(tokens)) {
            basename(testfile)
        } else NA_character_
    }, character(1L))
    msg <- paste(msg[!is.na(msg)], collapse = " ")
    if (nzchar(msg)) {
        handleNote("skip_on_bioc() found in testthat files: ", msg)
    }
}

.lineReport <- function(linedf) {
    paste0(
        linedf[, "File"], "#L", linedf[, "Line"], " ",
        substr(linedf[, "Context"], 1L, 40L), " ..."
    )
}

.rmYAMLfm <- function(lines) {
    fm_idx <- grep("^---\\s*$", lines)
    offval <- 0L
    if (length(fm_idx)) {
        offval <- max(fm_idx)
        lines <- lines[-seq(min(fm_idx), offval)]
    }
    class(lines) <- c("readLines", class(lines))
    attributes(lines) <- list(offset = offval)
    lines
}

checkFormatting <- function(.BiocPackage, nlines = 6L)
{
    rfiles <- .BiocPackage$RSources
    vigfiles <- .BiocPackage$VigSources
    manfiles <- if (!.BiocPackage$usesRoxygen) .BiocPackage$manSources
    namespace <- file.path(.BiocPackage$sourceDir, "NAMESPACE")
    files <- c(rfiles, namespace, manfiles, vigfiles)

    totallines <- 0L
    ok <- TRUE
    long <- tab <- indent <- Context()

    for (file in files)
    {
        if (file.exists(file) && file.info(file)$size == 0L)
        {
            handleNote("Add content to the empty file ",
                .getDirFiles(file))
        }

        if (file.exists(file) && file.info(file)$size > 0L)
        {
            lines <- readLines(file, warn = FALSE)
            offset <- 0L
            totallines <- totallines + length(lines)

            n <- nchar(lines, allowNA = TRUE)
            idx <- !is.na(n) & (n > 80L)
            long <- rbind(long, Context(file, lines, idx, offset))

            if (tolower(tools::file_ext(file)) %in% c("rmd", "qmd")) {
                lines <- .rmYAMLfm(lines)
                offset <- attr(lines, "offset")
            }

            idx <- grepl("\t", lines)
            tab <- rbind(tab, Context(file, lines, idx, offset))

            res <- regexpr("^([ ]+)", lines)
            match.length <- attr(res, "match.length")
            idx <- (match.length != -1L) & (match.length %% 4L != 0L)
            indent <- rbind(indent, Context(file, lines, idx, offset))
        }
    }

    if (n <- nrow(long))
    {
        ok <- FALSE
        msg <- sprintf(
            "Consider shorter lines; %s lines (%i%%) are > 80 characters long.",
            n, round((n / totallines) * 100L))
        msgs <- .lineReport(long)
        handleNote(
            msg,
            help_text = "First few lines:",
            messages = msgs
        )
    }

    if (n <- nrow(tab))
    {
        ok <- FALSE
        msg <- sprintf(
            "Consider 4 spaces instead of tabs; %s lines (%i%%) contain tabs.",
            n, round((n / totallines) * 100L))
        msgs <- .lineReport(tab)
        handleNote(msg,
            help_text = "First few lines:",
            messages = msgs
        )
    }

    if (n <- nrow(indent))
    {
        ok <- FALSE
        msgs <- .lineReport(indent)
        handleNote(
            "Consider multiples of 4 spaces for line indents; ", n, " lines",
            " (", round((n / totallines) * 100L), "%) are not.",
            help_text = "First few lines:",
            messages = msgs
        )
    }

    if (!ok)
    {
        handleMessage(
            "See https://contributions.bioconductor.org/r-code.html")
        handleMessage(
            "See styler package: https://cran.r-project.org/package=styler ",
            "as described in the BiocCheck vignette.")
    }
}

checkIsPackageNameAlreadyInUse <- function(
    pkgName,
    repo = c(
        "CRAN", "BioCsoft", "BioCann", "BioCexp", "BioCworkflows", "BioCbooks"
    )
) {
    repo <- match.arg(repo)
    msg <- paste0("'", pkgName, "' already exists in Bioconductor.")

    if (identical(repo, "CRAN"))
        msg <- paste(
            "The package already exists on CRAN. Packages submitted to",
            "Bioconductor must be removed from CRAN before the next",
            "Bioconductor release."
        )

    repo.url <- switch(
        repo,
        CRAN = paste(
            BiocManager::repositories()[repo],
            "src", "contrib", "PACKAGES", sep = "/"
        ),
        BioCsoft = "https://bioconductor.org/packages/devel/bioc/VIEWS",
        BioCann =
            "https://bioconductor.org/packages/devel/data/annotation/VIEWS",
        BioCexp =
            "https://bioconductor.org/packages/devel/data/experiment/VIEWS",
        BioCworkflows =
            "https://bioconductor.org/packages/devel/workflows/VIEWS",
        BioCbooks =
            "https://bioconductor.org/packages/devel/books/VIEWS"
    )

    conn <- url(repo.url)
    on.exit({ close(conn) })
    dcf <- try(read.dcf(conn), silent = TRUE)
    if (is(dcf, "try-error"))
        return(
            handleMessage(
                "Unable to access repository ",
                BiocManager::repositories()[repo]
            )
        )

    isDuplicate <- tolower(pkgName) %in% tolower(dcf[, "Package"])
    if (isDuplicate && identical(repo, "CRAN"))
        handleWarning(msg)
    else if (isDuplicate)
        handleError(msg)
}

#' @importFrom httr2 req_body_form resp_status resp_body_html
checkForBiocDevelSubscription <- function(.BiocPackage)
{
    email <- getMaintainerEmail(.BiocPackage) |> tolower()
    if (is.null(email)) {
        handleError(
            "Unable to determine maintainer email from DESCRIPTION file.",
            nframe = 3L
        )
        return()
    }
    if (identical(email, "maintainer@bioconductor.org")) {
        handleMessage("Maintainer email is ok.")
        return()
    }
    response <- try({
        request("https://stat.ethz.ch/mailman/admin/bioc-devel") |>
            req_body_form(
                adminpw = Sys.getenv("BIOC_DEVEL_PASSWORD")
            ) |>
            req_perform()
    }, silent = TRUE)
    if (inherits(response, "try-error")) {
        handleMessage(
            "Unable to connect to the Bioc-devel mailing list:",
            "\n  ", conditionMessage(attr(response, "condition")))
        return()
    } else if (resp_status(response) >= 300L) {
        handleMessage(
            "Unable to connect to the Bioc-devel mailing list:",
            "\n  status code ", resp_status(response))
        return()
    }
    response2 <- request(
        "https://stat.ethz.ch/mailman/admin/bioc-devel/members?letter=4") |>
        req_body_form(
            findmember = email, adminpw = Sys.getenv("BIOC_DEVEL_PASSWORD")
        ) |>
        req_perform()
    content <- resp_body_html(response2)
    result_email <- unlist(
        rvest::html_table(content)[[5L]][3L, 2L], use.names = FALSE
    )
    if (identical(tolower(result_email), tolower(email))) {
        handleMessage("Maintainer is subscribed to bioc-devel.")
    } else {
        handleError(
            "Subscribe to the Bioc-devel mailing list by going to ",
            "https://stat.ethz.ch/mailman/listinfo/bioc-devel",
            nframe = 3L
        )
    }
}

checkForSupportSiteRegistration <- function(.BiocPackage)
{
    email <- getMaintainerEmail(.BiocPackage) |> tolower()
    if (is.null(email)) {
        handleError(
            "Unable to determine maintainer email from DESCRIPTION file.",
            nframe = 3L
        )
        return()
    }
    if (identical(tolower(email), "maintainer@bioconductor.org")) {
        handleMessage("Maintainer email is ok.")
        return()
    }
    accountExists <- checkSupportReg(email)

    if (accountExists) {
        pkgname <- tolower(.BiocPackage$packageName)
        checkWatchedTag(email, pkgname)
    }
}

#' @importFrom httr2 req_perform request resp_body_json req_timeout
#' @importFrom utils URLencode
checkSupportReg <- function(email) {
    response <- paste0(
        "https://support.bioconductor.org/api/email/",
        URLencode(email, reserved = TRUE)
    ) |>
        request() |>
        req_timeout(15L) |>
        req_perform() |>
        try(silent = TRUE)

    response_error <- inherits(response, "try-error")
    result <- !response_error && resp_body_json(response)
    if (response_error) {
        handleWarning(
            "Unable to retrieve email info from the Support Site:",
            "\n  ", conditionMessage(attr(response, "condition"))
        )
    } else if (resp_body_json(response)) {
        handleMessage("Maintainer is registered at support site.")
    } else {
        handleError(
            "Register your email account on the Support Site at ",
            "https://support.bioconductor.org/accounts/signup/"
        )
    }
    result
}

checkWatchedTag <- function(email, pkgname) {
    if (identical(email, "maintainer@bioconductor.org"))
        return()

    response <- paste0(
        "https://support.bioconductor.org/api/watched/tags/",
        URLencode(email, reserved = TRUE)
    ) |>
        request() |>
        req_timeout(15L) |>
        req_perform() |>
        try(silent = TRUE)

    if (inherits(response, "try-error")) {
        handleMessage(
            "Unable to retrieve 'Watched Tags' profile from the Support Site:",
            "\n  ", conditionMessage(attr(response, "condition"))
        )
    } else {
        alltags <- resp_body_json(response)[["watched_tags"]]
        taglist <- unlist(strsplit(alltags, split = ","))
        tags <- tolower(taglist)
        if (tolower(pkgname) %in% tags)
            handleMessage(
                sQuote(pkgname, FALSE),
                " is already in your 'Watched Tags' on the Support Site."
            )
        else
            handleError(
                "The ", sQuote(pkgname, FALSE), " tag must be added to your ",
                "'Watched Tags' in your Support Site profile ",
                "to receive email notifications for tagged questions: ",
                "https://support.bioconductor.org/accounts/edit/profile"
            )
    }
}
