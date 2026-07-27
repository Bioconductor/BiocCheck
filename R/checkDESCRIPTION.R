checkReadDESCRIPTION <- function(.BiocPackage) {
    if (!.BiocPackage$isValid) {
        handleError("DESCRIPTION is malformed.")
        handleMessage(.BiocPackage$readError)
    }
}

checkDescFieldLength <- function(dcf) {
    handleCheck("Checking for proper Description: field...")

    if ("Description" %in% colnames(dcf)) {
        desc_field <- dcf[, "Description"]
        desc_words <- lengths(strsplit(desc_field, split = "[[:space:]]+"))
        desc_sentences <- length(
            strsplit(desc_field, split = "[.!?][[:space:]]+")[[1L]]
        )
        msg <- "The Description field in the DESCRIPTION is made up of less
            than 3 sentences. Provide a more detailed description of the
            package."

        # values chosen sensibly in a data-driven manner
        if (nchar(desc_field) < 50L || desc_words < 20L)
            handleWarning(
                "Description field in the DESCRIPTION file is too concise"
            )
        else if (desc_sentences < 3L)
            handleNote(paste(strwrap(msg), collapse = "\n"))
    }
}

checkWSinDESCfields <- function(dcf) {
    handleCheck("Checking for whitespace in DESCRIPTION field names...")
    if (any(grepl("\\s", colnames(dcf))))
        handleError("Remove whitespace from DESCRIPTION field names.")
}

validMaintainer <- function(.BiocPackage) {
    if (.BiocPackage$isTar)
        return()
    dcf <- .BiocPackage$DESCRIPTION
    authr <- "Authors@R" %in% colnames(dcf)
    autmain <- c("Author","Maintainer") %in% colnames(dcf)
    if (authr && any(autmain))
        handleError(
            "Use either 'Authors@R' or 'Author' and 'Maintainer' fields. ",
            "Do not use both."
        )
    else if (any(autmain))
        handleNote(
            "Consider using 'Authors@R' over the 'Author' and ",
            "'Maintainer' fields."
        )
}

checkDESCRIPTIONFile <- function(.BiocPackage) {
    handleCheck("Checking DESCRIPTION readability...")
    if (!.BiocPackage$isValid)
        .stop(
            "DESCRIPTION file is malformed. Cannot check DESCRIPTION fields."
        )

    dcf <- .BiocPackage$DESCRIPTION

    checkValidDESCfields(dcf)

    checkLicenseForRestrictiveUse(dcf[, "License"])
    checkRecDESCfields(dcf)

    checkWSinDESCfields(dcf)
    checkDescFieldLength(dcf)
    checkBiocDepsDESC(dcf)
    checkPinnedDeps(dcf)
    checkFndPerson(dcf)
}

checkRemotesUsage <- function(.BiocPackage)
{
    dcf <- .BiocPackage$DESCRIPTION
    restricted_fields <- c("Remotes", "Additional_repositories")
    if (any(restricted_fields %in% colnames(dcf)))
        handleError(
            "Package dependencies must be on CRAN or Bioconductor.",
            " Remove 'Remotes:' and/or 'Additional_repositories:' fields",
            " from DESCRIPTION"
        )
}

checkNewPackageVersionNumber <- function(.BiocPackage)
{
    dcf <- .BiocPackage$DESCRIPTION
    version <- dcf[, "Version"]
    if (!grepl("^0+[-.][0-9]+[-.][0-9]+$", version))
        handleWarning(
            "New package x version starting with non-zero value ",
            "(e.g., 1.y.z, 2.y.z); got ", sQuote(version), ".")
    if (!grepl("^[0-9]+[-.]99[-.][0-9]+$", version))
        handleError(
            "New package 'y' version not 99 (i.e., x.99.z)",
            "; Package version: ", version
        )
}

checkForVersionNumberMismatch <- function(.BiocPackage)
{
    if (!.BiocPackage$isTar)
        return()

    handleCheck("Checking for version number mismatch...")
    tarfilename <- .BiocPackage$tarFilename
    ver <- tail(unlist(strsplit(tarfilename, "_|\\.tar\\.[xg]z")), 1L)
    dcf <- .BiocPackage$DESCRIPTION
    dcfVer <- unname(dcf[, "Version"])
    if (!identical(ver, dcfVer))
    {
        handleError(
            "Version number in tarball filename must match Version field ",
            "in DESCRIPTION. (Tip: create tarball with R CMD build)")
    }
}

checkLazyDataUsage <- function(.BiocPackage)
{
    dcf <- .BiocPackage$DESCRIPTION
    if ("LazyData" %in% colnames(dcf) &&
        tools:::str_parse_logic(dcf[, "LazyData"]))
        handleNote(
            "'LazyData:' in the 'DESCRIPTION' should be set to false or removed"
        )
}

checkVersionNumber <- function(.BiocPackage)
{
    version <- .BiocPackage$packageVersion
    regex <- "^[0-9]+[-\\.]([0-9]+)[-\\.][0-9]+$"
    if(!grepl(regex, version))
    {
        handleError(
            "Invalid package Version, see ",
            "https://contributions.bioconductor.org/versionnum.html"
        )
        return()
    }
    tryCatch({
        pv <- package_version(version)
    }, error = function(e) {
        handleError(conditionMessage(e))
    })
    x <- pv$major
    y <- pv$minor
    mod <- y %% 2
    isDevel <- identical(
        BiocManager:::.version_bioc("devel"), BiocManager::version()
    )
    bioc.mod <- as.numeric(isDevel)
    if (identical(x, 0L)) {
        handleMessage("Package version ", as.character(pv), "; pre-release")
    } else if (mod != bioc.mod) {
        shouldBe <- ifelse(isDevel, "odd", "even")
        vers <- ifelse(isDevel, "devel", "release")
        handleWarning(
            "y of x.y.z version should be ", shouldBe, " in ", vers
        )
    }
}

.PersonsFromDCF <- function(
    dcf,
    field = c("Authors@R", "Author"),
    .dreturn = NULL
) {
    field <- match.arg(field)
    if (identical(field, "Author"))
        return(dcf[, field])
    env <- new.env(parent = emptyenv())
    env[["c"]] <- c
    env[["person"]] <- utils::person
    pp <- parse(text = dcf[, field], keep.source = FALSE)
    if (.is_valid_author_tree(pp[[1L]])) {
        eval(pp, env)
    } else {
        handleError("Malformed 'Authors@R' field in DESCRIPTION")
        return(.dreturn)
    }
}

.MainEmailAuthorsAtR <- function(dcf) {
    email <- NULL
    people <-
        .PersonsFromDCF(dcf, "Authors@R", .dreturn = NULL)
    for (person in people) {
        if ("cre" %in% person$role) {
            email <- person$email
            break
        }
    }
    email
}

.MainEmailMaintainer <- function(dcf) {
    res <- unname(
        gsub(".*<(.*)>", "\\1", dcf[, "Maintainer"])
    )
    if (!nzchar(res)) NULL else res
}

pullMaintainerEmail <- function(pkgpath) {
    desc <- file.path(pkgpath, "DESCRIPTION")
    stopifnot(file.exists(desc))
    dcf <- read.dcf(desc)
    if ("Maintainer" %in% colnames(dcf))
        .MainEmailMaintainer(dcf)
    else if ("Authors@R" %in% colnames(dcf))
        .MainEmailAuthorsAtR(dcf)
}

getMaintainerEmail <- function(.BiocPackage)
{
    # Eventually update this to just look at Authors@R
    # Since the intention is to possible start running
    # this on the daily builder, leave Maintainer field
    # check. This is used to check for mailing list registration
    dcf <- .BiocPackage$DESCRIPTION
    if ("Maintainer" %in% colnames(dcf))
        .MainEmailMaintainer(dcf)
    else if ("Authors@R" %in% colnames(dcf))
        .MainEmailAuthorsAtR(dcf)
}

checkRVersionDependency <- function(.BiocPackage) {
    dcf <- .BiocPackage$DESCRIPTION
    if ("Depends" %in% colnames(dcf)) {
        res <- cleanupDependency(dcf[, "Depends"], FALSE)
        verStr <- names(res)[res == "R"]
        if (isScalarCharacter(verStr)) {
            pkgVer <- package_version(verStr)
            RVer <- package_version(
                paste0(BiocManager:::.version_field("R"), ".0")
            )
            if (pkgVer < RVer)
                handleNote(
                    "Update R version dependency from ", pkgVer, " to ", RVer
                )
        }
    }
}

.LICENSE_DB_LOCATION <- "$R_HOME/share/licenses/license.db"

checkLicenseForRestrictiveUse <- function(license) {
    handleCheck("Checking License: for restrictive use...")

    if (!identical(length(license), 1L) || is.na(license)) {
        handleNote("malformed 'License:' field '", license, "'")
        return(invisible())
    }
    ldb_file <- file.path(R.home("share"), "licenses", "license.db")
    if (!file.exists(ldb_file)) {
        handleNote(
            "license database not found. ",
            "Expected location: '", ldb_file, "'. ",
            "License: '", license, "'"
        )
        return(invisible())
    }
    licenses <- read.dcf(ldb_file)
    result <- tools:::analyze_licenses(license, licenses)
    test <- result[["restricts_use"]]
    if (isTRUE(test))
        handleError("License '", license, "' restricts use")
    else if (is.na(test) || !result[, "is_verified"]) {
        handleNote(
            "License '", license, "' unknown; refer to ", .LICENSE_DB_LOCATION
        )
        handleMessage(
            "and https://choosealicense.com/appendix/ for more info.",
            indent = 6L
        )
    }
}

checkRecDESCfields <- function(dcf) {
    handleCheck("Checking for recommended DESCRIPTION fields...")
    fields <- c("URL", "BugReports")
    if ("Date" %in% colnames(dcf)) {
        date <- dcf[, "Date"]
        if (!grepl("^\\d{4}-\\d{2}-\\d{2}$", date))
            handleNote("'Date:' field format is not 'YYYY-MM-DD': ", date)
    }
    present <- fields %in% colnames(dcf)
    res <- fields[!present]
    if (length(res)) {
        notFields <- paste(shQuote(res), collapse = ", ")
        handleNote("Provide ", notFields, " field(s) in DESCRIPTION")
    }
}

checkBiocDepsDESC <- function(dcf, which = c("Depends", "Imports")) {
    handleCheck("Checking for Bioconductor software dependencies...")
    which_fields <- dcf[, colnames(dcf) %in% which]
    all_deps <- unlist(
        lapply(which_fields, function(x) strsplit(x, ",\\s*")[[1L]]),
        use.names = FALSE
    )
    all_deps <- gsub("(\\w+)\\s+\\(.*\\)$", "\\1", all_deps)
    all_deps <- all_deps[all_deps != "R"]
    repo <- BiocManager:::.repositories_bioc(BiocManager::version())["BioCsoft"]
    biocdb <- utils::available.packages(repos = repo)
    bioc_deps <- all_deps %in% rownames(biocdb)
    percent <- unname(round(prop.table(table(bioc_deps))["TRUE"], 2L) * 100)

    if (!any(bioc_deps)) {
        views <- .BiocPackage$getBiocViews()
        handleFUN <-
            if ("Infrastructure" %in% views) handleNote else handleWarning
        msg <- "No Bioconductor dependencies detected. Note that some
            infrastructure packages may not have Bioconductor dependencies.
            For more information, reach out to the Bioconductor community
            and/or consider a CRAN submission."
        handleFUN(msg)
    } else {
        handleMessage(
            "Bioconductor dependencies found in Imports & Depends (",
            percent,
            "%)."
        )
    }
}

checkPinnedDeps <- function(dcf) {
    handleCheck("Checking for pinned package versions in DESCRIPTION...")
    deps <- c("Depends", "Imports", "Suggests", "Enhances", "LinkingTo")
    validdeps <- deps[deps %in% colnames(dcf)]
    doubleeq <- grepl("==", dcf[, validdeps], fixed = TRUE)
    if (any(doubleeq))
        handleError("Dependencies in the DESCRIPTION file contain '=='")
}

checkFndPerson <- function(dcf) {
    handleCheck("Checking for 'fnd' role in Authors@R...")
    field <- if ("Authors@R" %in% colnames(dcf)) "Authors@R" else "Author"
    people <- .PersonsFromDCF(dcf, field, .dreturn = "")
    msg <- paste(
        "No 'fnd' role found in 'Authors@R'. If the work is supported by a",
        "grant, consider adding the 'fnd' role to the list of authors."
    )
    if (!any(grepl("fnd", people, fixed = TRUE)))
        handleMessage(msg)
}

checkValidDESCfields <- function(dcf) {
    handleCheck("Checking validity of DESCRIPTION fields...")

    present_fields <- colnames(dcf)

    known_fields <- c(
        tools:::.get_standard_DESCRIPTION_fields(),
        c("RoxygenNote", "Video")
    )

    is_config_field <- grepl("^Remote|Config/", present_fields)
    fields_to_check <- present_fields[!is_config_field]

    bad_fields <- fields_to_check[!(fields_to_check %in% known_fields)]

    if (length(bad_fields)) {
        terms <- c(bad_fields, known_fields)
        distmat <- stringdist::stringdistmatrix(
            terms, useNames = "strings", method = "lv"
        )
        distmat <- as.matrix(distmat)

        distmat <- distmat > 0L & distmat < 3L
        distmat[bad_fields, bad_fields] <- FALSE

        suggestedFields <- vapply(
            bad_fields,
            function(field) {
                alt <- colnames(distmat)[distmat[field, ]]
                msg <- shQuote(field)
                if (length(alt)) {
                    alt <- shQuote(alt)
                    oneof <- if (length(alt) > 1L) "one of" else ""
                    alt <- paste(oneof, paste(alt, collapse = ", "))
                    msg <- paste0(msg, ": Did you mean ", alt, "?")
                }
                msg
            },
            character(1L)
        )

        handleWarning(
            "Unknown or non-standard DESCRIPTION field(s):",
            messages = suggestedFields
        )
    }
}
