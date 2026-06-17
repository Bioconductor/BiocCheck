.BIOC_UNIVERSE_URL <- "https://bioc.r-universe.dev/api/packages"

.get_runi_meta <- function(.BiocPackage) {
    pkg_name <- .BiocPackage$packageName
    glue::glue(
        .BIOC_UNIVERSE_URL, "/{pkg_name}"
    ) |>
        jsonlite::fromJSON()
}

.get_ru_version <- function(.BiocPackage) {
    ru_meta <- .get_runi_meta(.BiocPackage)
    mini_ver <- ru_meta[["_bioc"]]
    if (is.null(mini_ver)) {
        handleError(
            "No Bioconductor information in r-universe for package: ", pkg_name
        )
        return(invisible(NULL))
    }

    bioc_ver <- BiocManager::version() |> as.character()
    matched_ver <- match(bioc_ver, mini_ver[["bioc"]])

    if (is.na(matched_ver)) {
        handleError(
            "No version in r-universe matches Bioconductor version: ",
            bioc_ver
        )
        return(invisible(NULL))
    }

    mini_ver[matched_ver, "version"] |>
        as.character()
}

## compare version in the r-universe with current package version
ru_valid_version <- function(.BiocPackage) {
    handleCheck("Checking for version number mismatch with r-universe...")

    pkg_version <- .BiocPackage$packageVersion
    version_name <- BiocManager:::.version_field("BiocStatus") |>
        as.character()
    ru_version <- .get_ru_version(.BiocPackage)

    if (identical(pkg_version, ru_version)) {
        handleMessage(
            "Version in r-universe (", version_name,
            ") matches package version: ", pkg_version
        )
    } else {
        handleError(
            "Package version mismatch with r-universe (", version_name, "): ",
            "r-universe version: ", ru_version, "; package version: ",
            pkg_version
        )
    }
}

.check_incremental <- function(version1, version2) {
    version1 <- as.package_version(version1)
    version2 <- as.package_version(version2)

    if (version1 <= version2)
        return(FALSE)

    version2[, 3L] <- as.numeric(version2[, 3L]) + 1L

    identical(
        version1, version2
    )
}

.check_major_increment <- function(version1, version2) {
    version1 <- as.package_version(version1)
    version2 <- as.package_version(version2)

    if (version1 <= version2)
        return(FALSE)

    version1[, 2L] == '99' &&
        identical(version1[, 1L], version2[, 1L])
}

check_ru_ver_bump <- function(.BiocPackage) {
    handleCheck(
        "Checking for valid version bump compared to r-universe version..."
    )

    pkg_version <- .BiocPackage$packageVersion
    version_name <- BiocManager:::.version_field("BiocStatus") |>
        as.character()
    ru_version <- .get_ru_version(.BiocPackage)

    .check_incremental(pkg_version, ru_version) ||
        .check_major_increment(pkg_version, ru_version) ||
            handleError(
                "Version bump is not valid compared to r-universe (",
                version_name, "): ",
                "r-universe version: ", ru_version, "; package version: ",
                pkg_version
            )
}

.filter_unsupported <- function(results, desc) {
    desc_field <- "Config/Bioconductor/UnsupportedPlatforms"
    fields <- colnames(desc)
    if (!desc_field %in% fields)
        return(results)
    unsupplat <- desc[, desc_field]
    plats <- strsplit(unsupplat, ",\\s+")[[1L]] |>
        gsub("macosx", "macos", x = _)
    unsupported <- lapply(
        plats, startsWith, x = results[["config"]]
    ) |>
        Reduce(`|`, x = _)
    results[!unsupported, , drop = FALSE]
}

.filter_other_checks <- function(results) {
    other_checks <- c("bioc-checks", "wasm-release")
    results[!results[["config"]] %in% other_checks, , drop = FALSE]
}

.filter_r_ver <- function(results) {
    rver <- BiocManager:::.version_field("R")
    rver[, 3L] <- 0L
    results[results[["r"]] == rver, , drop = FALSE]
}

check_ru_status <- function(.BiocPackage) {
    pkg_name <- .BiocPackage$packageName
    bioc_ver <- BiocManager::version() |> as.character()

    rver <- BiocManager:::.version_field("R")
    rver[, 3L] <- 0L

    results <- glue::glue(
        .BIOC_UNIVERSE_URL, "/{pkg_name}"
    ) |>
        rjsoncons::j_pivot(
            path = "_jobs[]", as = "data.frame"
        ) |>
        .filter_r_ver() |>
        .filter_other_checks() |>
        .filter_unsupported(.BiocPackage$DESCRIPTION)

    statuses <- results[["check"]] |>
        unlist() |>
        unname()

    if (!any(statuses %in% c("ERROR", "FAIL", "CANCELLED"))) {
        handleMessage(
            "No 'ERROR', 'FAIL', or 'CANCELLED' status in r-universe",
            " for package: ", pkg_name
        )
    } else {
        handleError(
            "'ERROR', 'FAIL', or 'CANCELLED' status found in r-universe",
            "for package: ", pkg_name
        )
    }
}
