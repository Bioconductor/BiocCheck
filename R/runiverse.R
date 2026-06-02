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
        )
    ruver <- results[["r"]] |> as.package_version()
    prop_cond <- ruver == rver &
        !results[["config"]] %in% c("bioc-checks", "wasm-release")

    statuses <- results[prop_cond, "check", drop = FALSE] |>
        unlist() |>
        unname()

    if (!any(statuses %in% c("ERROR", "FAIL"))) {
        handleMessage(
            "No 'ERROR' or 'FAIL' statuses in r-universe for package: ",
            pkg_name
        )
    } else {
        handleError(
            "'ERROR' or 'FAIL' status found in r-universe for package: ",
            pkg_name
        )
    }
}