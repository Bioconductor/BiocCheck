.BIOC_UNIVERSE_URL <- "https://bioc.r-universe.dev/api/packages"

## compare version in the r-universe with current package version
ru_valid_version <- function(.BiocPackage) {
    handleCheck("Checking for version number mismatch with r-universe...")

    pkg_version <- .BiocPackage$packageVersion
    pkg_name <- .BiocPackage$packageName
    bioc_ver <- BiocManager::version() |> as.character()

    ru_meta <- glue::glue(
        .BIOC_UNIVERSE_URL, "/{pkg_name}"
    ) |>
        jsonlite::fromJSON()

    mini_ver <- ru_meta[["_bioc"]]

    ru_version <- mini_ver[
        match(bioc_ver, mini_ver[["bioc"]]),
        "version"
    ] |>
        as.character()

    if (identical(pkg_version, ru_version)) {
        handleMessage(
            "Version in r-universe (", version_name,
            ") matches package version: ", pkg_version
        )
    } else {
        handleError(
            "Version mismatch with r-universe (", version_name, "): ",
            "version in r-universe: ", ru_version, "; package version: ",
            pkg_version
        )
    }
}
