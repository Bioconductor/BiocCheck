checkNAMESPACE <- function(.BiocPackage) {
    handleCheck("Checking NAMESPACE...")
    namespace <- file.path(.BiocPackage$sourceDir, "NAMESPACE")
    ns_lines <- readLines(namespace, warn = FALSE)
    hasEP <- grepl(
        'exportPattern.*\\[\\[:alpha:\\]\\]', ns_lines
    )
    if (any(hasEP)) {
        handleError(
            "'NAMESPACE' contains exportPattern with '[[:alpha:]]'; ",
            "use explicit exports instead."
        )
    }
}
