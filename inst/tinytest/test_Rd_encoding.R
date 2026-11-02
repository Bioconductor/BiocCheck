library(tinytest)

# Use an existing namespace so these parser tests need no package installation.
test_rd_encoding <- function() {
    old_locale <- Sys.getlocale("LC_CTYPE")
    on.exit(Sys.setlocale("LC_CTYPE", old_locale), add = TRUE)
    test_dir <- tempfile()
    dir.create(test_dir)
    on.exit(unlink(test_dir, recursive = TRUE), add = TRUE)
    on.exit(BiocCheck::.BiocCheck$zero(), add = TRUE)
    manpage <- file.path(test_dir, "median.Rd")
    description <- file.path(test_dir, "DESCRIPTION")

    write_description <- function(encoding = NULL) {
        fields <- c(Package = "stats", Version = "1.0")
        if (!is.null(encoding))
            fields <- c(fields, Encoding = encoding)
        write.dcf(as.data.frame(as.list(fields)), description)
        read.dcf(description)
    }
    write_rd <- function(title, encoding = "UTF-8", rd_encoding = NULL,
                         value = TRUE, dontrun = FALSE) {
        lines <- c(
            if (!is.null(rd_encoding))
                paste0("\\encoding{", rd_encoding, "}"),
            "\\name{median}", "\\alias{median}",
            paste0("\\title{", title, "}"),
            "\\description{An encoding regression fixture.}",
            "\\usage{median(x)}",
            "\\arguments{\\item{x}{A numeric vector.}}",
            if (value) "\\value{A number.}",
            "\\examples{",
            if (dontrun) "\\dontrun{",
            "median(c(1, 2, 3))",
            if (dontrun) "}",
            "}"
        )
        text <- paste0(paste(lines, collapse = "\n"), "\n")
        writeBin(charToRaw(iconv(text, from = "UTF-8", to = encoding)), manpage)
    }
    pkg <- list(
        sourceDir = test_dir, packageName = "stats", usesRdpack = FALSE,
        manSources = setNames(manpage, "man/median.Rd")
    )
    title <- "caf\u00e9 \u2013 \u03b2"

    for (locale in unique(c(old_locale, "C"))) {
        selected_locale <- suppressWarnings(Sys.setlocale("LC_CTYPE", locale))
        if (!nzchar(selected_locale)) {
            message("Skipping unavailable LC_CTYPE locale: ", locale)
            next
        }
        pkg$DESCRIPTION <- write_description("UTF-8")
        write_rd(title)

        # Each production caller must pass DESCRIPTION Encoding to the parser.
        BiocCheck::.BiocCheck$zero()
        BiocCheck:::checkForValueSection(pkg)
        expect_equivalent(BiocCheck::.BiocCheck$getNum("warning"), 0L)
        expect_identical(
            BiocCheck:::checkExportsAreDocumented(pkg, lib.loc = .libPaths()),
            character(0)
        )
        expect_equivalent(BiocCheck::.BiocCheck$getNum("error"), 0L)
        BiocCheck:::checkUsageOfDont(pkg)
        expect_equivalent(BiocCheck::.BiocCheck$getNum("note"), 0L)
        parsed <- BiocCheck:::parseFile(pkg, manpage)
        expect_true(any(
            parsed$token == "SYMBOL_FUNCTION_CALL" & parsed$text == "median"
        ))

        rds <- BiocCheck:::.read_all_rds(
            pkg$manSources, pkg$usesRdpack,
            encoding = BiocCheck:::.rd_encoding(pkg)
        )
        expect_identical(names(rds), names(pkg$manSources))
        expect_identical(
            enc2utf8(BiocCheck:::.tagsExtract(rds[[1L]], Tag = "\\title")),
            title
        )
        # Rdpack macros are used when available, with the usual fallback otherwise.
        rd <- BiocCheck:::.parse_Rd_pack(manpage, TRUE, encoding = "UTF-8")
        expect_identical(
            enc2utf8(BiocCheck:::.tagsExtract(rd, Tag = "\\title")), title
        )

        # Parsing must still report genuine documentation problems.
        write_rd(title, value = FALSE, dontrun = TRUE)
        BiocCheck::.BiocCheck$zero()
        BiocCheck:::checkForValueSection(pkg)
        expect_equivalent(BiocCheck::.BiocCheck$getNum("warning"), 1L)
        expect_true(any(grepl(
            "Empty or missing \\value sections",
            BiocCheck::.BiocCheck$get("warning")[["checkForValueSection"]],
            fixed = TRUE
        )))
        BiocCheck:::checkUsageOfDont(pkg)
        expect_equivalent(BiocCheck::.BiocCheck$getNum("note"), 2L)
        expect_identical(
            BiocCheck:::checkExportsAreDocumented(pkg, lib.loc = .libPaths()),
            "median.Rd"
        )
        expect_equivalent(BiocCheck::.BiocCheck$getNum("error"), 1L)

        # An omitted field retains parse_Rd's native-encoding default.
        pkg$DESCRIPTION <- write_description()
        expect_identical(BiocCheck:::.rd_encoding(pkg), "unknown")
        write_rd("ASCII title")
        rd <- BiocCheck:::.parse_Rd_pack(manpage, FALSE)
        expect_identical(
            BiocCheck:::.tagsExtract(rd, Tag = "\\title"), "ASCII title"
        )
        expect_identical(
            length(BiocCheck:::.read_all_rds(pkg$manSources, FALSE)), 1L
        )
        BiocCheck::.BiocCheck$zero()
        BiocCheck:::checkForValueSection(pkg)
        expect_equivalent(BiocCheck::.BiocCheck$getNum("warning"), 0L)
        expect_identical(
            BiocCheck:::checkExportsAreDocumented(pkg, lib.loc = .libPaths()),
            character(0)
        )
        BiocCheck:::checkUsageOfDont(pkg)
        expect_equivalent(BiocCheck::.BiocCheck$getNum("note"), 0L)
        parsed <- BiocCheck:::parseFile(pkg, manpage)
        expect_true(any(
            parsed$token == "SYMBOL_FUNCTION_CALL" & parsed$text == "median"
        ))

        # Respect other declared encodings and per-file overrides.
        latin_title <- "caf\u00e9"
        pkg$DESCRIPTION <- write_description("latin1")
        write_rd(latin_title, encoding = "latin1")
        rd <- BiocCheck:::.parse_Rd_pack(
            manpage, FALSE, encoding = BiocCheck:::.rd_encoding(pkg)
        )
        expect_identical(
            enc2utf8(BiocCheck:::.tagsExtract(rd, Tag = "\\title")),
            latin_title
        )
        pkg$DESCRIPTION <- write_description("UTF-8")
        write_rd(latin_title, encoding = "latin1", rd_encoding = "latin1")
        rd <- BiocCheck:::.parse_Rd_pack(
            manpage, FALSE, encoding = BiocCheck:::.rd_encoding(pkg)
        )
        expect_identical(
            enc2utf8(BiocCheck:::.tagsExtract(rd, Tag = "\\title")),
            latin_title
        )
    }
}

test_rd_encoding()
