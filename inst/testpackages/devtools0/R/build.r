#' Build package.
#'
#' Building converts a package source directory into a single bundled file.
#' If \code{binary = FALSE} this creates a \code{tar.gz} package that can
#' be installed on any platform, provided they have a full development
#' environment (although packages without source code can typically be
#' install out of the box). If \code{binary = TRUE}, the package will have
#' a platform specific extension (e.g. \code{.zip} for windows), and will
#' only be installable on the current platform, but no development
#' environment is needed.
#'
#' @param pkg package description, can be path or package name.  See
#'   \code{\link{as.package}} for more information
#' @param path path in which to produce package.  If \code{NULL}, defaults to
#'   the parent directory of the package.
#' @param binary Produce a binary (\code{--binary}) or source (
#'   \code{--no-manual --no-resave-data}) version of the package.
#' @param vignettes For source packages: if \code{FALSE}, don't build PDF
#'   vignettes (\code{--no-vignettes}).
#' @param args An optional character vector of additional command
#'   line arguments to be passed to \code{R CMD build} if \code{binary = FALSE},
#'   or \code{R CMD install} if \code{binary = TRUE}.
#' @param quiet if \code{TRUE} suppresses output from this function.
#' @export
#' @family build functions
#' @return a string giving the location (including file name) of the built
#'  package
build <- function(pkg = ".", path = NULL, binary = FALSE, vignettes = TRUE,
                  args = NULL, quiet = FALSE) {
  pkg <- as.package(pkg)
  if (is.null(path)) {
    path <- dirname(pkg$path)
  }

  compile_rcpp_attributes(pkg)

  if (binary) {
    args <- c("--build", args)
    cmd <- paste0("CMD INSTALL ", shQuote(pkg$path), " ",
      paste0(args, collapse = " "))
    ext <- if (.Platform$OS.type == "windows") "zip" else "tgz"
  } else {
    args <- c(args, "--no-manual", "--no-resave-data")

    if (!vignettes) {
      args <- c(args, "--no-vignettes")

    } else if (!nzchar(Sys.which("pdflatex"))) {
      message("pdflatex not found. Not building PDF vignettes.")
      args <- c(args, "--no-vignettes")
    }

    cmd <- paste0("CMD build ", shQuote(pkg$path), " ",
      paste0(args, collapse = " "))

    ext <- "tar.gz"
  }
  with_libpaths(c(tempdir(), .libPaths()), R(cmd, path, quiet = quiet))
  targz <- paste0(pkg$package, "_", pkg$version, ".", ext)

  file.path(path, targz)
}


#' Build windows binary package.
#'
#' This function works by bundling source package, and then uploading to
#' \url{http://win-builder.r-project.org/}.  Once building is complete you'll
#' receive a link to the built package in the email address listed in the
#' maintainer field.  It usually takes around 30 minutes. As a side effect,
#' win-build also runs \code{R CMD check} on the package, so \code{build_win}
#' is also useful to check that your package is ok on windows.
#'
#' @param pkg package description, can be path or package name.  See
#'   \code{\link{as.package}} for more information
#' @param version directory to upload to on the win-builder, controlling
#'   which version of R is used to build the package. Possible options are
#'   listed on \url{http://win-builder.r-project.org/}. Defaults to the
#'   released version of R.
#' @param quiet if \code{TRUE} suppresses output from this function.
#' @importFrom RCurl ftpUpload
#' @export
#' @family build functions
build_win <- function(pkg = ".", version = "R-release", quiet = FALSE) {
  pkg <- as.package(pkg)
  if (!quiet) message("Building windows version of ", pkg$package,
    " with win-builder.r-project.org.\n")

  built_path <- build(pkg, tempdir(), quiet = quiet)
  on.exit(unlink(built_path))

  ftpUpload(built_path, paste("ftp://win-builder.r-project.org/", version,
    "/", basename(built_path), sep = ""))

  message("Check your email for a link to the built package in 30-60 mins.")
  invisible()
}
