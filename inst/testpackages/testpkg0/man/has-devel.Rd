\name{has_devel}
\alias{has_devel}
\title{Check if you have a development environment installed.}
\usage{
has_devel()
}
\value{
TRUE if your development environment is correctly set up,
otherwise returns an error.
}
\description{
Thanks to the suggestion of Simon Urbanek.
}
\examples{
    foo_path <- file.path(tempdir(), "foo.c")

    cat("void foo(int *bar) { *bar=1; }\n", file = foo_path)
    on.exit(unlink(foo_path))

    devtools:::R("CMD SHLIB foo.c", tempdir())
    dylib <- file.path(tempdir(), paste("foo", .Platform$dynlib.ext, sep=''))
    on.exit(unlink(dylib), add = TRUE)

    dll <- dyn.load(dylib)
    on.exit(dyn.unload(dylib), add = TRUE)

    stopifnot(.C(dll$bar, 0L)[[1]] == 1L)


}

