\name{BiocCheck}
\alias{BiocCheck}
\title{
Check a package's adherence with the Bioconductor Package Guidelines
}
\description{
Analyzes an R package for adherence with Bioconductor package guidelines
and best practices. The check outputs are categorized into ERROR, WARNING,
and NOTE. See the vignette for more details. \code{BiocCheck} is complementary
to \code{R CMD check}, which should always be run first.
}
\usage{
BiocCheck(package = ".", ...)
}
\arguments{
  \item{package}{
    A directory or tarball (\code{.tar.gz} file) of an R package.
  }
  \item{\dots}{
    See the details section for available options. When running
    \code{BiocCheck}, options can be specified as:
    \verb{BiocCheck(package, `no-check-vignettes`=TRUE)}
  }
}

\details{
\code{BiocCheck()} reviews R packages for adherence with Bioconductor package
guidelines and best practices. See \url{https://contributions.bioconductor.org}
for the latest guidance for writing Bioconductor software.
Some rationale behind these best practices can be seen in the vignette and
pages in the \code{references} section. The vignette also provides detailed
explanations of all the checks performed by \code{BiocCheck}.

\code{BiocCheck} is called within R with
\preformatted{
    BiocCheck(<package>)
}
where \code{package} points to the source directory or the \code{.tar.gz}
tarball that was created using \code{R CMD build}.

\emph{Note} that \code{BiocCheck} is complementary to \code{R CMD check}.
\code{R CMD check} should always be run first for best results.

\subsection{... options} {
    \describe{
        \item{new-package}{ enable checks specific to new packages}
        \item{no-check-dependencies}{ disable check for bad dependencies}
        \item{no-check-deprecated}{ disable check for usage of deprecated packages}
        \item{no-check-remotes}{ disable check for usage of remote packages other than those hosted on CRAN or Bioconductor}
        \item{no-check-version-num}{ disable check for valid version number}
        \item{no-check-R-ver}{ disable check for valid R version}
        \item{no-check-pkg-size}{ disable check for package tarball size}
        \item{no-check-file-size}{ disable check for individual file size}
        \item{no-check-bioc-views}{ disable biocViews-specific checks (for non-BioC packages)}
        \item{no-check-bbs}{ disable BBS-specific checks (for non-BioC packages). Valid DESCRIPTION}
        \item{no-check-description}{ disable DESCRIPTION file checks}
        \item{no-check-namespace}{ disable namespace checks}
        \item{no-check-vignettes}{ disable vignette checks}
        \item{no-check-library-calls}{ disable check usage of functions that install or update packages}
        \item{no-check-install-self}{ disable check for require or library of itself}
        \item{no-check-coding-practices}{ disable check for some common best coding practices}
        \item{no-check-function-len}{ disable check for function length}
        \item{no-check-man-doc}{ disable checks for man page documentation}
        \item{no-check-news}{ disable checks for NEWS file}
        \item{no-check-unit-tests}{ disable checks for unit tests}
        \item{no-check-skip-bioc-tests}{ disable check for tests that skip when on bioc}
        \item{no-check-formatting}{ disable checks for file formatting}
        \item{no-check-CRAN}{ disable check for if package exists in CRAN}
        \item{no-check-bioc-help}{ disable check for registration on Bioconductor mailing list and support site}
        \item{build-output-file}{ file containing R CMD build output, for additional analysis}
        \item{quit-with-status}{ enable exit code option when performing check}
    }
}

}
\value{
Mostly called for the side effect of the information displayed.
When called interactively, returns a list with three components:

\item{ERROR}{Items that must be fixed before the package
can be accepted into Bioconductor.}

\item{WARNING}{We strongly suggest fixing these items.}

\item{NOTE}{Fixing these items is not required, there is no expectation
  that considerations will escalate in severity.}
}
\references{
\url{https://contributions.bioconductor.org}
}
\author{
Dan Tenenbaum, Lori Shepherd, and Marcel Ramos
}
\examples{
packageDir <- system.file("testpackages", "testpkg0", package="BiocCheck")
BiocCheck(packageDir, `quit-with-status`=FALSE)
}
