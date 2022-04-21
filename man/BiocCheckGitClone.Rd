\name{BiocCheckGitClone}
\alias{BiocCheckGitClone}
\title{
Checks specific to a Git clone of a package repository
}
\description{
Analyzes an R package for adherence with Bioconductor package guidelines
and best practices. The check outputs are categorized into ERROR, WARNING,
and NOTE. This function is typically used in the Bioconductor Build System
(BBS) and not intended for general use.
}
\usage{
BiocCheckGitClone(package=".", ...)
}
\arguments{
  \item{package}{
    A directory containing an R source package. Not a package tar ball.
  }
  \item{\dots}{Currently, only \code{quit-with-status} is available.
    See \code{BiocCheck}}
}

\details{
\code{BiocCheckGitClone()} reviews R packages for adherence with Bioconductor
package guidelines and best practices.
See \url{https://contributions.bioconductor.org} for the latest guidance for
writing Bioconductor software. This function should only be run on a source
directory and not on a tarball.

\code{BiocCheckGitClone} is called within R with, as
\preformatted{
    BiocCheckGitClone(<package>)
}
where \code{package} is the source directory containing the \code{R} package.
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
Lori Shepherd
}

\examples{
packageDir <- system.file("testpackages", "testpkg0", package="BiocCheck")
BiocCheckGitClone(packageDir, `quit-with-status`=FALSE)
}
