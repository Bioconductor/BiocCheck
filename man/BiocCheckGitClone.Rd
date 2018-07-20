\name{BiocCheckGitClone}
\alias{BiocCheckGitClone}
\alias{usage2}
\title{
Additional checks for a package for compliance with Bioconductor package guidelines
}
\description{
Analyzes \code{R} packages for compliance with Bioconductor package guidelines
and best practices. Divides output into three categories:
ERROR, WARNING, and NOTE.
This function is mostly meant to be called from the operating system's
command line (via \code{R CMD BiocCheckGitClone}). Not meant
to replace \code{R CMD check} or \code{R CMD BiocCheck}, which should always be run first.
}
\usage{
BiocCheckGitClone(package=".", ...)
usage2()
}
\arguments{
  \item{package}{
    A directory containing an R source package. Not a package tar ball.
  }
  \item{\dots}{Only available option currently is quit-with-status.
    See \code{BiocCheck}}
}


\details{
\code{BiocCheckGitClone()} analyzes R packages for compliance with
Bioconductor package guidelines and best practices. This function should
only be run on a open source directory not a tarball.

\code{BiocCheckGitClone} is typically called from the operating system's
command line, as

\verb{R CMD BiocCheckGitClone package}

where \code{package} is a directory containing an \code{R} source package.

\code{BiocCheckGitClone} is not meant as a replacement for
\code{R CMD check} or \code{R CMD BiocCheck}, which should always be run first
for best results.

Installing \code{BiocCheck} will attempt to install
the \code{BiocCheckGitClone} script, which could fail; details
in the vignette.

See the vignette for detailed explanations of all
the checks performed by \code{BiocCheckGitClone}.

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
\url{http://www.bioconductor.org/developers/how-to/coding-style/}
\url{http://www.bioconductor.org/developers/package-guidelines/}
\url{http://www.bioconductor.org/developers/how-to/version-numbering/}
\url{http://www.bioconductor.org/developers/how-to/unitTesting-guidelines/}
}
\author{
Lori Shepherd
}

\examples{
packageDir <- system.file("testpackages", "testpkg0", package="BiocCheck")
BiocCheckGitClone(packageDir, `quit-with-status`=FALSE)

## or from the operating system's command line as:
## R CMD BiocCheckGitClone myPackage

}
