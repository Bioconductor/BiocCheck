\name{BiocCheck}
\alias{BiocCheck}
\title{
Check a package for compliance with Bioconductor package guidelines
}
\description{
Analyzes \code{R} packages for compliance with Bioconductor package guidelines
and best practices. Divides output into three categories: 
ERROR, WARNING, and NOTE.
See the vignette for more details. Not meant to replace \code{R CMD check}, which should always be run first.
}
\usage{
BiocCheck(package = ".", ...)
}
\arguments{
  \item{package}{
    A directory or tarball (.tar.gz file) containing an R source package.
  }
  \item{\dots}{
    Run \code{usage()} to see the available options. When running
    \code{BiocCheck} interactively, options can be passed like so:
    \verb{BiocCheck(package, `no-check-vignettes`=TRUE)}
  }
}

\details{
\code{BiocCheck()} analyzes R packages for compliance with
Bioconductor package guidelines and best practices. For
the rationale behind these guidelines and best practices,
see the vignette and pages in the \code{references} section.
\code{usage()} displays the options that can be passed
to \code{BiocCheck()}.

\code{BiocCheck} is called within R with

\verb{BiocCheck(<package>)}

where \code{package} is a directory or .tar.gz file containing
an \code{R} source package.

\code{BiocCheck} is not meant as a replacement for 
\code{R CMD check}, which should always be run first
for best results.

See the vignette for detailed explanations of all
the checks performed by \code{BiocCheck}.

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
Dan Tenenbaum and Lori Shepherd
}

\examples{
packageDir <- system.file("testpackages", "testpkg0", package="BiocCheck")
BiocCheck(packageDir, `quit-with-status`=FALSE)

}
