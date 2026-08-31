# [BiocCheck](https://bioconductor.org/packages/BiocCheck) <a href='https://bioconductor.org/packages/BiocCheck/'><img src='https://raw.githubusercontent.com/Bioconductor/BiocStickers/devel/BiocCheck/BiocCheck.png' align="right" height="204" /></a>

<!-- badges: start -->
[![BioC status](http://www.bioconductor.org/shields/build/devel/bioc/BiocCheck.svg)](https://bioconductor.org/checkResults/devel/bioc-LATEST/BiocCheck)
[![Platforms](http://www.bioconductor.org/shields/availability/devel/BiocCheck.svg)](https://www.bioconductor.org/packages/devel/bioc/html/BiocCheck.html#archives)
[![Codecov test coverage](https://codecov.io/gh/Bioconductor/BiocCheck/graph/badge.svg)](https://app.codecov.io/gh/Bioconductor/BiocCheck)
[![Downloads](http://www.bioconductor.org/shields/downloads/devel/BiocCheck.svg)](https://bioconductor.org/packages/stats/bioc/BiocCheck)
<!-- badges: end -->

The `BiocCheck` package provides a set of tools for checking a package
against the current version of Bioconductor coding and style standards.

## Installation 

To install this package, start R and enter:

```r
if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager")

BiocManager::install("BiocCheck")
```

## Bioconductor Guidelines

The Bioconductor guidelines are available at
https://contributions.bioconductor.org/. This site provides the basis for
many of the checks performed by `BiocCheck`. We encourage packages to
follow these guidelines to ensure that they are of high quality and
interoperate well with other Bioconductor packages.

## Usage

To check a package, use the `BiocCheck::BiocCheck()` function. For
example, to check the `BiocCheck` package itself, use:

```r
BiocCheck::BiocCheck("BiocCheck")
```

in the directory above the source package directory. 

Note that the `BiocCheck` package must be installed to use this function.

If you are using RStudio, you can use the `BiocCheck` addin to check a
package. First, install the BiocAddins package:

```r
BiocManager::install("Bioconductor/BiocAddins")
```

Then, in RStudio, click on the "Addins" menu, and select "Run BiocCheck".

## Machine-readable output

Each run writes both `00BiocCheck.log` and `00BiocCheck.json` to the
`<PackageName>.BiocCheck` folder. The JSON report contains the text output
along with a `summary` count of the errors, warnings, and notes, an overall
`status`, and one structured record per condition raised, so that continuous
integration jobs and other tools do not have to parse the text output:

```sh
jq -e '.summary.error == 0' MyPackage.BiocCheck/00BiocCheck.json
```

See `vignette("BiocCheck")` for the schema.

## Documentation

The `BiocCheck` package contains a vignette that describes the package
in more detail. To view the vignette, start R and enter:

```r
vignette("BiocCheck")
```

