if (FALSE)
{
    library(testpkg0)
    require(testpkg0)
    library("testpkg0")
    require("testpkg0")
    library(package="testpkg0")
    library(package=testpkg0)
    require(package="testpkg0")
    require(package=testpkg0)
    require(package = teskpkg0)
    require('testpkg0')
    library('testpkg0')
    require(lib.loc=NULL, 
        package=testpkg0)
    #bad:
    require(help=testpkg0)
    require(lib.loc=NULL, help = 
    testpkg0)
}