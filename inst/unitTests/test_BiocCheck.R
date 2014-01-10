test_isVignettesDirectoryPresent <- function()
{
    pkgdir <- system.file(package="BiocCheck")
    checkException(BiocCheck:::isVignettesDirectoryPresent(pkgdir))
}