.onLoad <- function(libname, pkgname)
{
    library.dynam(pkgname, pkgname, NULL)
    if (FALSE) .C()
}

.onUnload <- function(libpath)
{
    library.dynam.unload("testpkg1", libpath)
}
