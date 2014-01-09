.printf <- function(...) cat(noquote(sprintf(...)), "\n")

.BiocCheckFromCommandLine <- function()
{
    option_list <- list(
#        make_option(c("-n", "--add_numbers"), action="store_true", default=FALSE,
#        help="Print line number at the beginning of each line [default]")
        )
    parser <- OptionParser(usage = "%prog [options] package", option_list=option_list)
    arguments <- parse_args(parser, positional_arguments = 1)
    opt <- arguments$options
    file <- arguments$args

    opt$Called_from_command_line <- TRUE
    .printf("file is %s, class of opt is %s\n", file, class(opt))
    BiocCheck(file, opt)
}

BiocCheck <- function(package, ...)
{
    .printf("in BiocCheck()")
    .printf("package is %s, args are:", package)
    dots = list(...)
    print(dots)

    package_dir <- .get_package_dir(package)
}

.get_package_dir <- function(pkgname)
{
    if (!file.exists(pkgname))
    {
        stop(.printf("'%s' does not exist!", pkgname))
    }
    if (file.info(pkgname)$isdir)
        return(pkgname)

    if(!grepl("\\.tar\\.gz$", pkgname))
    {
        stop(.printf("'%s' is not a directory or package source tarball.",
            pkgname))
    }

    t = tempdir()
    res = untar(pkgname, exdir=t)
    

}