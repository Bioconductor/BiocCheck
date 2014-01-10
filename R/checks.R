setRefClass("MsgClass",
    fields=list(
        name="character"
    )
)


getVigSources <- function(dir)
{
    dir(dir,
        pattern=glob2rx(c("*.Rnw", "*.Rmd", "*.Rrst")),
        ignore.case=TRUE)
}

checkVignetteDir <- function(pkgdir)
{
    vigdir <- file.path(pkgdir, "vignettes")
    instdocdir <- file.path(pkgdir, "inst", "doc")
    if (!file.exists(vigdir))
        handleError("No 'vignettes' directory!")
    vigdircontents <- getVigSources(vigdir)
    if (length(vigdircontents)==0)
    {
        handleError("No vignette sources in vignettes/ directory.")
    }
    instdocdircontents <- getVigSources(instdocdir)
    if (length(instdocdircontents) > 0)
    {
        handleWarning("Vignette sources exist in inst/doc/; they belong in vignettes/.")
    }


}