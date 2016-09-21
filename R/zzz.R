.MsgClass <- setRefClass("MsgClass",
    fields=list(
        msg="character"
    ),
    methods=list(
        get = function() .self$msg,
        getNum = function() length(.self$msg),
        zero = function() .self$msg <- character(0),
        add = function(m) .self$msg <- append(.self$msg, m)
        )
)

## singletons. Exported but 'hidden' from ls() by the '.'

.error <- .MsgClass()

.warning <- .MsgClass()

.note <- .MsgClass()

.zeroCounters <- function()
{
    .error$zero()
    .warning$zero()
    .note$zero()
}

.isScriptInstalled <- function()
{
    if (nchar(Sys.which("BiocCheck")))
        return(TRUE)

    onWindows <- (.Platform$OS.type == "windows")

    if (onWindows)
        file <- "BiocCheck.bat"
    else
        file <- "BiocCheck"

    path <- file.path(Sys.getenv("R_HOME"), "bin")

    all(file.exists(file.path(path, file)))

}

.onLoad <- function(libname, pkgname)
{
    if (.isScriptInstalled())
        return()
    .installScript()
}
