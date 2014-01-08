#!/usr/bin/env Rscript --vanilla



srcFile <- file.path("inst", "script", "BiocCheck")
destDir <- file.path(Sys.getenv("R_HOME"), "bin")
destFile <- file.path(destDir, "BiocCheck")
alreadyExists <- file.exists(destFile)
## FIXME complain if we can't copy, suggest an alternative
res <- file.copy(srcFile, destDir, overwrite<-TRUE)
if (!res)
{

}
destFile <- file.path(destDir, "BiocCheck")
res <- Sys.chmod(destFile, "0755")
if (!res)
{

}
