@echo off

set arg0=%~dp0
shift


%R_HOME%/bin/Rscript --vanilla %arg0%BiocCheck %*

