If you are submitting a pull request to `BiocCheck` please follow the instructions outlined in [this presentation](https://docs.google.com/presentation/d/1DkN2WVPOMVGqUtlSSrWbx6IMjtGP_cEHoE3nfOEnD68/edit#slide=id.p). This presentation includes steps for forking, creating a branch for you to work on, and useful related information.

Prior to sending the pull request, please verify that `R CMD build` and `R CMD check` run without warnings or errors on the latest Bioconductor-devel and complete the following steps:

* [ ] Update the NEWS file (required)
* [ ] Update the vignette file (required)
* [ ] Add unit tests (optional)

The easiest way to obtain a working environment with Bioconductor-devel in your computer is to use the Bioconductor devel docker image as described in detail [in the Bioconductor website](https://www.bioconductor.org/help/docker/).

For more questions, please get in touch with the Bioconductor core team through the [Bioconductor Slack workspace](https://bioc-community.herokuapp.com/).
