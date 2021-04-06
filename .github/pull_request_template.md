If you are submitting a pull request to `BiocCheck` please follow the
[instructions](https://docs.google.com/presentation/d/1DkN2WVPOMVGqUtlSSrWbx6IMjtGP_cEHoE3nfOEnD68/edit#slide=id.p).
The presentation includes steps for forking, creating a working branch, and
useful related information.

For a successful merge, the following steps are required:

* [ ] Update the NEWS file
* [ ] Update the vignette file
* [ ] Add unit tests (optional but highly recommended)
* [ ] Passing `R CMD build` & `R CMD check` on Bioconductor devel

List a reviewer in the Pull Request (either @LiNk-NY, @lshep, @mtmorgan).
Reviewers will make sure to `Comment`, `Approve` or `Request changes`.

We _highly_ recommend the use of the Bioconductor devel docker image described
[on the Bioconductor website](https://www.bioconductor.org/help/docker/).

If you have any questions, please get in touch with the Bioconductor core team
on the [Bioconductor Community Slack](https://bioc-community.herokuapp.com/).


