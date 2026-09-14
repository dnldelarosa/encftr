# Build and deploy encftr

Build and validation steps create local artifacts. Publication is a separate
action. Install R >= 4.1, the DESCRIPTION dependencies and labeler >= 0.11.0.
The delivery contains the labeler source package and Windows binary when that
version is not yet published.

From the ENDOM workspace, run `Rscript encftr/scripts/check-release.R`. This
builds the source package with its vignettes and runs R CMD check, including
examples and tests. Install the resulting tarball in a new library and run the
bundled synthetic examples. On Windows, `R CMD INSTALL --build` creates a binary.

Run `Rscript encftr/scripts/build-docs.R` to build Spanish at the site root and
English in `en/`. Then run
`python encftr/scripts/check-sites.py artifacts/encftr-release/sites/r --kind r`
to add reciprocal language navigation and verify links, fragments and assets.
The canonical destination is https://endomer-py.github.io/encftr/.

Maintain native help through roxygen comments in `R/`. The paired guides and
English reference live in `pkgdown/i18n/en/`. Their hashes are recorded in the
translation manifest. Regenerate reviewed rules and references using the scripts
documented in the repository's deployment guide after changing the API.

Deliver source/binary packages, static sites, aggregate validation reports and
SHA256 manifests. Exclude raw surveys and intermediate person/household comparison
files. Dictionary editions, economic-table coverage and the IIH model are
independent inputs to reproducibility. GitHub workflow runs build artifacts;
the Pages workflow publishes only when explicitly requested with its publish input.

## ICV SIUBEN / encftr0

Run `Rscript encftr/scripts/export-icv-rules.R` before building the Python wheel. This exports the closed historical ICV pipelines and shared synthetic example. Install encftr 0.10.0 before the optional encftr0 0.1.0 compatibility package. See the ICV migration guide. Set `ENCFT_RELEASE_DIR` to a separate artifact directory to preserve an earlier release.
