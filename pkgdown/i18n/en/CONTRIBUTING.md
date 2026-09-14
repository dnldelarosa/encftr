# Contributing to encftr

Report reproducible issues with synthetic data and describe expected and actual
results. Include R version, encftr version, dictionary revision, poverty
methodology and the periods involved. Keep real person and household data out of
issues and pull requests.

Change function documentation in the roxygen comments under `R/`, then regenerate
`man/`. Update paired Spanish/English guides, translation hashes and the Python
parity reference when a calculation contract changes. Add a focused regression
test for corrected behavior and run the release checks and site link checker.

Pull requests should describe the concrete calculation change, compatibility
impact and validation evidence. Model coefficient or poverty-table updates must
include documented provenance and an independent reference comparison.

See [the code of conduct](CODE_OF_CONDUCT.html) for community participation.
