## Submission

This is a minor update of an existing package (0.6.1 -> 0.7.0).

It adds a new filtering function (`ir_filter_masses()`), gives the `mass` and
`ratio` arguments of the plotting functions full tidyselect support, renames
`ir_plot_continuous_flow()` to `ir_plot_traces()` (the old name is kept as a
soft-deprecated alias), and fixes a few bugs. The changes are summarized at the top of NEWS.md.

## Test environments

* Local MacOS, R 4.5.2 (R CMD check --as-cran)
* Mac OS X 26.4 (on GitHub), R 4.6.0 (release)
* Ubuntu 24.04 (on GitHub), R 4.6.0 (release)
* Windows Server 2025 (on GitHub), R 4.6.0 (release)
* Win-builder (release, devel, and oldrelease)

## R CMD check results

0 errors | 0 warnings | 0 notes

## Reverse dependencies

- isoexplorer - no breaking changes
