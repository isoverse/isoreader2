## Submission

This is a patch update of an existing package (0.7.0 -> 0.7.1).

It makes all metadata timestamps consistently UTC across file formats and fixes
a few bugs (reading and printing files from aborted acquisitions that contain no
data, and printing aggregated data with problems in narrow consoles). There are
no user-facing API changes. The changes are summarized at the top of NEWS.md.

## Test environments

* Local MacOS, R 4.5.2 (R CMD check --as-cran)
* Mac OS X 26.4 (on GitHub), R 4.6.1 (release)
* Ubuntu 24.04 (on GitHub), R 4.6.1 (release, devel, and oldrelease)
* Windows Server 2025 (on GitHub), R 4.6.1 (release)
* Win-builder (release, devel, and oldrelease)

## R CMD check results

0 errors | 0 warnings | 0 notes

## Reverse dependencies

- isoexplorer - no breaking changes
