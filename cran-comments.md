## Submission

This is a resubmission of a new package. It addresses the reviewer feedback:

* Removed the single quotes around the software and file-format names in the
  Description field.
* Added a reference describing the methods to the Description field:
  Kopf et al. (2021) <doi:10.21105/joss.02878>.
* Replaced `\dontrun{}` in the examples: `ir_copy_examples()` is now unwrapped
  (it runs in < 5s, writing to a temporary directory) and `ir_export_to_excel()`
  uses `\donttest{}` (it needs the suggested 'openxlsx' package and writes to a
  temporary directory).
* The package no longer installs any packages: the previous automatic
  `install.packages()` calls for the suggested 'openxlsx' and 'arrow' packages
  were removed; the functions now error with a message asking the user to
  install the package themselves.

## Test environments

* Local MacOS, R 4.5 (R CMD check --as-cran)
* Mac OS X 26.4 (on GitHub), R 4.6.0 (release)
* Ubuntu 24.04 (on GitHub), R 4.6.0 (release)
* Windows Server 2025 (on GitHub), R 4.6.0 (release)
* Win-builder (release, devel, and oldrelease)

## R CMD check results

0 errors | 0 warnings | 1 note

This is a new submission.
