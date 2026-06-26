## Submission

This is a new submission of isoreader2 to CRAN.

## Test environments

* local macOS, R 4.5 (R CMD check --as-cran)

## R CMD check results

0 errors | 0 warnings | 0 notes

The only note seen locally is environment-specific and does not reflect the
package ("Skipping checking HTML validation: 'tidy' doesn't look like recent
enough HTML Tidy" / "package 'V8' unavailable"); it is caused by tooling missing
from the local check machine and does not appear on the CRAN check machines.

On CRAN's incoming checks a "New submission" note is expected, as is a
"possibly mis-spelled words in DESCRIPTION" note for the product and file-format
names used in the Description field ('Isodat', 'IonOS', 'LyticOS', 'Callisto',
'Qtegra', 'isoextract', and the file extensions). These are intentional.

## Notes for the reviewers

* The package reads vendor isotope-data files. The actual binary parsing is done
  by an external command-line helper ('isoextract'); `ir_check_isoextract()`
  downloads that helper into the per-user cache directory
  (`tools::R_user_dir("isoreader2", "cache")`) only after asking for interactive
  confirmation, and aborts in non-interactive sessions. No download, write, or
  network access happens without explicit user consent.
* Examples, tests, and the vignette do not download anything and do not require
  the external helper: each bundled example file in `inst/extdata/` ships with a
  pre-extracted `.json` sidecar, so the reader runs entirely offline on CRAN.
