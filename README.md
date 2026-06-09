
<!-- README.md is generated from README.Rmd. Please edit that file -->

# isoreader2 <a href='https://isoreader2.isoverse.org/'> <img src="man/figures/isoreader_logo_thumb.png" align="right" width="100" alt="isoreader logo"/> </a>

<!-- badges: start -->

[![Documentation](https://img.shields.io/badge/docs-online-green.svg)](https://isoreader2.isoverse.org/)
[![R-CMD-check](https://github.com/isoverse/isoreader2/workflows/R-CMD-check/badge.svg)](https://github.com/isoverse/isoreader2/actions)
[![Codecov test
coverage](https://codecov.io/gh/isoverse/isoreader2/graph/badge.svg)](https://app.codecov.io/gh/isoverse/isoreader2)
<!-- badges: end -->

## Overview

This package provides easy access to common IRMS (isotope ratio mass
spectrometry) file formats, enabling the reading and processing of
stable isotope data directly from the data files for
platform-independent (Windows, Mac, Linux), efficient, and reproducible
data reduction.

[isoreader2](https://isoreader2.isoverse.org/) succeeds the
[isoreader](https://isoreader.isoverse.org/) package with a completely
new architecture built around the
[isoextract](https://github.com/isoverse/IsofileExtractor) command-line
tool. This makes [isoreader2](https://isoreader2.isoverse.org/)
signifcantely faster, and more versatile with support for the following
file formats:

| Extension | Measurement type | Produced by |
|----|----|----|
| [`.dxf`](https://github.com/isoverse/IsofileExtractor/blob/main/docs/isodat_structure.md) | Continuous flow | Thermo Fisher Isodat |
| [`.cf`](https://github.com/isoverse/IsofileExtractor/blob/main/docs/isodat_structure.md) | Continuous flow (legacy) | Thermo Fisher Isodat |
| [`.bch`](https://github.com/isoverse/IsofileExtractor/blob/main/docs/bch_structure.md) | Continuous flow | SerCon Callisto |
| [`.iarc`](https://github.com/isoverse/IsofileExtractor/blob/main/docs/iarc_larc_structure.md) | Continuous flow | Elementar IonOS |
| [`.larc`](https://github.com/isoverse/IsofileExtractor/blob/main/docs/iarc_larc_structure.md) | Continuous flow | Elementar LyticOS |
| [`.imexp`](https://github.com/isoverse/IsofileExtractor/blob/main/docs/imexp_structure.md)\* | Continuous flow | Thermo Fisher Qtegra |
| [`.did`](https://github.com/isoverse/IsofileExtractor/blob/main/docs/isodat_structure.md) | Dual inlet | Thermo Fisher Isodat |
| [`.caf`](https://github.com/isoverse/IsofileExtractor/blob/main/docs/isodat_structure.md) | Dual inlet (legacy) | Thermo Fisher Isodat |
| [`.scn`](https://github.com/isoverse/IsofileExtractor/blob/main/docs/isodat_structure.md) | Scan | Thermo Fisher Isodat |

> *\* the first step of reading Qtegra notebooks (extraction of the
> virtual file system) requires a Windows computer at present but we’re
> working on a solution that works on all major operating systems*

## Installation

[isoreader2](https://isoreader2.isoverse.org/) is not yet on the
Comprehensive R Archive Network (CRAN) but you can install the latest
version from [GitHub](https://github.com/isoverse/isoreader2) as shown
below. If you are on Windows, make sure to install the equivalent
version of [Rtools](https://cran.r-project.org/bin/windows/Rtools/) for
your version of R (e.g. for the latest R 4.5 and 4.6, use
[RTools4.5](https://cran.r-project.org/bin/windows/Rtools/rtools45/rtools.html) -
you can find out which version you have with `getRversion()` from an R
console).

``` r
# checks that you are set up to build R packages from source
if (!requireNamespace("pkgbuild", quietly = TRUE)) {
  install.packages("pkgbuild")
}
pkgbuild::check_build_tools()

# installs the latest isoreader2 package from GitHub
if (!requireNamespace("pak", quietly = TRUE)) {
  install.packages("pak")
}
pak::pak("isoverse/isoreader2")

# check/install isoextract
isoreader2::ir_check_isoextract()
```

## Show me some code

### Read data files

``` r
# load library
library(isoreader2)

# provide the path to your data folder here:
# for this example, we use the example files bundled with the package
data_folder <- ir_examples_folder()

# to use your own data instead, comment in this this line (remove the '#')
# and adjust the path to point to your data folder(s)
# data_folder <- file.path("project", "data")

# and search for continuous flow files (all known file types) in that folder
file_paths <- ir_find_continuous_flow(data_folder)

# for this example, we use the example files bundled with the package
# instead (remove this line if working with your own data)
file_paths <- ir_examples_folder() |> ir_find_continuous_flow()

# read the files
isofiles <- file_paths |> ir_read_isofiles()
✔ [342ms] ir_extract_isofiles() finished extracting 2 files/archives
✔ [55ms] ir_read_isofiles() finished reading 2 isotope data files/archives
# show information about the files
isofiles
─────────────────── 2 isofiles with 2 analyses - combine with ir_aggregate_isofiles() ───────────────────
1. continuous_flow_ea_example.dxf: with 1.1k time points for N2 (masses 28, 29, and 30); 1.34k time
points for CO2 (masses 44, 45, and 46); 20 metadata columns
2. continuous_flow_gc_example.cf:   with 8.6k time points for HD (masses 2 and 3); 19 metadata columns
# aggregate the data from the read files specifying which units to use
# (mV, V, nA, A, cps, etc.), conversion via resistor values happens automatically
dataset <- isofiles |> ir_aggregate_isofiles("mV")
✔ [85ms] ir_aggregate_isofiles() aggregated metadata (2) and traces (24.5k, intensity in mV) from 2
files using the standard aggregator
# show the available data that was aggregated  metadata is all the available
# sequence information from the different file types
dataset
───────────── aggregated data from 2 isofiles with 2 analyses - retrieve with ir_get_data() ─────────────
→ metadata (2): uidx, file_path, file_name, analysis, timestamp, type, h3_factor (1 NA), Row (1 NA),
Peak Center (1 NA), Check Ref. Dilution (1 NA), H3 Stability (1 NA), H3 Factor (1 NA), Amount (1 NA),
Type (1 NA), EA Method (1 NA), Identifier 1, Identifier 2 (1 NA), Analysis, Comment (1 NA), Preparation
(1 NA), Method, Line (1 NA), GC Method (1 NA), AS Sample (1 NA), AS Method (1 NA), Pre Script (all NA),
Post Script (all NA)
→ traces (24.5k): uidx, analysis, species, mass, trace, time.s, intensity.mV; (not aggregated: channel)
→ problems: has no issues
# plot the data  with the default plotting settings
dataset |> ir_plot_continuous_flow()
```

<img src="man/figures/README-unnamed-chunk-3-1.png" alt="" width="100%" />

### Export the data

``` r
# the file metadata
dataset |>
  ir_export_to_excel(
    include = c("metadata", "traces"),
    file = "my_export.xlsx"
  )
✔ [643ms] ir_export_to_excel() exported 2 rows of metadata and 24.5k rows of traces to ']8;;file:///Users/seko0922/Dropbox/Tools/software/R/isoreader2/my_export.xlsxmy_export.xlsx]8;;'
```

## Package structure

[![](man/figures/isoreader2_flowchart.svg)](https://isoreader2.isoverse.org/#package-structure)

## Getting help

If you encounter a bug, please file an issue with a minimal reproducible
example on [GitHub](https://github.com/isoverse/isoreader2/issues).
Example files are very helpful for fixing bugs so please consider
including an example data file (you will have to attach it as a zip
archive).

## isoverse <a href='http://www.isoverse.org'><img src='man/figures/isoverse_logo_thumb.png' align="right" width="100" alt="isoverse logo"/></a>

This package is part of the isoverse suite of data tools for stable
isotopes. If you like the functionality that isoverse packages provide,
please help us spread the word and include an isoverse or individual
package logo on one of your posters or slides. All logos are posted in
high resolution in [this repository](https://github.com/isoverse/logos).
If you have suggestions for new features or other constructive feedback,
please let us know on this short [feeback
form](https://www.isoverse.org/feedback/).

## Funding <a href='https://www.nsf.gov/'><img src='man/figures/NSF_logo.svg' align="right" width="100" alt="NSF logo"/></a>

This project is supported by a grant from the US National Science
Foundation
([EAR-2411458](https://www.nsf.gov/awardsearch/show-award?AWD_ID=2411458))
to Sebastian Kopf.
