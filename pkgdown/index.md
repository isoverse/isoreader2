---
output: github_document
params:
  generating_pkgdown_index: false
---

<!-- README.md is generated from README.Rmd. Please edit that file -->



# isoreader2 <a href='https://isoreader2.isoverse.org/'> <img src="man/figures/isoreader_logo_thumb.png" align="right" width="100" alt="isoreader logo"/> </a>

<!-- badges: start -->
  [![Documentation](https://img.shields.io/badge/docs-online-green.svg)](https://isoreader2.isoverse.org/)
  [![R-CMD-check](https://github.com/isoverse/isoreader2/workflows/R-CMD-check/badge.svg)](https://github.com/isoverse/isoreader2/actions)
  [![Codecov test coverage](https://codecov.io/gh/isoverse/isoreader2/graph/badge.svg)](https://app.codecov.io/gh/isoverse/isoreader2)
<!-- badges: end -->

## Overview

This package provides easy access to common IRMS (isotope ratio mass spectrometry) file formats, enabling the reading and processing of stable isotope data directly from the data files for platform-independent (Windows, Mac, Linux), efficient, and reproducible data reduction.

[isoreader2](https://isoreader2.isoverse.org/) succeeds the [isoreader](https://isoreader.isoverse.org/) package with a completely new architecture built around the [isoextract](https://github.com/isoverse/IsofileExtractor) command-line tool. This makes [isoreader2](https://isoreader2.isoverse.org/) signifcantely faster, and more versatile with support for the following file formats:


| Extension | Measurement type | Produced by | 
|-----------|-----------------|-------------|
| [`.dxf`](https://github.com/isoverse/IsofileExtractor/blob/main/docs/isodat_structure.md)     | Continuous flow          | Thermo Fisher Isodat |
| [`.cf`](https://github.com/isoverse/IsofileExtractor/blob/main/docs/isodat_structure.md)      | Continuous flow (legacy) | Thermo Fisher Isodat |
| [`.bch`](https://github.com/isoverse/IsofileExtractor/blob/main/docs/bch_structure.md)        | Continuous flow          | SerCon Callisto      |
| [`.iarc`](https://github.com/isoverse/IsofileExtractor/blob/main/docs/iarc_larc_structure.md) | Continuous flow          | Elementar IonOS      | 
| [`.larc`](https://github.com/isoverse/IsofileExtractor/blob/main/docs/iarc_larc_structure.md) | Continuous flow          | Elementar LyticOS    | 
| [`.imexp`](https://github.com/isoverse/IsofileExtractor/blob/main/docs/imexp_structure.md)*   | Continuous flow          | Thermo Fisher Qtegra | 
| [`.did`](https://github.com/isoverse/IsofileExtractor/blob/main/docs/isodat_structure.md)     | Dual inlet               | Thermo Fisher Isodat |
| [`.caf`](https://github.com/isoverse/IsofileExtractor/blob/main/docs/isodat_structure.md)     | Dual inlet (legacy)      | Thermo Fisher Isodat | 
| [`.scn`](https://github.com/isoverse/IsofileExtractor/blob/main/docs/isodat_structure.md)     | Scan                     | Thermo Fisher Isodat | 

> *\* the first step of reading Qtegra notebooks (extraction of the virtual file system) requires a Windows computer at present but we're working on a solution that works on all major operating systems*

## Installation

[isoreader2](https://isoreader2.isoverse.org/) is not yet on the Comprehensive R Archive Network (CRAN) but you can install the latest version from [GitHub](https://github.com/isoverse/isoreader2) as shown below. If you are on Windows, make sure to install the equivalent version of [Rtools](https://cran.r-project.org/bin/windows/Rtools/) for your
version of R (e.g. for the latest R 4.5 and 4.6, use [RTools4.5](https://cran.r-project.org/bin/windows/Rtools/rtools45/rtools.html) - you can find out which version you have with `getRversion()` from an R console).

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




``` r
# load library
library(isoreader2)

# load data
dataset <-
  ir_examples_folder() |>
  ir_find_continuous_flow() |>
  ir_read_isofiles() |>
  ir_aggregate_isofiles("mV")

# visualize data
dataset |>
  ir_plot_continuous_flow()
```

<div class="figure">
<img src="man/figures/README-continuous_flow_example-1.png" alt="Plot of continuous flow examples" width="100%" />
<p class="caption">Plot of continuous flow examples</p>
</div>

## Show me more details



### Read isotope data files


``` r
# load library
library(isoreader2)

# specify where the data files are located (relative or absolute path)
data_folder <- "tmp/project/data"

# search for dual inlet files (all known file types) in that folder
# (or use ir_find_continuous_flow or ir_find_scans instead)
file_paths <- ir_find_dual_inlet(data_folder)

# for this example, we use the example files bundled with the package
# instead (remove this line if working with your own data)
file_paths <- ir_examples_folder() |> ir_find_dual_inlet()

# read the files
isofiles <- file_paths |> ir_read_isofiles()
```

<PRE class="fansi fansi-message"><CODE><span style='color: #00BB00;'>✔</span> <span style='color: #B2B2B2;'>[307ms]</span> <span style='font-weight: bold;'>ir_extract_isofiles()</span> finished extracting 1 file/archive
</CODE></PRE><PRE class="fansi fansi-message"><CODE><span style='color: #00BB00;'>✔</span> <span style='color: #B2B2B2;'>[260ms]</span> <span style='font-weight: bold;'>ir_read_isofiles()</span> finished reading 1 isotope data file/archive
</CODE></PRE>

``` r
# show information about the files
isofiles
```

<PRE class="fansi fansi-message"><CODE>─────── <span style='font-weight: bold;'>1 isofile with 1 analysis - process with ir_aggregate_isofiles()</span> ───────
</CODE></PRE><PRE class="fansi fansi-message"><CODE>1. <span style='color: #0000BB;'>caf_dual_inlet_example.caf</span>: with <span style='color: #00BBBB;'>8</span> sample/standard cycles for <span style='color: #BB00BB;'>CO2clump</span>
(masses <span style='color: #00BB00;'>44</span>, <span style='color: #00BB00;'>45</span>, <span style='color: #00BB00;'>46</span>, <span style='color: #00BB00;'>47</span>, <span style='color: #00BB00;'>48</span>, <span style='color: #00BB00;'>49</span>, <span style='color: #00BB00;'>44</span>, <span style='color: #00BB00;'>45</span>, <span style='color: #00BB00;'>46</span>, <span style='color: #00BB00;'>47</span>, <span style='color: #00BB00;'>48</span>, <span style='color: #00BB00;'>49</span>, <span style='color: #00BB00;'>44</span>, <span style='color: #00BB00;'>45</span>, <span style='color: #00BB00;'>46</span>, <span style='color: #00BB00;'>47</span>, <span style='color: #00BB00;'>48</span>, <span style='color: #00BB00;'>49</span>,
…, <span style='color: #00BB00;'>48</span>, and <span style='color: #00BB00;'>49</span>); <span style='color: #00BBBB;'>21</span> metadata columns
</CODE></PRE>

### Aggregate the data


``` r
# aggregate the data from the read files specifying which units to use
# (mV, V, nA, A, cps, etc.), conversion via resistor values happens automatically
dataset <- isofiles |> ir_aggregate_isofiles("V")
```

<PRE class="fansi fansi-message"><CODE><span style='color: #00BB00;'>✔</span> <span style='color: #B2B2B2;'>[45ms]</span> <span style='font-weight: bold;'>ir_aggregate_isofiles()</span> aggregated <span style='color: #0000BB;'>metadata</span> (1) and <span style='color: #0000BB;'>cycles</span> (102,
<span style='color: #00BB00;'>intensity</span> in <span style='color: #BB00BB;'>V</span>) from 1 file using the <span style='font-weight: bold; font-style: italic;'>standard</span> aggregator
</CODE></PRE>

``` r
# show the available data that was aggregated  metadata is all the available
# sequence information from the different file types
dataset
```

<PRE class="fansi fansi-message"><CODE>─ <span style='font-weight: bold;'>aggregated data from 1 isofiles with 1 analysis - retrieve with ir_get_data(</span> ─
</CODE></PRE><PRE class="fansi fansi-message"><CODE>→ <span style='color: #0000BB;'>metadata</span> (1): <span style='color: #00BB00;'>uidx</span>, <span style='color: #00BB00;'>file_path</span>, <span style='color: #00BB00;'>file_name</span>, <span style='color: #00BB00;'>analysis</span>, <span style='color: #00BB00;'>timestamp</span>, <span style='color: #00BB00;'>type</span>,
<span style='color: #00BB00;'>h3_factor</span> (<span style='color: #BBBB00;'>all NA</span>), <span style='color: #00BB00;'>Line</span>, <span style='color: #00BB00;'>Peak Center</span>, <span style='color: #00BB00;'>Pressadjust</span>, <span style='color: #00BB00;'>Background</span>, <span style='color: #00BB00;'>Reference</span>
<span style='color: #00BB00;'>Refill</span>, <span style='color: #00BB00;'>Weight [mg]</span>, <span style='color: #00BB00;'>Sample</span>, <span style='color: #00BB00;'>Identifier 1</span>, <span style='color: #00BB00;'>Identifier 2</span>, <span style='color: #00BB00;'>Analysis</span>, <span style='color: #00BB00;'>Comment</span>,
<span style='color: #00BB00;'>Preparation</span>, <span style='color: #00BB00;'>Pre Script</span>, <span style='color: #00BB00;'>Post Script</span>, <span style='color: #00BB00;'>Method</span>
</CODE></PRE><PRE class="fansi fansi-message"><CODE>→ <span style='color: #0000BB;'>cycles</span> (102): <span style='color: #00BB00;'>uidx</span>, <span style='color: #00BB00;'>analysis</span>, <span style='color: #00BB00;'>species</span>, <span style='color: #00BB00;'>cycle</span>, <span style='color: #00BB00;'>type</span>, <span style='color: #00BB00;'>mass</span>, <span style='color: #00BB00;'>trace</span>, <span style='color: #00BB00;'>intensity.V</span>;
(<span style='font-style: italic;'>not aggregated</span>: <span style='color: #BBBB00; font-style: italic;'>channel</span>)
</CODE></PRE><PRE class="fansi fansi-message"><CODE>→ <span style='color: #0000BB;'>problems</span>: has <span style='color: #00BB00;'>no issues</span>
</CODE></PRE>

### Visualize the data


``` r
# filter the data by a metadata field and mass range and plot it
# (use ir_plot_continuous_flow() and ir_plot_scans(), respectively)
library(ggplot2)
dataset |>
  ir_filter_metadata(file_name == "caf_dual_inlet_example") |>
  ir_plot_dual_inlet(mass = c(44:48)) +
  # use ggplot2 to modify the plot with custom theming (or any other ggplot elements)
  theme(strip.text = element_text(size = 30))
```

<div class="figure">
<img src="man/figures/README-dual_inlet_example-1.png" alt="Plot of dual inlet examples" width="100%" />
<p class="caption">Plot of dual inlet examples</p>
</div>

### Export the data


``` r
# get the data of interest (here, metadata and dual inlet cycles data)
# and export both into one excel file (one sheet per data set)
ir_export_to_excel(
  metadata = dataset |> ir_get_metadata(),
  cycles = dataset |> ir_get_cycles(),
  file = "my_export.xlsx"
)
```

<PRE class="fansi fansi-message"><CODE><span style='color: #00BB00;'>✔</span> <span style='color: #B2B2B2;'>[2ms]</span> <span style='font-weight: bold;'>ir_get_data()</span> retrieved 1 records from <span style='color: #0000BB;'>metadata</span>
</CODE></PRE><PRE class="fansi fansi-message"><CODE><span style='color: #00BB00;'>✔</span> <span style='color: #B2B2B2;'>[3ms]</span> <span style='font-weight: bold;'>ir_get_data()</span> retrieved 102 records from the combination of <span style='color: #0000BB;'>metadata</span>
(1) and <span style='color: #0000BB;'>cycles</span> (102) via <span style='color: #00BB00;'>uidx</span> and <span style='color: #00BB00;'>analysis</span>
</CODE></PRE><PRE class="fansi fansi-message"><CODE><span style='color: #00BB00;'>✔</span> <span style='color: #B2B2B2;'>[191ms]</span> <span style='font-weight: bold;'>ir_export_to_excel()</span> exported 1 row of <span style='color: #00BB00;'>metadata</span> and 102 rows of
<span style='color: #00BB00;'>cycles</span> to <span style='color: #0000BB;'>my_export.xlsx</span>
</CODE></PRE>



## Package structure

<p>Click on the individual functions to jump straight to their documenation.</p>

```{=html}
<?xml version="1.0" encoding="UTF-8"?>
<svg id="Layer_1" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" version="1.1" viewBox="0 0 765.22 959.06">
  <!-- Generator: Adobe Illustrator 30.5.1, SVG Export Plug-In . SVG Version: 2.1.4 Build 3)  -->
  <defs>
    <style>
      .st0 {
        fill: #010101;
      }

      .st1 {
        stroke-dasharray: 6 6;
      }

      .st1, .st2, .st3, .st4, .st5, .st6, .st7, .st8, .st9, .st10, .st11, .st12 {
        stroke-miterlimit: 10;
      }

      .st1, .st2, .st3, .st4, .st5, .st6, .st7, .st8, .st9, .st10, .st12 {
        stroke: #231f20;
      }

      .st1, .st2, .st5, .st6, .st7, .st8, .st9, .st10, .st11, .st12 {
        fill: none;
      }

      .st1, .st5, .st6, .st10, .st12 {
        stroke-width: 2px;
      }

      .st2 {
        stroke-dasharray: 6;
        stroke-width: 2.36px;
      }

      .st13, .st14, .st15, .st16 {
        fill: #231f20;
      }

      .st17 {
        fill: #e2e2e2;
      }

      .st3 {
        stroke-width: 1.53px;
      }

      .st3, .st4 {
        fill: #cdacd1;
      }

      .st4 {
        stroke-width: 2.66px;
      }

      .st18 {
        font-size: 9px;
      }

      .st18, .st14, .st19, .st20, .st15, .st16, .st21 {
        isolation: isolate;
      }

      .st18, .st20, .st15, .st21 {
        font-weight: 700;
      }

      .st18, .st20, .st21 {
        font-family: Arial-BoldMT, Arial;
      }

      .st14, .st15 {
        font-size: 19.94px;
      }

      .st14, .st16 {
        font-family: ArialMT, Arial;
      }

      .st6 {
        stroke-dasharray: 8.01;
      }

      .st20 {
        font-size: 8px;
      }

      .st22 {
        fill: #fff;
      }

      .st7 {
        stroke-width: 3px;
      }

      .st8 {
        stroke-width: 2.49px;
      }

      .st9 {
        stroke-width: 4px;
      }

      .st15 {
        font-family: Arial-BoldItalicMT, Arial;
        font-style: italic;
      }

      .st10 {
        stroke-dasharray: 6.98;
      }

      .st11 {
        stroke: #000;
        stroke-width: .2px;
      }

      .st16 {
        font-size: 17px;
      }

      .st23 {
        fill: #5ebb55;
      }

      .st21 {
        font-size: 7px;
      }

      .st24 {
        fill: #5f88c6;
      }

      .st12 {
        stroke-dasharray: 7.04 7.04;
      }
    </style>
  </defs>
  <rect class="st22" width="765.22" height="959.06"/>
  <g>
    <g>
      <rect class="st23" x="459.17" y="714.99" width="200.29" height="41.92" rx="4.23" ry="4.23"/>
      <rect class="st6" x="459.17" y="714.99" width="200.29" height="41.92" rx="4.23" ry="4.23"/>
    </g>
    <path class="st0" d="M473.49,735.95l1.12-10.98c.08-.77.61-1.35,1.25-1.35h6.61c.59,0,1.06.58,1.06,1.3,0,.15-.02.31-.07.46l-1.81,5.94h5.46c.79,0,1.44.79,1.44,1.77,0,.36-.09.7-.25,1l-7.53,13.53c-.23.41-.61.66-1.01.66h-.11c-.62,0-1.12-.62-1.12-1.37,0-.11,0-.22.04-.33l1.84-9.08h-5.64c-.69,0-1.25-.69-1.25-1.54h-.03,0Z"/>
    <g class="st19">
      <g class="st19">
        <g class="st19">
          <g class="st19">
            <g class="st19">
              <g class="st19">
                <g class="st19">
                  <g class="st19">
                    <text class="st16" transform="translate(498.03 740.48)"><tspan x="0" y="0">ir_filter</tspan></text>
                  </g>
                </g>
              </g>
            </g>
          </g>
        </g>
      </g>
    </g>
  </g>
  <g>
    <g>
      <rect class="st23" x="459.17" y="763.11" width="200.29" height="41.92" rx="4.23" ry="4.23"/>
      <rect class="st6" x="459.17" y="763.11" width="200.29" height="41.92" rx="4.23" ry="4.23"/>
    </g>
    <path class="st0" d="M473.49,784.07l1.12-10.98c.08-.77.61-1.35,1.25-1.35h6.61c.59,0,1.06.58,1.06,1.3,0,.15-.02.31-.07.46l-1.81,5.94h5.46c.79,0,1.44.79,1.44,1.77,0,.36-.09.7-.25,1l-7.53,13.53c-.23.41-.61.66-1.01.66h-.11c-.62,0-1.12-.62-1.12-1.37,0-.11,0-.22.04-.33l1.84-9.08h-5.64c-.69,0-1.25-.69-1.25-1.54h-.03,0Z"/>
    <g class="st19">
      <g class="st19">
        <g class="st19">
          <g class="st19">
            <g class="st19">
              <g class="st19">
                <g class="st19">
                  <g class="st19">
                    <text class="st16" transform="translate(498.03 788.6)"><tspan x="0" y="0">ir_join_metadata</tspan></text>
                  </g>
                </g>
              </g>
            </g>
          </g>
        </g>
      </g>
    </g>
  </g>
  <g>
    <g>
      <rect class="st23" x="9.22" y="620.58" width="234.17" height="41.92" rx="4.23" ry="4.23"/>
      <rect class="st6" x="9.22" y="620.58" width="234.17" height="41.92" rx="4.23" ry="4.23"/>
    </g>
    <path class="st0" d="M23.54,641.54l1.12-10.98c.08-.77.61-1.35,1.25-1.35h6.61c.59,0,1.06.58,1.06,1.3,0,.15-.02.31-.07.46l-1.81,5.94h5.46c.79,0,1.44.79,1.44,1.77,0,.36-.09.7-.25,1l-7.53,13.53c-.23.41-.61.66-1.01.66h-.11c-.62,0-1.12-.62-1.12-1.37,0-.11,0-.22.04-.33l1.84-9.08h-5.64c-.69,0-1.25-.69-1.25-1.54h-.03Z"/>
    <g class="st19">
      <g class="st19">
        <g class="st19">
          <g class="st19">
            <g class="st19">
              <g class="st19">
                <g class="st19">
                  <g class="st19">
                    <text class="st16" transform="translate(48.41 646.07)"><tspan x="0" y="0">ir_export_data_to_excel</tspan></text>
                  </g>
                </g>
              </g>
            </g>
          </g>
        </g>
      </g>
    </g>
  </g>
  <g>
    <g>
      <rect class="st23" x="427.32" y="493.88" width="240.26" height="41.92" rx="4.23" ry="4.23"/>
      <rect class="st6" x="427.32" y="493.88" width="240.26" height="41.92" rx="4.23" ry="4.23"/>
    </g>
    <path class="st0" d="M441.64,514.84l1.12-10.98c.08-.77.61-1.35,1.25-1.35h6.61c.59,0,1.06.58,1.06,1.3,0,.15-.02.31-.07.46l-1.81,5.94h5.46c.79,0,1.44.79,1.44,1.77,0,.36-.09.7-.25,1l-7.53,13.53c-.23.41-.61.66-1.01.66h-.11c-.62,0-1.12-.62-1.12-1.37,0-.11,0-.22.04-.33l1.84-9.08h-5.64c-.69,0-1.25-.69-1.25-1.54h-.03,0Z"/>
    <g class="st19">
      <g class="st19">
        <g class="st19">
          <g class="st19">
            <g class="st19">
              <g class="st19">
                <g class="st19">
                  <g class="st19">
                    <text class="st16" transform="translate(466.18 519.37)"><tspan x="0" y="0">ir_save_aggregated_data</tspan></text>
                  </g>
                </g>
              </g>
            </g>
          </g>
        </g>
      </g>
    </g>
  </g>
  <g>
    <g>
      <rect class="st23" x="427.32" y="594.43" width="240.26" height="41.92" rx="4.23" ry="4.23"/>
      <rect class="st6" x="427.32" y="594.43" width="240.26" height="41.92" rx="4.23" ry="4.23"/>
    </g>
    <path class="st0" d="M441.64,615.39l1.12-10.98c.08-.77.61-1.35,1.25-1.35h6.61c.59,0,1.06.58,1.06,1.3,0,.15-.02.31-.07.46l-1.81,5.94h5.46c.79,0,1.44.79,1.44,1.77,0,.36-.09.7-.25,1l-7.53,13.53c-.23.41-.61.66-1.01.66h-.11c-.62,0-1.12-.62-1.12-1.37,0-.11,0-.22.04-.33l1.84-9.08h-5.64c-.69,0-1.25-.69-1.25-1.54h-.03,0Z"/>
    <g class="st19">
      <g class="st19">
        <g class="st19">
          <g class="st19">
            <g class="st19">
              <g class="st19">
                <g class="st19">
                  <g class="st19">
                    <text class="st16" transform="translate(466.18 619.92)"><tspan x="0" y="0">ir_load_aggregated_data</tspan></text>
                  </g>
                </g>
              </g>
            </g>
          </g>
        </g>
      </g>
    </g>
  </g>
  <g>
    <g>
      <rect class="st23" x="459.17" y="666.85" width="200.29" height="41.92" rx="4.23" ry="4.23"/>
      <rect class="st6" x="459.17" y="666.85" width="200.29" height="41.92" rx="4.23" ry="4.23"/>
    </g>
    <path class="st0" d="M473.49,687.81l1.12-10.98c.08-.77.61-1.35,1.25-1.35h6.61c.59,0,1.06.58,1.06,1.3,0,.15-.02.31-.07.46l-1.81,5.94h5.46c.79,0,1.44.79,1.44,1.77,0,.36-.09.7-.25,1l-7.53,13.53c-.23.41-.61.66-1.01.66h-.11c-.62,0-1.12-.62-1.12-1.37,0-.11,0-.22.04-.33l1.84-9.08h-5.64c-.69,0-1.25-.69-1.25-1.54h-.03,0Z"/>
    <g class="st19">
      <g class="st19">
        <g class="st19">
          <g class="st19">
            <g class="st19">
              <g class="st19">
                <g class="st19">
                  <g class="st19">
                    <text class="st16" transform="translate(498.03 692.34)"><tspan x="0" y="0">ir_filter_metadata</tspan></text>
                  </g>
                </g>
              </g>
            </g>
          </g>
        </g>
      </g>
    </g>
  </g>
  <g>
    <g>
      <rect class="st23" x="459.17" y="714.97" width="200.29" height="41.92" rx="4.23" ry="4.23"/>
      <rect class="st6" x="459.17" y="714.97" width="200.29" height="41.92" rx="4.23" ry="4.23"/>
    </g>
    <path class="st0" d="M473.49,735.93l1.12-10.98c.08-.77.61-1.35,1.25-1.35h6.61c.59,0,1.06.58,1.06,1.3,0,.15-.02.31-.07.46l-1.81,5.94h5.46c.79,0,1.44.79,1.44,1.77,0,.36-.09.7-.25,1l-7.53,13.53c-.23.41-.61.66-1.01.66h-.11c-.62,0-1.12-.62-1.12-1.37,0-.11,0-.22.04-.33l1.84-9.08h-5.64c-.69,0-1.25-.69-1.25-1.54h-.03,0Z"/>
    <g class="st19">
      <g class="st19">
        <g class="st19">
          <g class="st19">
            <g class="st19">
              <g class="st19">
                <g class="st19">
                  <g class="st19">
                    <text class="st16" transform="translate(498.03 740.46)"><tspan x="0" y="0">ir_mutate_metadata</tspan></text>
                  </g>
                </g>
              </g>
            </g>
          </g>
        </g>
      </g>
    </g>
  </g>
  <g>
    <line class="st9" x1="383.95" y1="466.46" x2="383.95" y2="907.58"/>
    <polygon class="st13" points="371.99 904.08 383.95 924.8 395.92 904.08 371.99 904.08"/>
  </g>
  <g>
    <line class="st9" x1="383.51" y1="64.12" x2="383.51" y2="84.67"/>
    <polygon class="st13" points="371.55 81.17 383.51 101.89 395.48 81.17 371.55 81.17"/>
  </g>
  <g>
    <line class="st9" x1="383.51" y1="150.15" x2="383.51" y2="170.7"/>
    <polygon class="st13" points="371.55 167.2 383.51 187.92 395.48 167.2 371.55 167.2"/>
  </g>
  <g>
    <line class="st9" x1="383.51" y1="236.95" x2="383.51" y2="400.5"/>
    <polygon class="st13" points="371.55 397 383.51 417.72 395.48 397 371.55 397"/>
  </g>
  <g>
    <line class="st1" x1="386.19" y1="775.7" x2="442.72" y2="775.7"/>
    <polygon class="st13" points="439.8 785.67 457.08 775.7 439.8 765.73 439.8 785.67"/>
  </g>
  <g>
    <line class="st1" x1="458.36" y1="791.15" x2="401.84" y2="791.15"/>
    <polygon class="st13" points="404.74 781.18 387.48 791.15 404.74 801.12 404.74 781.18"/>
  </g>
  <g>
    <line class="st1" x1="386.19" y1="728.72" x2="442.72" y2="728.72"/>
    <polygon class="st13" points="439.8 738.69 457.08 728.72 439.8 718.75 439.8 738.69"/>
  </g>
  <g>
    <line class="st1" x1="458.36" y1="744.17" x2="401.84" y2="744.17"/>
    <polygon class="st13" points="404.74 734.2 387.48 744.17 404.74 754.14 404.74 734.2"/>
  </g>
  <g>
    <line class="st1" x1="386.19" y1="682.99" x2="442.72" y2="682.99"/>
    <polygon class="st13" points="439.8 692.96 457.08 682.99 439.8 673.02 439.8 692.96"/>
  </g>
  <g>
    <line class="st1" x1="458.36" y1="698.44" x2="401.84" y2="698.44"/>
    <polygon class="st13" points="404.74 688.47 387.48 698.44 404.74 708.41 404.74 688.47"/>
  </g>
  <g>
    <line class="st1" x1="529.37" y1="217.65" x2="492.85" y2="217.65"/>
    <polygon class="st13" points="495.75 207.68 478.49 217.65 495.75 227.62 495.75 207.68"/>
  </g>
  <g>
    <line class="st1" x1="387.3" y1="514.53" x2="409.21" y2="514.53"/>
    <polygon class="st13" points="406.3 524.5 423.57 514.53 406.3 504.56 406.3 524.5"/>
  </g>
  <g>
    <line class="st1" x1="426.57" y1="616.29" x2="404.66" y2="616.29"/>
    <polygon class="st13" points="407.57 606.32 390.3 616.29 407.57 626.26 407.57 606.32"/>
  </g>
  <g>
    <line class="st1" x1="75.6" y1="663.27" x2="75.6" y2="859.18"/>
    <polygon class="st13" points="65.63 856.27 75.6 873.54 85.57 856.27 65.63 856.27"/>
  </g>
  <g>
    <line class="st1" x1="384.1" y1="494.54" x2="323.19" y2="494.54"/>
    <polygon class="st13" points="326.1 484.57 308.83 494.54 326.1 504.51 326.1 484.57"/>
  </g>
  <g>
    <line class="st1" x1="384.1" y1="694.59" x2="323.19" y2="694.59"/>
    <polygon class="st13" points="326.1 684.62 308.83 694.59 326.1 704.56 326.1 684.62"/>
  </g>
  <g>
    <polyline class="st1" points="526.35 36.65 582.58 36.65 582.58 70.07"/>
    <polygon class="st13" points="572.61 67.16 582.58 84.43 592.55 67.16 572.61 67.16"/>
  </g>
  <g>
    <polyline class="st1" points="260.71 642.16 282.13 642.16 282.13 837.39"/>
    <polygon class="st13" points="263.62 632.19 246.35 642.16 263.62 652.13 263.62 632.19"/>
  </g>
  <g>
    <polyline class="st1" points="720.99 586.72 720.99 615.96 687.57 615.96"/>
    <polygon class="st13" points="690.48 605.99 673.21 615.96 690.48 625.93 690.48 605.99"/>
  </g>
  <g>
    <polygon class="st4" points="666.02 414.34 603.85 414.34 572.77 444.92 603.85 475.5 666.02 475.5 697.1 444.92 666.02 414.34"/>
    <g class="st19">
      <g class="st19">
        <g class="st19">
          <g class="st19">
            <g class="st19">
              <g class="st19">
                <g class="st19">
                  <g class="st19">
                    <text class="st16" transform="translate(616.13 440.65)"><tspan x="0" y="0">V or A</tspan></text>
                  </g>
                </g>
                <g class="st19">
                  <g class="st19">
                    <text class="st16" transform="translate(616.13 461.05)"><tspan x="0" y="0">or cps</tspan></text>
                  </g>
                </g>
              </g>
            </g>
          </g>
        </g>
      </g>
    </g>
    <path class="st0" d="M597.43,434.67c0-1.41,1.15-2.56,2.56-2.56s2.56,1.15,2.56,2.56-1.15,2.56-2.56,2.56-2.56-1.15-2.56-2.56ZM594.86,442.36c0-.94.76-1.71,1.71-1.71h3.42c.94,0,1.71.76,1.71,1.71v11.96h1.71c.94,0,1.71.76,1.71,1.71s-.76,1.71-1.71,1.71h-6.83c-.94,0-1.71-.76-1.71-1.71s.76-1.71,1.71-1.71h1.71v-10.25h-1.71c-.94,0-1.71-.76-1.71-1.71h-.01Z"/>
  </g>
  <g>
    <g>
      <rect class="st17" x="2.19" y="128.96" width="283.19" height="322.82" rx="16.31" ry="16.31"/>
      <rect class="st5" x="2.19" y="128.96" width="283.19" height="322.82" rx="16.31" ry="16.31"/>
    </g>
    <g class="st19">
      <g class="st19">
        <g class="st19">
          <g class="st19">
            <g class="st19">
              <g class="st19">
                <g class="st19">
                  <g class="st19">
                    <text class="st14" transform="translate(63.55 334.76)"><tspan x="0" y="0">information functions</tspan></text>
                  </g>
                </g>
              </g>
            </g>
          </g>
        </g>
      </g>
    </g>
    <g class="st19">
      <g class="st19">
        <g class="st19">
          <g class="st19">
            <g class="st19">
              <g class="st19">
                <g class="st19">
                  <g class="st19">
                    <text class="st14" transform="translate(63.55 295.32)"><tspan x="0" y="0">processing functions</tspan></text>
                  </g>
                </g>
              </g>
            </g>
          </g>
        </g>
      </g>
    </g>
    <g class="st19">
      <g class="st19">
        <g class="st19">
          <g class="st19">
            <g class="st19">
              <g class="st19">
                <g class="st19">
                  <g class="st19">
                    <text class="st14" transform="translate(62.35 208.94)"><tspan x="0" y="0">isoreader core functions</tspan></text>
                  </g>
                </g>
              </g>
            </g>
          </g>
        </g>
      </g>
    </g>
    <g class="st19">
      <g class="st19">
        <g class="st19">
          <g class="st19">
            <g class="st19">
              <g class="st19">
                <g class="st19">
                  <g class="st19">
                    <text class="st14" transform="translate(62.35 238.7)"><tspan x="0" y="0">auxiliary functions</tspan></text>
                  </g>
                </g>
              </g>
            </g>
          </g>
        </g>
      </g>
      <g class="st19">
        <g class="st19">
          <g class="st19">
            <g class="st19">
              <g class="st19">
                <g class="st19">
                  <g class="st19">
                    <text class="st14" transform="translate(62.35 262.63)"><tspan x="0" y="0">(optional)</tspan></text>
                  </g>
                </g>
              </g>
            </g>
          </g>
        </g>
      </g>
    </g>
    <g class="st19">
      <g class="st19">
        <g class="st19">
          <g class="st19">
            <g class="st19">
              <g class="st19">
                <g class="st19">
                  <g class="st19">
                    <text class="st14" transform="translate(63.55 374.93)"><tspan x="0" y="0">visualization functions</tspan></text>
                  </g>
                </g>
              </g>
            </g>
          </g>
        </g>
      </g>
    </g>
    <g class="st19">
      <g class="st19">
        <g class="st19">
          <g class="st19">
            <g class="st19">
              <g class="st19">
                <g class="st19">
                  <g class="st19">
                    <text class="st14" transform="translate(63.55 409.71)"><tspan x="0" y="0">core functions, essential</tspan></text>
                  </g>
                </g>
              </g>
            </g>
          </g>
        </g>
      </g>
      <g class="st19">
        <g class="st19">
          <g class="st19">
            <g class="st19">
              <g class="st19">
                <g class="st19">
                  <g class="st19">
                    <text class="st14" transform="translate(63.55 433.64)"><tspan x="0" y="0">input from user</tspan></text>
                  </g>
                </g>
              </g>
            </g>
          </g>
        </g>
      </g>
    </g>
    <path class="st0" d="M23.69,289.99l1.23-12.12c.09-.85.67-1.49,1.38-1.49h7.29c.65,0,1.17.64,1.17,1.44,0,.17-.03.35-.07.5l-2,6.56h6.03c.87,0,1.59.87,1.59,1.95,0,.39-.1.78-.28,1.1l-8.31,14.93c-.26.46-.67.73-1.12.73h-.13c-.68,0-1.23-.68-1.23-1.51,0-.12.01-.24.04-.37l2.03-10.03h-6.23c-.77,0-1.38-.76-1.38-1.7h-.01Z"/>
    <g>
      <rect class="st23" x="17.45" y="188" width="31.26" height="31.26" rx="5.69" ry="5.69"/>
      <rect class="st7" x="17.45" y="188" width="31.26" height="31.26" rx="5.69" ry="5.69"/>
    </g>
    <g>
      <rect class="st23" x="17.45" y="229.72" width="31.26" height="31.26" rx="5.69" ry="5.69"/>
      <rect class="st10" x="17.45" y="229.72" width="31.26" height="31.26" rx="5.69" ry="5.69"/>
    </g>
    <g>
      <polygon class="st3" points="34.43 398.74 33.18 398.74 15.27 416.36 33.18 433.99 34.43 433.99 52.35 416.36 34.43 398.74"/>
      <path class="st0" d="M32.55,410.45c0-.82.66-1.48,1.48-1.48s1.48.66,1.48,1.48-.66,1.48-1.48,1.48-1.48-.66-1.48-1.48ZM31.07,414.89c0-.54.44-.98.98-.98h1.97c.54,0,.98.44.98.98v6.89h.98c.54,0,.98.44.98.98s-.44.98-.98.98h-3.94c-.54,0-.98-.44-.98-.98s.44-.98.98-.98h.98v-5.91h-.98c-.54,0-.98-.44-.98-.98h0Z"/>
    </g>
    <path class="st0" d="M31.99,343.98c8.03,0,14.54-6.51,14.54-14.54s-6.51-14.54-14.54-14.54-14.54,6.51-14.54,14.54,6.51,14.54,14.54,14.54ZM29.72,333.98h1.36v-3.63h-1.36c-.76,0-1.36-.61-1.36-1.36s.61-1.36,1.36-1.36h2.73c.76,0,1.36.61,1.36,1.36v5h.45c.76,0,1.36.61,1.36,1.36s-.61,1.36-1.36,1.36h-4.54c-.76,0-1.36-.61-1.36-1.36s.61-1.36,1.36-1.36h0ZM31.99,322.17c1,0,1.82.81,1.82,1.82s-.81,1.82-1.82,1.82-1.82-.81-1.82-1.82.81-1.82,1.82-1.82Z"/>
    <path class="st0" d="M22.01,359.63c0-.92-.74-1.66-1.66-1.66s-1.66.74-1.66,1.66v17.47c0,2.3,1.86,4.16,4.16,4.16h20.8c.92,0,1.66-.74,1.66-1.66s-.74-1.66-1.66-1.66h-20.8c-.46,0-.83-.37-.83-.83v-17.47h-.01,0ZM43.15,364.13c.65-.65.65-1.71,0-2.36s-1.71-.65-2.36,0l-5.47,5.48-2.98-2.98c-.65-.65-1.71-.65-2.36,0l-5.82,5.82c-.65.65-.65,1.71,0,2.36s1.71.65,2.36,0l4.65-4.64,2.98,2.98c.65.65,1.71.65,2.36,0l6.65-6.65h-.01Z"/>
    <g class="st19">
      <g class="st19">
        <g class="st19">
          <g class="st19">
            <g class="st19">
              <g class="st19">
                <g class="st19">
                  <g class="st19">
                    <text class="st14" transform="translate(63.48 166.75)"><tspan x="0" y="0">data files</tspan></text>
                  </g>
                </g>
              </g>
            </g>
          </g>
        </g>
      </g>
    </g>
    <g>
      <rect class="st24" x="17.13" y="145.16" width="32.52" height="32.52" rx="1.55" ry="1.55"/>
      <rect class="st8" x="17.13" y="145.16" width="32.52" height="32.52" rx="1.55" ry="1.55"/>
    </g>
  </g>
  <a xlink:href="https://isoreader2.isoverse.org/reference/ir_read_isofiles.html">
    <g>
      <g>
        <rect class="st23" x="300.37" y="194.41" width="172.29" height="41.92" rx="4.23" ry="4.23"/>
        <rect class="st7" x="300.37" y="194.41" width="172.29" height="41.92" rx="4.23" ry="4.23"/>
      </g>
      <path class="st0" d="M314.69,215.37l1.12-10.98c.08-.77.61-1.35,1.25-1.35h6.61c.59,0,1.06.58,1.06,1.3,0,.15-.02.31-.07.46l-1.81,5.94h5.46c.79,0,1.44.79,1.44,1.77,0,.36-.09.7-.25,1l-7.53,13.53c-.23.41-.61.66-1.01.66h-.11c-.62,0-1.12-.62-1.12-1.37,0-.11.01-.22.04-.33l1.84-9.08h-5.64c-.69,0-1.25-.69-1.25-1.54h-.03,0Z"/>
      <g class="st19">
        <g class="st19">
          <g class="st19">
            <g class="st19">
              <g class="st19">
                <g class="st19">
                  <g class="st19">
                    <g class="st19">
                      <text class="st16" transform="translate(340.22 219.91)"><tspan x="0" y="0">ir_read_isofiles</tspan></text>
                    </g>
                  </g>
                </g>
              </g>
            </g>
          </g>
        </g>
      </g>
    </g>
  </a>
  <a xlink:href="https://isoreader2.isoverse.org/reference/ir_find_isofiles.html">
    <g>
      <g>
        <rect class="st23" x="300.37" y="107.86" width="172.29" height="41.92" rx="4.23" ry="4.23"/>
        <rect class="st7" x="300.37" y="107.86" width="172.29" height="41.92" rx="4.23" ry="4.23"/>
      </g>
      <path class="st0" d="M314.69,128.82l1.12-10.98c.08-.77.61-1.35,1.25-1.35h6.61c.59,0,1.06.58,1.06,1.3,0,.15-.02.31-.07.46l-1.81,5.94h5.46c.79,0,1.44.79,1.44,1.77,0,.36-.09.7-.25,1l-7.53,13.53c-.23.41-.61.66-1.01.66h-.11c-.62,0-1.12-.62-1.12-1.37,0-.11.01-.22.04-.33l1.84-9.08h-5.64c-.69,0-1.25-.69-1.25-1.54h-.03,0Z"/>
      <g class="st19">
        <g class="st19">
          <g class="st19">
            <g class="st19">
              <g class="st19">
                <g class="st19">
                  <g class="st19">
                    <g class="st19">
                      <text class="st16" transform="translate(340.22 133.36)"><tspan x="0" y="0">ir_find_isofiles</tspan></text>
                    </g>
                  </g>
                </g>
              </g>
            </g>
          </g>
        </g>
      </g>
    </g>
  </a>
  <a xlink:href="https://isoreader2.isoverse.org/reference/ir_aggregate_isofiles.html">
    <g>
      <g>
        <rect class="st23" x="300.18" y="423.96" width="207.29" height="41.92" rx="4.23" ry="4.23"/>
        <rect class="st7" x="300.18" y="423.96" width="207.29" height="41.92" rx="4.23" ry="4.23"/>
      </g>
      <path class="st0" d="M314.5,444.92l1.12-10.98c.08-.77.61-1.35,1.25-1.35h6.61c.59,0,1.06.58,1.06,1.3,0,.15-.02.31-.07.46l-1.81,5.94h5.46c.79,0,1.44.79,1.44,1.77,0,.36-.09.7-.25,1l-7.53,13.53c-.23.41-.61.66-1.01.66h-.11c-.62,0-1.12-.62-1.12-1.37,0-.11.01-.22.04-.33l1.84-9.08h-5.64c-.69,0-1.25-.69-1.25-1.54h-.03,0Z"/>
      <g class="st19">
        <g class="st19">
          <g class="st19">
            <g class="st19">
              <g class="st19">
                <g class="st19">
                  <g class="st19">
                    <g class="st19">
                      <text class="st16" transform="translate(339.03 449.46)"><tspan x="0" y="0">ir_aggregate_isofiles</tspan></text>
                    </g>
                  </g>
                </g>
              </g>
            </g>
          </g>
        </g>
      </g>
    </g>
  </a>
  <a xlink:href="https://isoreader2.isoverse.org/reference/ir_plot_scans.html">
    <g>
      <g>
        <path class="st23" d="M72.65,569.79h215.03c3.12,0,11.65,2.53,11.65,5.65v30.62c0,3.12-8.53,5.65-11.65,5.65H72.65c-3.12,0-5.65-2.53-5.65-5.65v-30.62c0-3.12,2.53-5.65,5.65-5.65Z"/>
        <path class="st6" d="M72.65,569.79h215.03c3.12,0,11.65,2.53,11.65,5.65v30.62c0,3.12-8.53,5.65-11.65,5.65H72.65c-3.12,0-5.65-2.53-5.65-5.65v-30.62c0-3.12,2.53-5.65,5.65-5.65Z"/>
      </g>
      <g class="st19">
        <g class="st19">
          <g class="st19">
            <g class="st19">
              <g class="st19">
                <g class="st19">
                  <g class="st19">
                    <g class="st19">
                      <text class="st16" transform="translate(111.85 595.28)"><tspan x="0" y="0">ir_plot_scans</tspan></text>
                    </g>
                  </g>
                </g>
              </g>
            </g>
          </g>
        </g>
      </g>
      <path class="st0" d="M77.29,580.58c0-.92-.74-1.66-1.66-1.66s-1.66.74-1.66,1.66v17.47c0,2.3,1.86,4.16,4.16,4.16h20.8c.92,0,1.66-.74,1.66-1.66s-.74-1.66-1.66-1.66h-20.8c-.46,0-.83-.37-.83-.83v-17.47h-.01,0ZM98.43,585.08c.65-.65.65-1.71,0-2.36s-1.71-.65-2.36,0l-5.47,5.48-2.98-2.98c-.65-.65-1.71-.65-2.36,0l-5.82,5.82c-.65.65-.65,1.71,0,2.36s1.71.65,2.36,0l4.65-4.64,2.98,2.98c.65.65,1.71.65,2.36,0l6.65-6.65h-.01Z"/>
    </g>
  </a>
  <g>
    <line class="st1" x1="384.1" y1="543.35" x2="323.19" y2="543.35"/>
    <polygon class="st13" points="326.1 533.38 308.83 543.35 326.1 553.32 326.1 533.38"/>
  </g>
  <a xlink:href="https://isoreader2.isoverse.org/reference/ir_plot_dual_inlet.html">
    <g>
      <g>
        <path class="st23" d="M72.65,520.99h215.03c3.12,0,11.65,2.53,11.65,5.65v30.62c0,3.12-8.53,5.65-11.65,5.65H72.65c-3.12,0-5.65-2.53-5.65-5.65v-30.62c0-3.12,2.53-5.65,5.65-5.65Z"/>
        <path class="st6" d="M72.65,520.99h215.03c3.12,0,11.65,2.53,11.65,5.65v30.62c0,3.12-8.53,5.65-11.65,5.65H72.65c-3.12,0-5.65-2.53-5.65-5.65v-30.62c0-3.12,2.53-5.65,5.65-5.65Z"/>
      </g>
      <g class="st19">
        <g class="st19">
          <g class="st19">
            <g class="st19">
              <g class="st19">
                <g class="st19">
                  <g class="st19">
                    <g class="st19">
                      <text class="st16" transform="translate(111.85 546.48)"><tspan x="0" y="0">ir_plot_dual_inlet</tspan></text>
                    </g>
                  </g>
                </g>
              </g>
            </g>
          </g>
        </g>
      </g>
      <path class="st0" d="M77.29,531.78c0-.92-.74-1.66-1.66-1.66s-1.66.74-1.66,1.66v17.47c0,2.3,1.86,4.16,4.16,4.16h20.8c.92,0,1.66-.74,1.66-1.66s-.74-1.66-1.66-1.66h-20.8c-.46,0-.83-.37-.83-.83v-17.47h-.01,0ZM98.43,536.28c.65-.65.65-1.71,0-2.36s-1.71-.65-2.36,0l-5.47,5.48-2.98-2.98c-.65-.65-1.71-.65-2.36,0l-5.82,5.82c-.65.65-.65,1.71,0,2.36s1.71.65,2.36,0l4.65-4.64,2.98,2.98c.65.65,1.71.65,2.36,0l6.65-6.65h-.01Z"/>
    </g>
  </a>
  <g>
    <line class="st1" x1="384.1" y1="591.22" x2="323.19" y2="591.22"/>
    <polygon class="st13" points="326.1 581.25 308.83 591.22 326.1 601.19 326.1 581.25"/>
  </g>
  <a xlink:href="https://isoreader2.isoverse.org/reference/ir_plot_continuous_flow.html">
    <g>
      <g>
        <path class="st23" d="M72.65,473.11h215.03c3.12,0,11.65,2.53,11.65,5.65v30.62c0,3.12-8.53,5.65-11.65,5.65H72.65c-3.12,0-5.65-2.53-5.65-5.65v-30.62c0-3.12,2.53-5.65,5.65-5.65Z"/>
        <path class="st6" d="M72.65,473.11h215.03c3.12,0,11.65,2.53,11.65,5.65v30.62c0,3.12-8.53,5.65-11.65,5.65H72.65c-3.12,0-5.65-2.53-5.65-5.65v-30.62c0-3.12,2.53-5.65,5.65-5.65Z"/>
      </g>
      <g class="st19">
        <g class="st19">
          <g class="st19">
            <g class="st19">
              <g class="st19">
                <g class="st19">
                  <g class="st19">
                    <g class="st19">
                      <text class="st16" transform="translate(111.85 498.6)"><tspan x="0" y="0">ir_plot_continuous_flow</tspan></text>
                    </g>
                  </g>
                </g>
              </g>
            </g>
          </g>
        </g>
      </g>
      <path class="st0" d="M77.29,483.9c0-.92-.74-1.66-1.66-1.66s-1.66.74-1.66,1.66v17.47c0,2.3,1.86,4.16,4.16,4.16h20.8c.92,0,1.66-.74,1.66-1.66s-.74-1.66-1.66-1.66h-20.8c-.46,0-.83-.37-.83-.83v-17.47h-.01,0ZM98.43,488.4c.65-.65.65-1.71,0-2.36s-1.71-.65-2.36,0l-5.47,5.48-2.98-2.98c-.65-.65-1.71-.65-2.36,0l-5.82,5.82c-.65.65-.65,1.71,0,2.36s1.71.65,2.36,0l4.65-4.64,2.98,2.98c.65.65,1.71.65,2.36,0l6.65-6.65h-.01Z"/>
    </g>
  </a>
  <a xlink:href="https://isoreader2.isoverse.org/reference/ir_find_isofiles.html">
    <g>
      <g>
        <rect class="st23" x="529.88" y="95.23" width="224.14" height="41.92" rx="4.23" ry="4.23"/>
        <rect class="st2" x="529.88" y="95.23" width="224.14" height="41.92" rx="4.23" ry="4.23"/>
      </g>
      <path class="st0" d="M544.2,117.19l1.12-10.98c.08-.77.61-1.35,1.25-1.35h6.61c.59,0,1.06.58,1.06,1.3,0,.15-.02.31-.07.46l-1.81,5.94h5.46c.79,0,1.44.79,1.44,1.77,0,.36-.09.7-.25,1l-7.53,13.53c-.23.41-.61.66-1.01.66h-.11c-.62,0-1.12-.62-1.12-1.37,0-.11,0-.22.04-.33l1.84-9.08h-5.64c-.69,0-1.25-.69-1.25-1.54h-.03,0Z"/>
      <g class="st19">
        <g class="st19">
          <g class="st19">
            <g class="st19">
              <g class="st19">
                <g class="st19">
                  <g class="st19">
                    <g class="st19">
                      <text class="st16" transform="translate(565.99 121.72)"><tspan x="0" y="0">ir_find_continuous_flow</tspan></text>
                    </g>
                  </g>
                </g>
              </g>
            </g>
          </g>
        </g>
      </g>
    </g>
  </a>
  <a xlink:href="https://isoreader2.isoverse.org/reference/ir_find_isofiles.html">
    <g>
      <g>
        <rect class="st23" x="39.37" y="49.11" width="195.14" height="41.92" rx="4.23" ry="4.23"/>
        <rect class="st2" x="39.37" y="49.11" width="195.14" height="41.92" rx="4.23" ry="4.23"/>
      </g>
      <path class="st0" d="M53.69,71.07l1.12-10.98c.08-.77.61-1.35,1.25-1.35h6.61c.59,0,1.06.58,1.06,1.3,0,.15-.02.31-.07.46l-1.81,5.94h5.46c.79,0,1.44.79,1.44,1.77,0,.36-.09.7-.25,1l-7.53,13.53c-.23.41-.61.66-1.01.66h-.11c-.62,0-1.12-.62-1.12-1.37,0-.11,0-.22.04-.33l1.84-9.08h-5.64c-.69,0-1.25-.69-1.25-1.54h-.03,0Z"/>
      <g class="st19">
        <g class="st19">
          <g class="st19">
            <g class="st19">
              <g class="st19">
                <g class="st19">
                  <g class="st19">
                    <g class="st19">
                      <text class="st16" transform="translate(75.48 75.6)"><tspan x="0" y="0">ir_examples_folder</tspan></text>
                    </g>
                  </g>
                </g>
              </g>
            </g>
          </g>
        </g>
      </g>
    </g>
  </a>
  <a xlink:href="https://isoreader2.isoverse.org/reference/ir_find_isofiles.html">
    <g>
      <g>
        <rect class="st23" x="530.07" y="145.61" width="178.14" height="41.92" rx="4.23" ry="4.23"/>
        <rect class="st2" x="530.07" y="145.61" width="178.14" height="41.92" rx="4.23" ry="4.23"/>
      </g>
      <path class="st0" d="M544.39,167.57l1.12-10.98c.08-.77.61-1.35,1.25-1.35h6.61c.59,0,1.06.58,1.06,1.3,0,.15-.02.31-.07.46l-1.81,5.94h5.46c.79,0,1.44.79,1.44,1.77,0,.36-.09.7-.25,1l-7.53,13.53c-.23.41-.61.66-1.01.66h-.11c-.62,0-1.12-.62-1.12-1.37,0-.11,0-.22.04-.33l1.84-9.08h-5.64c-.69,0-1.25-.69-1.25-1.54h-.03,0Z"/>
      <g class="st19">
        <g class="st19">
          <g class="st19">
            <g class="st19">
              <g class="st19">
                <g class="st19">
                  <g class="st19">
                    <g class="st19">
                      <text class="st16" transform="translate(566.05 172.1)"><tspan x="0" y="0">ir_find_dual_inlet</tspan></text>
                    </g>
                  </g>
                </g>
              </g>
            </g>
          </g>
        </g>
      </g>
    </g>
  </a>
  <a xlink:href="https://isoreader2.isoverse.org/reference/ir_find_isofiles.html">
    <g>
      <g>
        <rect class="st23" x="529.45" y="196.69" width="156.14" height="41.92" rx="4.23" ry="4.23"/>
        <rect class="st2" x="529.45" y="196.69" width="156.14" height="41.92" rx="4.23" ry="4.23"/>
      </g>
      <path class="st0" d="M543.77,218.65l1.12-10.98c.08-.77.61-1.35,1.25-1.35h6.61c.59,0,1.06.58,1.06,1.3,0,.15-.02.31-.07.46l-1.81,5.94h5.46c.79,0,1.44.79,1.44,1.77,0,.36-.09.7-.25,1l-7.53,13.53c-.23.41-.61.66-1.01.66h-.11c-.62,0-1.12-.62-1.12-1.37,0-.11,0-.22.04-.33l1.84-9.08h-5.64c-.69,0-1.25-.69-1.25-1.54h-.03,0Z"/>
      <g class="st19">
        <g class="st19">
          <g class="st19">
            <g class="st19">
              <g class="st19">
                <g class="st19">
                  <g class="st19">
                    <g class="st19">
                      <text class="st16" transform="translate(569.3 223.18)"><tspan x="0" y="0">ir_find_scans</tspan></text>
                    </g>
                  </g>
                </g>
              </g>
            </g>
          </g>
        </g>
      </g>
    </g>
  </a>
  <a xlink:href="https://isoreader2.isoverse.org/reference/ir_get_data.html">
    <g>
      <g>
        <rect class="st23" x="115.81" y="672.42" width="187.52" height="41.92" rx="5.08" ry="5.08"/>
        <rect class="st12" x="115.81" y="672.42" width="187.52" height="41.92" rx="5.08" ry="5.08"/>
      </g>
      <g class="st19">
        <g class="st19">
          <g class="st19">
            <g class="st19">
              <g class="st19">
                <g class="st19">
                  <g class="st19">
                    <g class="st19">
                      <text class="st16" transform="translate(160.66 697.91)"><tspan x="0" y="0">ir_get_data</tspan></text>
                    </g>
                  </g>
                </g>
              </g>
            </g>
          </g>
        </g>
      </g>
      <path class="st0" d="M136.9,707.92c8.03,0,14.54-6.51,14.54-14.54s-6.51-14.54-14.54-14.54-14.54,6.51-14.54,14.54,6.51,14.54,14.54,14.54ZM134.63,697.92h1.36v-3.63h-1.36c-.76,0-1.36-.61-1.36-1.36s.61-1.36,1.36-1.36h2.73c.76,0,1.36.61,1.36,1.36v5h.45c.76,0,1.36.61,1.36,1.36s-.61,1.36-1.36,1.36h-4.54c-.76,0-1.36-.61-1.36-1.36s.61-1.36,1.36-1.36h0ZM136.9,686.11c1,0,1.82.81,1.82,1.82s-.81,1.82-1.82,1.82-1.82-.81-1.82-1.82.81-1.82,1.82-1.82Z"/>
    </g>
  </a>
  <a xlink:href="https://isoreader2.isoverse.org/reference/ir_get_data.html">
    <g>
      <g>
        <rect class="st23" x="115.81" y="722.27" width="206.52" height="41.92" rx="5.08" ry="5.08"/>
        <rect class="st12" x="115.81" y="722.27" width="206.52" height="41.92" rx="5.08" ry="5.08"/>
      </g>
      <g class="st19">
        <g class="st19">
          <g class="st19">
            <g class="st19">
              <g class="st19">
                <g class="st19">
                  <g class="st19">
                    <g class="st19">
                      <text class="st16" transform="translate(160.66 747.76)"><tspan x="0" y="0">ir_get_metadata</tspan></text>
                    </g>
                  </g>
                </g>
              </g>
            </g>
          </g>
        </g>
      </g>
      <path class="st0" d="M136.9,757.77c8.03,0,14.54-6.51,14.54-14.54s-6.51-14.54-14.54-14.54-14.54,6.51-14.54,14.54,6.51,14.54,14.54,14.54ZM134.63,747.77h1.36v-3.63h-1.36c-.76,0-1.36-.61-1.36-1.36s.61-1.36,1.36-1.36h2.73c.76,0,1.36.61,1.36,1.36v5h.45c.76,0,1.36.61,1.36,1.36s-.61,1.36-1.36,1.36h-4.54c-.76,0-1.36-.61-1.36-1.36s.61-1.36,1.36-1.36h0ZM136.9,735.96c1,0,1.82.81,1.82,1.82s-.81,1.82-1.82,1.82-1.82-.81-1.82-1.82.81-1.82,1.82-1.82Z"/>
    </g>
  </a>
  <a xlink:href="https://isoreader2.isoverse.org/reference/ir_get_data.html">
    <g>
      <g>
        <rect class="st23" x="115.81" y="772.12" width="252.52" height="41.92" rx="5.08" ry="5.08"/>
        <rect class="st12" x="115.81" y="772.12" width="252.52" height="41.92" rx="5.08" ry="5.08"/>
      </g>
      <g class="st19">
        <g class="st19">
          <g class="st19">
            <g class="st19">
              <g class="st19">
                <g class="st19">
                  <g class="st19">
                    <g class="st19">
                      <text class="st16" transform="translate(160.66 797.61)"><tspan x="0" y="0">ir_get_traces/cycles/scans</tspan></text>
                    </g>
                  </g>
                </g>
              </g>
            </g>
          </g>
        </g>
      </g>
      <path class="st0" d="M136.9,807.62c8.03,0,14.54-6.51,14.54-14.54s-6.51-14.54-14.54-14.54-14.54,6.51-14.54,14.54,6.51,14.54,14.54,14.54ZM134.63,797.62h1.36v-3.63h-1.36c-.76,0-1.36-.61-1.36-1.36s.61-1.36,1.36-1.36h2.73c.76,0,1.36.61,1.36,1.36v5h.45c.76,0,1.36.61,1.36,1.36s-.61,1.36-1.36,1.36h-4.54c-.76,0-1.36-.61-1.36-1.36s.61-1.36,1.36-1.36h0ZM136.9,785.81c1,0,1.82.81,1.82,1.82s-.81,1.82-1.82,1.82-1.82-.81-1.82-1.82.81-1.82,1.82-1.82Z"/>
    </g>
  </a>
  <a xlink:href="https://isoreader2.isoverse.org/reference/ir_get_data.html">
    <g>
      <g>
        <rect class="st23" x="115.81" y="820.16" width="171.52" height="41.92" rx="5.08" ry="5.08"/>
        <rect class="st12" x="115.81" y="820.16" width="171.52" height="41.92" rx="5.08" ry="5.08"/>
      </g>
      <g class="st19">
        <g class="st19">
          <g class="st19">
            <g class="st19">
              <g class="st19">
                <g class="st19">
                  <g class="st19">
                    <g class="st19">
                      <text class="st16" transform="translate(160.66 845.65)"><tspan x="0" y="0">ir_get_resistors</tspan></text>
                    </g>
                  </g>
                </g>
              </g>
            </g>
          </g>
        </g>
      </g>
      <path class="st0" d="M136.9,855.66c8.03,0,14.54-6.51,14.54-14.54s-6.51-14.54-14.54-14.54-14.54,6.51-14.54,14.54,6.51,14.54,14.54,14.54ZM134.63,845.66h1.36v-3.63h-1.36c-.76,0-1.36-.61-1.36-1.36s.61-1.36,1.36-1.36h2.73c.76,0,1.36.61,1.36,1.36v5h.45c.76,0,1.36.61,1.36,1.36s-.61,1.36-1.36,1.36h-4.54c-.76,0-1.36-.61-1.36-1.36s.61-1.36,1.36-1.36h0ZM136.9,833.85c1,0,1.82.81,1.82,1.82s-.81,1.82-1.82,1.82-1.82-.81-1.82-1.82.81-1.82,1.82-1.82Z"/>
    </g>
  </a>
  <g>
    <g>
      <rect class="st24" x="300.58" y="9.25" width="224.34" height="53.98" rx="4.45" ry="4.45"/>
      <rect class="st8" x="300.58" y="9.25" width="224.34" height="53.98" rx="4.45" ry="4.45"/>
    </g>
    <g class="st19">
      <g class="st19">
        <g class="st19">
          <g class="st19">
            <g class="st19">
              <g class="st19">
                <g class="st19">
                  <g class="st19">
                    <text class="st16" transform="translate(314.27 57.15)"><tspan x="0" y="0">isotope data files/archives</tspan></text>
                  </g>
                </g>
              </g>
            </g>
          </g>
        </g>
      </g>
    </g>
    <path class="st0" d="M324.01,39.84c.47,0,.86-.39.86-.86v-15.44h-4.29c-.95,0-1.72-.77-1.72-1.72v-4.29h-8.58c-.47,0-.86.39-.86.86v20.58c0,.47.39.86.86.86h13.73ZM306.86,18.4c0-1.89,1.54-3.43,3.43-3.43h8.87c.91,0,1.78.36,2.43,1l4.85,4.85c.64.64,1,1.52,1,2.43v15.73c0,1.89-1.54,3.43-3.43,3.43h-13.72c-1.89,0-3.43-1.54-3.43-3.43v-20.58h0Z"/>
    <g class="st19">
      <g class="st19">
        <g class="st19">
          <g class="st19">
            <g class="st19">
              <g class="st19">
                <g class="st19">
                  <text class="st20" transform="translate(317.15 38.54) rotate(-90)"><tspan x="0" y="0">dxf</tspan></text>
                </g>
              </g>
            </g>
          </g>
        </g>
      </g>
    </g>
    <path class="st0" d="M347.75,39.83c.47,0,.86-.39.86-.86v-15.44h-4.29c-.95,0-1.72-.77-1.72-1.72v-4.29h-8.58c-.47,0-.86.39-.86.86v20.58c0,.47.39.86.86.86h13.73ZM330.6,18.39c0-1.89,1.54-3.43,3.43-3.43h8.87c.91,0,1.78.36,2.43,1l4.85,4.85c.64.64,1,1.52,1,2.43v15.73c0,1.89-1.54,3.43-3.43,3.43h-13.72c-1.89,0-3.43-1.54-3.43-3.43v-20.58h0Z"/>
    <g class="st19">
      <g class="st19">
        <g class="st19">
          <g class="st19">
            <g class="st19">
              <g class="st19">
                <g class="st19">
                  <text class="st20" transform="translate(340.89 38.53) rotate(-90)"><tspan x="0" y="0">cf</tspan></text>
                </g>
              </g>
            </g>
          </g>
        </g>
      </g>
    </g>
    <path class="st0" d="M371.65,39.83c.47,0,.86-.39.86-.86v-15.44h-4.29c-.95,0-1.72-.77-1.72-1.72v-4.29h-8.58c-.47,0-.86.39-.86.86v20.58c0,.47.39.86.86.86h13.73ZM354.5,18.39c0-1.89,1.54-3.43,3.43-3.43h8.87c.91,0,1.78.36,2.43,1l4.85,4.85c.64.64,1,1.52,1,2.43v15.73c0,1.89-1.54,3.43-3.43,3.43h-13.72c-1.89,0-3.43-1.54-3.43-3.43v-20.58h0Z"/>
    <g class="st19">
      <g class="st19">
        <g class="st19">
          <g class="st19">
            <g class="st19">
              <g class="st19">
                <g class="st19">
                  <text class="st20" transform="translate(364.79 38.53) rotate(-90)"><tspan x="0" y="0">bch</tspan></text>
                </g>
              </g>
            </g>
          </g>
        </g>
      </g>
    </g>
    <path class="st0" d="M395.87,39.84c.47,0,.86-.39.86-.86v-15.44h-4.29c-.95,0-1.72-.77-1.72-1.72v-4.29h-8.58c-.47,0-.86.39-.86.86v20.58c0,.47.39.86.86.86h13.73ZM378.72,18.4c0-1.89,1.54-3.43,3.43-3.43h8.87c.91,0,1.78.36,2.43,1l4.85,4.85c.64.64,1,1.52,1,2.43v15.73c0,1.89-1.54,3.43-3.43,3.43h-13.72c-1.89,0-3.43-1.54-3.43-3.43v-20.58h0Z"/>
    <g class="st19">
      <g class="st19">
        <g class="st19">
          <g class="st19">
            <g class="st19">
              <g class="st19">
                <g class="st19">
                  <text class="st20" transform="translate(389.01 38.54) rotate(-90)"><tspan x="0" y="0">iarc</tspan></text>
                </g>
              </g>
            </g>
          </g>
        </g>
      </g>
    </g>
    <path class="st0" d="M419.95,39.84c.47,0,.86-.39.86-.86v-15.44h-4.29c-.95,0-1.72-.77-1.72-1.72v-4.29h-8.58c-.47,0-.86.39-.86.86v20.58c0,.47.39.86.86.86h13.73ZM402.8,18.4c0-1.89,1.54-3.43,3.43-3.43h8.87c.91,0,1.78.36,2.43,1l4.85,4.85c.64.64,1,1.52,1,2.43v15.73c0,1.89-1.54,3.43-3.43,3.43h-13.72c-1.89,0-3.43-1.54-3.43-3.43v-20.58h0Z"/>
    <g class="st19">
      <g class="st19">
        <g class="st19">
          <g class="st19">
            <g class="st19">
              <g class="st19">
                <g class="st19">
                  <text class="st20" transform="translate(413.09 38.54) rotate(-90)"><tspan x="0" y="0">larc</tspan></text>
                </g>
              </g>
            </g>
          </g>
        </g>
      </g>
    </g>
    <path class="st0" d="M443.69,39.83c.47,0,.86-.39.86-.86v-15.44h-4.29c-.95,0-1.72-.77-1.72-1.72v-4.29h-8.58c-.47,0-.86.39-.86.86v20.58c0,.47.39.86.86.86h13.73ZM426.54,18.39c0-1.89,1.54-3.43,3.43-3.43h8.87c.91,0,1.78.36,2.43,1l4.85,4.85c.64.64,1,1.52,1,2.43v15.73c0,1.89-1.54,3.43-3.43,3.43h-13.72c-1.89,0-3.43-1.54-3.43-3.43v-20.58h0Z"/>
    <g class="st19">
      <g class="st19">
        <g class="st19">
          <g class="st19">
            <g class="st19">
              <g class="st19">
                <g class="st19">
                  <text class="st21" transform="translate(434.83 38.53) rotate(-90)"><tspan x="0" y="0">imexp</tspan></text>
                </g>
              </g>
            </g>
          </g>
        </g>
      </g>
    </g>
    <path class="st0" d="M467.59,39.83c.47,0,.86-.39.86-.86v-15.44h-4.29c-.95,0-1.72-.77-1.72-1.72v-4.29h-8.58c-.47,0-.86.39-.86.86v20.58c0,.47.39.86.86.86h13.73ZM450.44,18.39c0-1.89,1.54-3.43,3.43-3.43h8.87c.91,0,1.78.36,2.43,1l4.85,4.85c.64.64,1,1.52,1,2.43v15.73c0,1.89-1.54,3.43-3.43,3.43h-13.72c-1.89,0-3.43-1.54-3.43-3.43v-20.58h0Z"/>
    <g class="st19">
      <g class="st19">
        <g class="st19">
          <g class="st19">
            <g class="st19">
              <g class="st19">
                <g class="st19">
                  <text class="st20" transform="translate(460.73 38.53) rotate(-90)"><tspan x="0" y="0">did</tspan></text>
                </g>
              </g>
            </g>
          </g>
        </g>
      </g>
    </g>
    <path class="st0" d="M491.8,39.84c.47,0,.86-.39.86-.86v-15.44h-4.29c-.95,0-1.72-.77-1.72-1.72v-4.29h-8.58c-.47,0-.86.39-.86.86v20.58c0,.47.39.86.86.86h13.73ZM474.65,18.4c0-1.89,1.54-3.43,3.43-3.43h8.87c.91,0,1.78.36,2.43,1l4.85,4.85c.64.64,1,1.52,1,2.43v15.73c0,1.89-1.54,3.43-3.43,3.43h-13.72c-1.89,0-3.43-1.54-3.43-3.43v-20.58h0Z"/>
    <g class="st19">
      <g class="st19">
        <g class="st19">
          <g class="st19">
            <g class="st19">
              <g class="st19">
                <g class="st19">
                  <text class="st20" transform="translate(484.94 38.54) rotate(-90)"><tspan x="0" y="0">caf</tspan></text>
                </g>
              </g>
            </g>
          </g>
        </g>
      </g>
    </g>
    <path class="st0" d="M515.46,39.84c.47,0,.86-.39.86-.86v-15.44h-4.29c-.95,0-1.72-.77-1.72-1.72v-4.29h-8.58c-.47,0-.86.39-.86.86v20.58c0,.47.39.86.86.86h13.73ZM498.31,18.4c0-1.89,1.54-3.43,3.43-3.43h8.87c.91,0,1.78.36,2.43,1l4.85,4.85c.64.64,1,1.52,1,2.43v15.73c0,1.89-1.54,3.43-3.43,3.43h-13.72c-1.89,0-3.43-1.54-3.43-3.43v-20.58h0Z"/>
    <g class="st19">
      <g class="st19">
        <g class="st19">
          <g class="st19">
            <g class="st19">
              <g class="st19">
                <g class="st19">
                  <text class="st20" transform="translate(508.6 38.54) rotate(-90)"><tspan x="0" y="0">scn</tspan></text>
                </g>
              </g>
            </g>
          </g>
        </g>
      </g>
    </g>
  </g>
  <g>
    <g>
      <rect class="st24" x="546.51" y="544.15" width="209.29" height="41.92" rx="4.73" ry="4.73"/>
      <rect class="st7" x="546.51" y="544.15" width="209.29" height="41.92" rx="4.73" ry="4.73"/>
    </g>
    <g class="st19">
      <g class="st19">
        <g class="st19">
          <g class="st19">
            <g class="st19">
              <g class="st19">
                <g class="st19">
                  <text class="st16" transform="translate(586.93 569.68)"><tspan x="0" y="0">storage file (parquet)</tspan></text>
                </g>
              </g>
            </g>
          </g>
        </g>
      </g>
    </g>
    <g>
      <g>
        <line class="st11" x1="561.1" y1="563.69" x2="575.95" y2="563.69"/>
        <line class="st11" x1="561.09" y1="566.69" x2="575.94" y2="566.69"/>
        <line class="st11" x1="561.12" y1="569.68" x2="575.96" y2="569.68"/>
        <line class="st11" x1="561.11" y1="572.68" x2="575.95" y2="572.68"/>
        <line class="st11" x1="564.5" y1="563.69" x2="564.5" y2="576.37"/>
        <line class="st11" x1="568.51" y1="563.69" x2="568.51" y2="576.37"/>
        <line class="st11" x1="572.73" y1="563.69" x2="572.73" y2="576.37"/>
      </g>
      <path class="st0" d="M561.15,574.8v-19.72c0-.46.37-.82.82-.82h8.22v4.1c0,.91.74,1.65,1.65,1.65h4.1v14.79c0,.46-.37.82-.82.82h-13.15c-.46,0-.82-.37-.82-.82h0ZM561.97,551.8c-1.81,0-3.28,1.47-3.28,3.28v19.72c0,1.81,1.47,3.28,3.28,3.28h13.15c1.81,0,3.28-1.47,3.28-3.28v-15.07c0-.88-.34-1.71-.96-2.33l-4.65-4.65c-.61-.61-1.44-.96-2.32-.96h-8.5Z"/>
    </g>
  </g>
  <g>
    <line class="st9" x1="574.49" y1="444.92" x2="531.28" y2="444.92"/>
    <polygon class="st13" points="534.79 432.95 514.06 444.92 534.79 456.89 534.79 432.95"/>
  </g>
  <g>
    <g>
      <rect class="st24" x="9.13" y="879.82" width="248.29" height="41.92" rx="4.73" ry="4.73"/>
      <rect class="st7" x="9.13" y="879.82" width="248.29" height="41.92" rx="4.73" ry="4.73"/>
    </g>
    <g class="st19">
      <g class="st19">
        <g class="st19">
          <g class="st19">
            <g class="st19">
              <g class="st19">
                <g class="st19">
                  <text class="st16" transform="translate(49.55 905.35)"><tspan x="0" y="0">output spreadsheet (xlsx)</tspan></text>
                </g>
              </g>
            </g>
          </g>
        </g>
      </g>
    </g>
    <g>
      <g>
        <line class="st11" x1="23.72" y1="899.36" x2="38.57" y2="899.36"/>
        <line class="st11" x1="23.71" y1="902.36" x2="38.56" y2="902.36"/>
        <line class="st11" x1="23.74" y1="905.35" x2="38.58" y2="905.35"/>
        <line class="st11" x1="23.73" y1="908.35" x2="38.57" y2="908.35"/>
        <line class="st11" x1="27.12" y1="899.36" x2="27.12" y2="912.04"/>
        <line class="st11" x1="31.13" y1="899.36" x2="31.13" y2="912.04"/>
        <line class="st11" x1="35.35" y1="899.36" x2="35.35" y2="912.04"/>
      </g>
      <path class="st0" d="M23.77,910.47v-19.72c0-.46.37-.82.82-.82h8.22v4.1c0,.91.74,1.65,1.65,1.65h4.1v14.79c0,.46-.37.82-.82.82h-13.15c-.46,0-.82-.37-.82-.82h0ZM24.59,887.47c-1.81,0-3.28,1.47-3.28,3.28v19.72c0,1.81,1.47,3.28,3.28,3.28h13.15c1.81,0,3.28-1.47,3.28-3.28v-15.07c0-.88-.34-1.71-.96-2.33l-4.65-4.65c-.61-.61-1.44-.96-2.32-.96h-8.5Z"/>
      <g class="st19">
        <g class="st19">
          <g class="st19">
            <g class="st19">
              <g class="st19">
                <g class="st19">
                  <text class="st18" transform="translate(30.57 910.05) rotate(-90)"><tspan x="0" y="0">xlsx</tspan></text>
                </g>
              </g>
            </g>
          </g>
        </g>
      </g>
    </g>
  </g>
  <g class="st19">
    <g class="st19">
      <g class="st19">
        <g class="st19">
          <g class="st19">
            <g class="st19">
              <g class="st19">
                <g class="st19">
                  <text class="st15" transform="translate(257.19 949.52)"><tspan x="0" y="0">to downstream processing</tspan></text>
                </g>
              </g>
            </g>
          </g>
        </g>
      </g>
    </g>
  </g>
  <g>
    <polyline class="st1" points="668.18 513.02 720.41 513.02 720.41 527.44"/>
    <polygon class="st13" points="710.44 524.53 720.41 541.8 730.38 524.53 710.44 524.53"/>
  </g>
  <g>
    <g>
      <rect class="st23" x="427.32" y="252.46" width="174.26" height="41.92" rx="4.23" ry="4.23"/>
      <rect class="st6" x="427.32" y="252.46" width="174.26" height="41.92" rx="4.23" ry="4.23"/>
    </g>
    <path class="st0" d="M441.64,273.42l1.12-10.98c.08-.77.61-1.35,1.25-1.35h6.61c.59,0,1.06.58,1.06,1.3,0,.15-.02.31-.07.46l-1.81,5.94h5.46c.79,0,1.44.79,1.44,1.77,0,.36-.09.7-.25,1l-7.53,13.53c-.23.41-.61.66-1.01.66h-.11c-.62,0-1.12-.62-1.12-1.37,0-.11,0-.22.04-.33l1.84-9.08h-5.64c-.69,0-1.25-.69-1.25-1.54h-.03,0Z"/>
    <g class="st19">
      <g class="st19">
        <g class="st19">
          <g class="st19">
            <g class="st19">
              <g class="st19">
                <g class="st19">
                  <g class="st19">
                    <text class="st16" transform="translate(466.18 277.95)"><tspan x="0" y="0">ir_save_isofiles</tspan></text>
                  </g>
                </g>
              </g>
            </g>
          </g>
        </g>
      </g>
    </g>
  </g>
  <g>
    <g>
      <rect class="st23" x="427.32" y="354.01" width="176.26" height="41.92" rx="4.23" ry="4.23"/>
      <rect class="st6" x="427.32" y="354.01" width="176.26" height="41.92" rx="4.23" ry="4.23"/>
    </g>
    <path class="st0" d="M441.64,374.97l1.12-10.98c.08-.77.61-1.35,1.25-1.35h6.61c.59,0,1.06.58,1.06,1.3,0,.15-.02.31-.07.46l-1.81,5.94h5.46c.79,0,1.44.79,1.44,1.77,0,.36-.09.7-.25,1l-7.53,13.53c-.23.41-.61.66-1.01.66h-.11c-.62,0-1.12-.62-1.12-1.37,0-.11,0-.22.04-.33l1.84-9.08h-5.64c-.69,0-1.25-.69-1.25-1.54h-.03,0Z"/>
    <g class="st19">
      <g class="st19">
        <g class="st19">
          <g class="st19">
            <g class="st19">
              <g class="st19">
                <g class="st19">
                  <g class="st19">
                    <text class="st16" transform="translate(466.18 379.5)"><tspan x="0" y="0">ir_load_isofiles</tspan></text>
                  </g>
                </g>
              </g>
            </g>
          </g>
        </g>
      </g>
    </g>
  </g>
  <g>
    <line class="st1" x1="387.3" y1="273.11" x2="409.21" y2="273.11"/>
    <polygon class="st13" points="406.3 283.08 423.57 273.11 406.3 263.14 406.3 283.08"/>
  </g>
  <g>
    <line class="st1" x1="426.57" y1="375.87" x2="404.66" y2="375.87"/>
    <polygon class="st13" points="407.57 365.9 390.3 375.87 407.57 385.84 407.57 365.9"/>
  </g>
  <g>
    <polyline class="st1" points="233.81 72.14 325.05 72.14 325.05 84.56"/>
    <polygon class="st13" points="315.08 81.65 325.05 98.92 335.02 81.65 315.08 81.65"/>
  </g>
  <g>
    <g>
      <rect class="st24" x="546.51" y="303.73" width="182.29" height="41.92" rx="4.73" ry="4.73"/>
      <rect class="st7" x="546.51" y="303.73" width="182.29" height="41.92" rx="4.73" ry="4.73"/>
    </g>
    <g class="st19">
      <g class="st19">
        <g class="st19">
          <g class="st19">
            <g class="st19">
              <g class="st19">
                <g class="st19">
                  <text class="st16" transform="translate(586.93 329.26)"><tspan x="0" y="0">storage file (rds)</tspan></text>
                </g>
              </g>
            </g>
          </g>
        </g>
      </g>
    </g>
    <g>
      <g>
        <line class="st11" x1="561.1" y1="323.27" x2="575.95" y2="323.27"/>
        <line class="st11" x1="561.09" y1="326.27" x2="575.94" y2="326.27"/>
        <line class="st11" x1="561.12" y1="329.26" x2="575.96" y2="329.26"/>
        <line class="st11" x1="561.11" y1="332.26" x2="575.95" y2="332.26"/>
        <line class="st11" x1="564.5" y1="323.27" x2="564.5" y2="335.95"/>
        <line class="st11" x1="568.51" y1="323.27" x2="568.51" y2="335.95"/>
        <line class="st11" x1="572.73" y1="323.27" x2="572.73" y2="335.95"/>
      </g>
      <path class="st0" d="M561.15,334.38v-19.72c0-.46.37-.82.82-.82h8.22v4.1c0,.91.74,1.65,1.65,1.65h4.1v14.79c0,.46-.37.82-.82.82h-13.15c-.46,0-.82-.37-.82-.82h0ZM561.97,311.38c-1.81,0-3.28,1.47-3.28,3.28v19.72c0,1.81,1.47,3.28,3.28,3.28h13.15c1.81,0,3.28-1.47,3.28-3.28v-15.07c0-.88-.34-1.71-.96-2.33l-4.65-4.65c-.61-.61-1.44-.96-2.32-.96h-8.5Z"/>
    </g>
  </g>
  <g>
    <polyline class="st1" points="602.18 271.6 658.41 271.6 658.41 286.02"/>
    <polygon class="st13" points="648.44 283.11 658.41 300.38 668.38 283.11 648.44 283.11"/>
  </g>
</svg>

```




## Getting help

If you encounter a bug, please file an issue with a minimal reproducible example on [GitHub](https://github.com/isoverse/isoreader2/issues). Example files are very helpful for fixing bugs so please consider including an example data file (you will have to attach it as a zip archive).

## isoverse <a href='http://www.isoverse.org'><img src='man/figures/isoverse_logo_thumb.png' align="right" width="100" alt="isoverse logo"/></a>

This package is part of the isoverse suite of data tools for stable isotopes. If you like the functionality that isoverse packages provide, please help us spread the word and include an isoverse or individual package logo on one of your posters or slides. All logos are posted in high resolution in [this repository](https://github.com/isoverse/logos). If you have suggestions for new features or other constructive feedback, please let us know on this short [feeback form](https://www.isoverse.org/feedback/).

## Funding <a href='https://www.nsf.gov/'><img src='man/figures/NSF_logo.svg' align="right" width="100" alt="NSF logo"/></a>

This project is supported by a grant from the US National Science Foundation ([EAR-2411458](https://www.nsf.gov/awardsearch/show-award?AWD_ID=2411458)) to Sebastian Kopf. 

