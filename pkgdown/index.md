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

### Read data files




``` r
# load library
library(isoreader2)
Loading required package: ggplot2

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
```

<PRE class="fansi fansi-message"><CODE><span style='color: #00BB00;'>✔</span> <span style='color: #B2B2B2;'>[337ms]</span> <span style='font-weight: bold;'>ir_extract_isofiles()</span> finished extracting 2 files/archives
</CODE></PRE><PRE class="fansi fansi-message"><CODE><span style='color: #00BB00;'>✔</span> <span style='color: #B2B2B2;'>[159ms]</span> <span style='font-weight: bold;'>ir_read_isofiles()</span> finished reading 2 isotope data files/archives
</CODE></PRE>

``` r
# show information about the files
isofiles
```

<PRE class="fansi fansi-message"><CODE>─────── <span style='font-weight: bold;'>2 isofiles with 2 analyses - combine with ir_aggregate_isofiles()</span> ──────
</CODE></PRE><PRE class="fansi fansi-message"><CODE>1. <span style='color: #0000BB;'>continuous_flow_ea_example.dxf</span>: with <span style='color: #00BBBB;'>1.1k</span> time points for <span style='color: #BB00BB;'>N2</span> (masses <span style='color: #00BB00;'>28</span>, <span style='color: #00BB00;'>29</span>,
and <span style='color: #00BB00;'>30</span>); <span style='color: #00BBBB;'>1.34k</span> time points for <span style='color: #BB00BB;'>CO2</span> (masses <span style='color: #00BB00;'>44</span>, <span style='color: #00BB00;'>45</span>, and <span style='color: #00BB00;'>46</span>); <span style='color: #00BBBB;'>20</span> metadata columns
2. <span style='color: #0000BB;'>continuous_flow_gc_example.cf</span>:   with <span style='color: #00BBBB;'>8.6k</span> time points for <span style='color: #BB00BB;'>HD</span> (masses <span style='color: #00BB00;'>2</span> and
<span style='color: #00BB00;'>3</span>); <span style='color: #00BBBB;'>19</span> metadata columns
</CODE></PRE>

``` r
# aggregate the data from the read files specifying which units to use
# (mV, V, nA, A, cps, etc.), conversion via resistor values happens automatically
dataset <- isofiles |> ir_aggregate_isofiles("mV")
```

<PRE class="fansi fansi-message"><CODE><span style='color: #00BB00;'>✔</span> <span style='color: #B2B2B2;'>[87ms]</span> <span style='font-weight: bold;'>ir_aggregate_isofiles()</span> aggregated <span style='color: #0000BB;'>metadata</span> (2) and <span style='color: #0000BB;'>traces</span> (24.5k,
<span style='color: #00BB00;'>intensity</span> in <span style='color: #BB00BB;'>mV</span>) from 2 files using the <span style='font-weight: bold; font-style: italic;'>standard</span> aggregator
</CODE></PRE>

``` r
# show the available data that was aggregated  metadata is all the available
# sequence information from the different file types
dataset
```

<PRE class="fansi fansi-message"><CODE>─ <span style='font-weight: bold;'>aggregated data from 2 isofiles with 2 analyses - retrieve with ir_get_data(</span> ─
</CODE></PRE><PRE class="fansi fansi-message"><CODE>→ <span style='color: #0000BB;'>metadata</span> (2): <span style='color: #00BB00;'>uidx</span>, <span style='color: #00BB00;'>file_path</span>, <span style='color: #00BB00;'>file_name</span>, <span style='color: #00BB00;'>analysis</span>, <span style='color: #00BB00;'>timestamp</span>, <span style='color: #00BB00;'>type</span>,
<span style='color: #00BB00;'>h3_factor</span> (<span style='color: #BBBB00;'>1 NA</span>), <span style='color: #00BB00;'>Row</span> (<span style='color: #BBBB00;'>1 NA</span>), <span style='color: #00BB00;'>Peak Center</span> (<span style='color: #BBBB00;'>1 NA</span>), <span style='color: #00BB00;'>Check Ref. Dilution</span> (<span style='color: #BBBB00;'>1 NA</span>),
<span style='color: #00BB00;'>H3 Stability</span> (<span style='color: #BBBB00;'>1 NA</span>), <span style='color: #00BB00;'>H3 Factor</span> (<span style='color: #BBBB00;'>1 NA</span>), <span style='color: #00BB00;'>Amount</span> (<span style='color: #BBBB00;'>1 NA</span>), <span style='color: #00BB00;'>Type</span> (<span style='color: #BBBB00;'>1 NA</span>), <span style='color: #00BB00;'>EA Method</span> (<span style='color: #BBBB00;'>1</span>
<span style='color: #BBBB00;'>NA</span>), <span style='color: #00BB00;'>Identifier 1</span>, <span style='color: #00BB00;'>Identifier 2</span> (<span style='color: #BBBB00;'>1 NA</span>), <span style='color: #00BB00;'>Analysis</span>, <span style='color: #00BB00;'>Comment</span> (<span style='color: #BBBB00;'>1 NA</span>), <span style='color: #00BB00;'>Preparation</span>
(<span style='color: #BBBB00;'>1 NA</span>), <span style='color: #00BB00;'>Method</span>, <span style='color: #00BB00;'>Line</span> (<span style='color: #BBBB00;'>1 NA</span>), <span style='color: #00BB00;'>GC Method</span> (<span style='color: #BBBB00;'>1 NA</span>), <span style='color: #00BB00;'>AS Sample</span> (<span style='color: #BBBB00;'>1 NA</span>), <span style='color: #00BB00;'>AS Method</span> (<span style='color: #BBBB00;'>1</span>
<span style='color: #BBBB00;'>NA</span>), <span style='color: #00BB00;'>Pre Script</span> (<span style='color: #BBBB00;'>all NA</span>), <span style='color: #00BB00;'>Post Script</span> (<span style='color: #BBBB00;'>all NA</span>)
</CODE></PRE><PRE class="fansi fansi-message"><CODE>→ <span style='color: #0000BB;'>traces</span> (24.5k): <span style='color: #00BB00;'>uidx</span>, <span style='color: #00BB00;'>analysis</span>, <span style='color: #00BB00;'>species</span>, <span style='color: #00BB00;'>mass</span>, <span style='color: #00BB00;'>trace</span>, <span style='color: #00BB00;'>time.s</span>, <span style='color: #00BB00;'>intensity.mV</span>;
(<span style='font-style: italic;'>not aggregated</span>: <span style='color: #BBBB00; font-style: italic;'>channel</span>)
</CODE></PRE><PRE class="fansi fansi-message"><CODE>→ <span style='color: #0000BB;'>problems</span>: has <span style='color: #00BB00;'>no issues</span>
</CODE></PRE>

``` r
# plot the data  with the default plotting settings
dataset |> ir_plot_continuous_flow()
```

<div class="figure">
<img src="man/figures/README-unnamed-chunk-3-1.png" alt="plot of chunk unnamed-chunk-3" width="100%" />
<p class="caption">plot of chunk unnamed-chunk-3</p>
</div>

### Export the data


``` r
# the file metadata
dataset |>
  ir_export_to_excel(
    include = c("metadata", "traces"),
    file = "my_export.xlsx"
  )
```

<PRE class="fansi fansi-message"><CODE><span style='color: #00BB00;'>✔</span> <span style='color: #B2B2B2;'>[643ms]</span> <span style='font-weight: bold;'>ir_export_to_excel()</span> exported 2 rows of <span style='color: #00BB00;'>metadata</span> and 24.5k rows of
<span style='color: #00BB00;'>traces</span> to <span style='color: #0000BB;'>my_export.xlsx</span>
</CODE></PRE>



## Package structure

<p>Click on the individual functions to jump straight to their documenation.</p>

```{=html}
<?xml version="1.0" encoding="UTF-8"?>
<svg id="Layer_1" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" version="1.1" viewBox="0 0 765.22 978.73">
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
  <rect class="st22" width="765.22" height="978.73"/>
  <g>
    <g>
      <rect class="st23" x="459.84" y="714.99" width="200.29" height="41.92" rx="4.23" ry="4.23"/>
      <rect class="st6" x="459.84" y="714.99" width="200.29" height="41.92" rx="4.23" ry="4.23"/>
    </g>
    <path class="st0" d="M474.15,735.95l1.12-10.98c.08-.77.61-1.35,1.25-1.35h6.61c.59,0,1.06.58,1.06,1.3,0,.15-.02.31-.07.46l-1.81,5.94h5.46c.79,0,1.44.79,1.44,1.77,0,.36-.09.7-.25,1l-7.53,13.53c-.23.41-.61.66-1.01.66h-.11c-.62,0-1.12-.62-1.12-1.37,0-.11,0-.22.04-.33l1.84-9.08h-5.64c-.69,0-1.25-.69-1.25-1.54h-.03,0Z"/>
    <g class="st19">
      <g class="st19">
        <g class="st19">
          <g class="st19">
            <g class="st19">
              <g class="st19">
                <g class="st19">
                  <g class="st19">
                    <text class="st16" transform="translate(498.7 740.48)"><tspan x="0" y="0">ir_filter</tspan></text>
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
      <rect class="st23" x="459.84" y="763.11" width="200.29" height="41.92" rx="4.23" ry="4.23"/>
      <rect class="st6" x="459.84" y="763.11" width="200.29" height="41.92" rx="4.23" ry="4.23"/>
    </g>
    <path class="st0" d="M474.15,784.07l1.12-10.98c.08-.77.61-1.35,1.25-1.35h6.61c.59,0,1.06.58,1.06,1.3,0,.15-.02.31-.07.46l-1.81,5.94h5.46c.79,0,1.44.79,1.44,1.77,0,.36-.09.7-.25,1l-7.53,13.53c-.23.41-.61.66-1.01.66h-.11c-.62,0-1.12-.62-1.12-1.37,0-.11,0-.22.04-.33l1.84-9.08h-5.64c-.69,0-1.25-.69-1.25-1.54h-.03,0Z"/>
    <g class="st19">
      <g class="st19">
        <g class="st19">
          <g class="st19">
            <g class="st19">
              <g class="st19">
                <g class="st19">
                  <g class="st19">
                    <text class="st16" transform="translate(498.7 788.6)"><tspan x="0" y="0">ir_join_metadata</tspan></text>
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
      <rect class="st23" x="9.89" y="631.58" width="234.17" height="41.92" rx="4.23" ry="4.23"/>
      <rect class="st6" x="9.89" y="631.58" width="234.17" height="41.92" rx="4.23" ry="4.23"/>
    </g>
    <path class="st0" d="M24.21,652.54l1.12-10.98c.08-.77.61-1.35,1.25-1.35h6.61c.59,0,1.06.58,1.06,1.3,0,.15-.02.31-.07.46l-1.81,5.94h5.46c.79,0,1.44.79,1.44,1.77,0,.36-.09.7-.25,1l-7.53,13.53c-.23.41-.61.66-1.01.66h-.11c-.62,0-1.12-.62-1.12-1.37,0-.11,0-.22.04-.33l1.84-9.08h-5.64c-.69,0-1.25-.69-1.25-1.54h-.03Z"/>
    <g class="st19">
      <g class="st19">
        <g class="st19">
          <g class="st19">
            <g class="st19">
              <g class="st19">
                <g class="st19">
                  <g class="st19">
                    <text class="st16" transform="translate(49.08 657.07)"><tspan x="0" y="0">ir_export_data_to_excel</tspan></text>
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
      <rect class="st23" x="427.99" y="493.88" width="240.26" height="41.92" rx="4.23" ry="4.23"/>
      <rect class="st6" x="427.99" y="493.88" width="240.26" height="41.92" rx="4.23" ry="4.23"/>
    </g>
    <path class="st0" d="M442.31,514.84l1.12-10.98c.08-.77.61-1.35,1.25-1.35h6.61c.59,0,1.06.58,1.06,1.3,0,.15-.02.31-.07.46l-1.81,5.94h5.46c.79,0,1.44.79,1.44,1.77,0,.36-.09.7-.25,1l-7.53,13.53c-.23.41-.61.66-1.01.66h-.11c-.62,0-1.12-.62-1.12-1.37,0-.11,0-.22.04-.33l1.84-9.08h-5.64c-.69,0-1.25-.69-1.25-1.54h-.03,0Z"/>
    <g class="st19">
      <g class="st19">
        <g class="st19">
          <g class="st19">
            <g class="st19">
              <g class="st19">
                <g class="st19">
                  <g class="st19">
                    <text class="st16" transform="translate(466.85 519.37)"><tspan x="0" y="0">ir_save_aggregated_data</tspan></text>
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
      <rect class="st23" x="427.99" y="594.43" width="240.26" height="41.92" rx="4.23" ry="4.23"/>
      <rect class="st6" x="427.99" y="594.43" width="240.26" height="41.92" rx="4.23" ry="4.23"/>
    </g>
    <path class="st0" d="M442.31,615.39l1.12-10.98c.08-.77.61-1.35,1.25-1.35h6.61c.59,0,1.06.58,1.06,1.3,0,.15-.02.31-.07.46l-1.81,5.94h5.46c.79,0,1.44.79,1.44,1.77,0,.36-.09.7-.25,1l-7.53,13.53c-.23.41-.61.66-1.01.66h-.11c-.62,0-1.12-.62-1.12-1.37,0-.11,0-.22.04-.33l1.84-9.08h-5.64c-.69,0-1.25-.69-1.25-1.54h-.03,0Z"/>
    <g class="st19">
      <g class="st19">
        <g class="st19">
          <g class="st19">
            <g class="st19">
              <g class="st19">
                <g class="st19">
                  <g class="st19">
                    <text class="st16" transform="translate(466.85 619.92)"><tspan x="0" y="0">ir_load_aggregated_data</tspan></text>
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
      <rect class="st23" x="459.84" y="666.85" width="200.29" height="41.92" rx="4.23" ry="4.23"/>
      <rect class="st6" x="459.84" y="666.85" width="200.29" height="41.92" rx="4.23" ry="4.23"/>
    </g>
    <path class="st0" d="M474.15,687.81l1.12-10.98c.08-.77.61-1.35,1.25-1.35h6.61c.59,0,1.06.58,1.06,1.3,0,.15-.02.31-.07.46l-1.81,5.94h5.46c.79,0,1.44.79,1.44,1.77,0,.36-.09.7-.25,1l-7.53,13.53c-.23.41-.61.66-1.01.66h-.11c-.62,0-1.12-.62-1.12-1.37,0-.11,0-.22.04-.33l1.84-9.08h-5.64c-.69,0-1.25-.69-1.25-1.54h-.03,0Z"/>
    <g class="st19">
      <g class="st19">
        <g class="st19">
          <g class="st19">
            <g class="st19">
              <g class="st19">
                <g class="st19">
                  <g class="st19">
                    <text class="st16" transform="translate(498.7 692.34)"><tspan x="0" y="0">ir_filter_metadata</tspan></text>
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
      <rect class="st23" x="459.84" y="714.97" width="200.29" height="41.92" rx="4.23" ry="4.23"/>
      <rect class="st6" x="459.84" y="714.97" width="200.29" height="41.92" rx="4.23" ry="4.23"/>
    </g>
    <path class="st0" d="M474.15,735.93l1.12-10.98c.08-.77.61-1.35,1.25-1.35h6.61c.59,0,1.06.58,1.06,1.3,0,.15-.02.31-.07.46l-1.81,5.94h5.46c.79,0,1.44.79,1.44,1.77,0,.36-.09.7-.25,1l-7.53,13.53c-.23.41-.61.66-1.01.66h-.11c-.62,0-1.12-.62-1.12-1.37,0-.11,0-.22.04-.33l1.84-9.08h-5.64c-.69,0-1.25-.69-1.25-1.54h-.03,0Z"/>
    <g class="st19">
      <g class="st19">
        <g class="st19">
          <g class="st19">
            <g class="st19">
              <g class="st19">
                <g class="st19">
                  <g class="st19">
                    <text class="st16" transform="translate(498.7 740.46)"><tspan x="0" y="0">ir_mutate_metadata</tspan></text>
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
    <line class="st9" x1="384.62" y1="466.46" x2="384.62" y2="838.58"/>
    <polygon class="st13" points="372.65 835.08 384.62 855.8 396.59 835.08 372.65 835.08"/>
  </g>
  <g>
    <line class="st9" x1="384.18" y1="64.12" x2="384.18" y2="84.67"/>
    <polygon class="st13" points="372.22 81.17 384.18 101.89 396.15 81.17 372.22 81.17"/>
  </g>
  <g>
    <line class="st9" x1="384.18" y1="150.15" x2="384.18" y2="170.7"/>
    <polygon class="st13" points="372.22 167.2 384.18 187.92 396.15 167.2 372.22 167.2"/>
  </g>
  <g>
    <line class="st9" x1="384.18" y1="236.95" x2="384.18" y2="400.5"/>
    <polygon class="st13" points="372.22 397 384.18 417.72 396.15 397 372.22 397"/>
  </g>
  <g>
    <line class="st1" x1="386.86" y1="775.7" x2="443.39" y2="775.7"/>
    <polygon class="st13" points="440.47 785.67 457.75 775.7 440.47 765.73 440.47 785.67"/>
  </g>
  <g>
    <line class="st1" x1="459.03" y1="791.15" x2="402.51" y2="791.15"/>
    <polygon class="st13" points="405.4 781.18 388.15 791.15 405.4 801.12 405.4 781.18"/>
  </g>
  <g>
    <line class="st1" x1="386.86" y1="728.72" x2="443.39" y2="728.72"/>
    <polygon class="st13" points="440.47 738.69 457.75 728.72 440.47 718.75 440.47 738.69"/>
  </g>
  <g>
    <line class="st1" x1="459.03" y1="744.17" x2="402.51" y2="744.17"/>
    <polygon class="st13" points="405.4 734.2 388.15 744.17 405.4 754.14 405.4 734.2"/>
  </g>
  <g>
    <line class="st1" x1="386.86" y1="682.99" x2="443.39" y2="682.99"/>
    <polygon class="st13" points="440.47 692.96 457.75 682.99 440.47 673.02 440.47 692.96"/>
  </g>
  <g>
    <line class="st1" x1="459.03" y1="698.44" x2="402.51" y2="698.44"/>
    <polygon class="st13" points="405.4 688.47 388.15 698.44 405.4 708.41 405.4 688.47"/>
  </g>
  <g>
    <line class="st1" x1="530.04" y1="217.65" x2="493.52" y2="217.65"/>
    <polygon class="st13" points="496.42 207.68 479.15 217.65 496.42 227.62 496.42 207.68"/>
  </g>
  <g>
    <line class="st1" x1="387.97" y1="514.53" x2="409.88" y2="514.53"/>
    <polygon class="st13" points="406.97 524.5 424.24 514.53 406.97 504.56 406.97 524.5"/>
  </g>
  <g>
    <line class="st1" x1="427.24" y1="616.29" x2="405.33" y2="616.29"/>
    <polygon class="st13" points="408.24 606.32 390.97 616.29 408.24 626.26 408.24 606.32"/>
  </g>
  <g>
    <line class="st1" x1="76.27" y1="674.27" x2="76.27" y2="880.18"/>
    <polygon class="st13" points="66.3 877.27 76.27 894.54 86.24 877.27 66.3 877.27"/>
  </g>
  <g>
    <line class="st1" x1="384.77" y1="505.54" x2="323.86" y2="505.54"/>
    <polygon class="st13" points="326.77 495.57 309.5 505.54 326.77 515.51 326.77 495.57"/>
  </g>
  <g>
    <line class="st1" x1="384.77" y1="711.59" x2="323.86" y2="711.59"/>
    <polygon class="st13" points="326.77 701.62 309.5 711.59 326.77 721.56 326.77 701.62"/>
  </g>
  <g>
    <line class="st1" x1="384.77" y1="643.07" x2="266.86" y2="643.07"/>
    <polygon class="st13" points="269.77 633.1 252.5 643.07 269.77 653.04 269.77 633.1"/>
  </g>
  <g>
    <polyline class="st1" points="527.02 36.65 583.24 36.65 583.24 70.07"/>
    <polygon class="st13" points="573.28 67.16 583.24 84.43 593.22 67.16 573.28 67.16"/>
  </g>
  <g>
    <polyline class="st1" points="266.53 663.99 287.95 663.99 287.95 691.22"/>
    <polygon class="st13" points="269.43 654.02 252.17 663.99 269.43 673.96 269.43 654.02"/>
  </g>
  <g>
    <polyline class="st1" points="721.66 586.72 721.66 615.96 688.23 615.96"/>
    <polygon class="st13" points="691.15 605.99 673.88 615.96 691.15 625.93 691.15 605.99"/>
  </g>
  <g>
    <polygon class="st4" points="666.68 414.34 604.52 414.34 573.43 444.92 604.52 475.5 666.68 475.5 697.77 444.92 666.68 414.34"/>
    <g class="st19">
      <g class="st19">
        <g class="st19">
          <g class="st19">
            <g class="st19">
              <g class="st19">
                <g class="st19">
                  <g class="st19">
                    <text class="st16" transform="translate(616.79 440.65)"><tspan x="0" y="0">V or A</tspan></text>
                  </g>
                </g>
                <g class="st19">
                  <g class="st19">
                    <text class="st16" transform="translate(616.79 461.05)"><tspan x="0" y="0">or cps</tspan></text>
                  </g>
                </g>
              </g>
            </g>
          </g>
        </g>
      </g>
    </g>
    <path class="st0" d="M598.1,434.67c0-1.41,1.15-2.56,2.56-2.56s2.56,1.15,2.56,2.56-1.15,2.56-2.56,2.56-2.56-1.15-2.56-2.56ZM595.53,442.36c0-.94.76-1.71,1.71-1.71h3.42c.94,0,1.71.76,1.71,1.71v11.96h1.71c.94,0,1.71.76,1.71,1.71s-.76,1.71-1.71,1.71h-6.83c-.94,0-1.71-.76-1.71-1.71s.76-1.71,1.71-1.71h1.71v-10.25h-1.71c-.94,0-1.71-.76-1.71-1.71h-.01Z"/>
  </g>
  <g>
    <g>
      <rect class="st17" x="2.86" y="128.96" width="283.19" height="322.82" rx="16.31" ry="16.31"/>
      <rect class="st5" x="2.86" y="128.96" width="283.19" height="322.82" rx="16.31" ry="16.31"/>
    </g>
    <g class="st19">
      <g class="st19">
        <g class="st19">
          <g class="st19">
            <g class="st19">
              <g class="st19">
                <g class="st19">
                  <g class="st19">
                    <text class="st14" transform="translate(64.22 334.76)"><tspan x="0" y="0">information functions</tspan></text>
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
                    <text class="st14" transform="translate(64.22 295.32)"><tspan x="0" y="0">processing functions</tspan></text>
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
                    <text class="st14" transform="translate(63.02 208.94)"><tspan x="0" y="0">isoreader core functions</tspan></text>
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
                    <text class="st14" transform="translate(63.02 238.7)"><tspan x="0" y="0">auxiliary functions</tspan></text>
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
                    <text class="st14" transform="translate(63.02 262.63)"><tspan x="0" y="0">(optional)</tspan></text>
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
                    <text class="st14" transform="translate(64.22 374.93)"><tspan x="0" y="0">visualization functions</tspan></text>
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
                    <text class="st14" transform="translate(64.22 409.71)"><tspan x="0" y="0">core functions, essential</tspan></text>
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
                    <text class="st14" transform="translate(64.22 433.64)"><tspan x="0" y="0">input from user</tspan></text>
                  </g>
                </g>
              </g>
            </g>
          </g>
        </g>
      </g>
    </g>
    <path class="st0" d="M24.36,289.99l1.23-12.12c.09-.85.67-1.49,1.38-1.49h7.29c.65,0,1.17.64,1.17,1.44,0,.17-.03.35-.07.5l-2,6.56h6.03c.87,0,1.59.87,1.59,1.95,0,.39-.1.78-.28,1.1l-8.31,14.93c-.26.46-.67.73-1.12.73h-.13c-.68,0-1.23-.68-1.23-1.51,0-.12.01-.24.04-.37l2.03-10.03h-6.23c-.77,0-1.38-.76-1.38-1.7h-.01Z"/>
    <g>
      <rect class="st23" x="18.12" y="188" width="31.26" height="31.26" rx="5.69" ry="5.69"/>
      <rect class="st7" x="18.12" y="188" width="31.26" height="31.26" rx="5.69" ry="5.69"/>
    </g>
    <g>
      <rect class="st23" x="18.12" y="229.72" width="31.26" height="31.26" rx="5.69" ry="5.69"/>
      <rect class="st10" x="18.12" y="229.72" width="31.26" height="31.26" rx="5.69" ry="5.69"/>
    </g>
    <g>
      <polygon class="st3" points="35.1 398.74 33.85 398.74 15.94 416.36 33.85 433.99 35.1 433.99 53.02 416.36 35.1 398.74"/>
      <path class="st0" d="M33.22,410.45c0-.82.66-1.48,1.48-1.48s1.48.66,1.48,1.48-.66,1.48-1.48,1.48-1.48-.66-1.48-1.48ZM31.74,414.89c0-.54.44-.98.98-.98h1.97c.54,0,.98.44.98.98v6.89h.98c.54,0,.98.44.98.98s-.44.98-.98.98h-3.94c-.54,0-.98-.44-.98-.98s.44-.98.98-.98h.98v-5.91h-.98c-.54,0-.98-.44-.98-.98h0Z"/>
    </g>
    <path class="st0" d="M32.66,343.98c8.03,0,14.54-6.51,14.54-14.54s-6.51-14.54-14.54-14.54-14.54,6.51-14.54,14.54,6.51,14.54,14.54,14.54ZM30.39,333.98h1.36v-3.63h-1.36c-.76,0-1.36-.61-1.36-1.36s.61-1.36,1.36-1.36h2.73c.76,0,1.36.61,1.36,1.36v5h.45c.76,0,1.36.61,1.36,1.36s-.61,1.36-1.36,1.36h-4.54c-.76,0-1.36-.61-1.36-1.36s.61-1.36,1.36-1.36h0ZM32.66,322.17c1,0,1.82.81,1.82,1.82s-.81,1.82-1.82,1.82-1.82-.81-1.82-1.82.81-1.82,1.82-1.82Z"/>
    <path class="st0" d="M22.68,359.63c0-.92-.74-1.66-1.66-1.66s-1.66.74-1.66,1.66v17.47c0,2.3,1.86,4.16,4.16,4.16h20.8c.92,0,1.66-.74,1.66-1.66s-.74-1.66-1.66-1.66h-20.8c-.46,0-.83-.37-.83-.83v-17.47h-.01,0ZM43.82,364.13c.65-.65.65-1.71,0-2.36s-1.71-.65-2.36,0l-5.47,5.48-2.98-2.98c-.65-.65-1.71-.65-2.36,0l-5.82,5.82c-.65.65-.65,1.71,0,2.36s1.71.65,2.36,0l4.65-4.64,2.98,2.98c.65.65,1.71.65,2.36,0l6.65-6.65h-.01Z"/>
    <g class="st19">
      <g class="st19">
        <g class="st19">
          <g class="st19">
            <g class="st19">
              <g class="st19">
                <g class="st19">
                  <g class="st19">
                    <text class="st14" transform="translate(64.15 166.75)"><tspan x="0" y="0">data files</tspan></text>
                  </g>
                </g>
              </g>
            </g>
          </g>
        </g>
      </g>
    </g>
    <g>
      <rect class="st24" x="17.8" y="145.16" width="32.52" height="32.52" rx="1.55" ry="1.55"/>
      <rect class="st8" x="17.8" y="145.16" width="32.52" height="32.52" rx="1.55" ry="1.55"/>
    </g>
  </g>
  <a xlink:href="https://isoreader2.isoverse.org/reference/ir_read_isofiles.html">
    <g>
      <g>
        <rect class="st23" x="301.04" y="194.41" width="172.29" height="41.92" rx="4.23" ry="4.23"/>
        <rect class="st7" x="301.04" y="194.41" width="172.29" height="41.92" rx="4.23" ry="4.23"/>
      </g>
      <path class="st0" d="M315.36,215.37l1.12-10.98c.08-.77.61-1.35,1.25-1.35h6.61c.59,0,1.06.58,1.06,1.3,0,.15-.02.31-.07.46l-1.81,5.94h5.46c.79,0,1.44.79,1.44,1.77,0,.36-.09.7-.25,1l-7.53,13.53c-.23.41-.61.66-1.01.66h-.11c-.62,0-1.12-.62-1.12-1.37,0-.11.01-.22.04-.33l1.84-9.08h-5.64c-.69,0-1.25-.69-1.25-1.54h-.03,0Z"/>
      <g class="st19">
        <g class="st19">
          <g class="st19">
            <g class="st19">
              <g class="st19">
                <g class="st19">
                  <g class="st19">
                    <g class="st19">
                      <text class="st16" transform="translate(340.89 219.91)"><tspan x="0" y="0">ir_read_isofiles</tspan></text>
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
        <rect class="st23" x="301.04" y="107.86" width="172.29" height="41.92" rx="4.23" ry="4.23"/>
        <rect class="st7" x="301.04" y="107.86" width="172.29" height="41.92" rx="4.23" ry="4.23"/>
      </g>
      <path class="st0" d="M315.36,128.82l1.12-10.98c.08-.77.61-1.35,1.25-1.35h6.61c.59,0,1.06.58,1.06,1.3,0,.15-.02.31-.07.46l-1.81,5.94h5.46c.79,0,1.44.79,1.44,1.77,0,.36-.09.7-.25,1l-7.53,13.53c-.23.41-.61.66-1.01.66h-.11c-.62,0-1.12-.62-1.12-1.37,0-.11.01-.22.04-.33l1.84-9.08h-5.64c-.69,0-1.25-.69-1.25-1.54h-.03,0Z"/>
      <g class="st19">
        <g class="st19">
          <g class="st19">
            <g class="st19">
              <g class="st19">
                <g class="st19">
                  <g class="st19">
                    <g class="st19">
                      <text class="st16" transform="translate(340.89 133.36)"><tspan x="0" y="0">ir_find_isofiles</tspan></text>
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
        <rect class="st23" x="300.85" y="423.96" width="207.29" height="41.92" rx="4.23" ry="4.23"/>
        <rect class="st7" x="300.85" y="423.96" width="207.29" height="41.92" rx="4.23" ry="4.23"/>
      </g>
      <path class="st0" d="M315.17,444.92l1.12-10.98c.08-.77.61-1.35,1.25-1.35h6.61c.59,0,1.06.58,1.06,1.3,0,.15-.02.31-.07.46l-1.81,5.94h5.46c.79,0,1.44.79,1.44,1.77,0,.36-.09.7-.25,1l-7.53,13.53c-.23.41-.61.66-1.01.66h-.11c-.62,0-1.12-.62-1.12-1.37,0-.11.01-.22.04-.33l1.84-9.08h-5.64c-.69,0-1.25-.69-1.25-1.54h-.03,0Z"/>
      <g class="st19">
        <g class="st19">
          <g class="st19">
            <g class="st19">
              <g class="st19">
                <g class="st19">
                  <g class="st19">
                    <g class="st19">
                      <text class="st16" transform="translate(339.7 449.46)"><tspan x="0" y="0">ir_aggregate_isofiles</tspan></text>
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
        <path class="st23" d="M73.32,580.79h215.03c3.12,0,11.65,2.53,11.65,5.65v30.62c0,3.12-8.53,5.65-11.65,5.65H73.32c-3.12,0-5.65-2.53-5.65-5.65v-30.62c0-3.12,2.53-5.65,5.65-5.65Z"/>
        <path class="st6" d="M73.32,580.79h215.03c3.12,0,11.65,2.53,11.65,5.65v30.62c0,3.12-8.53,5.65-11.65,5.65H73.32c-3.12,0-5.65-2.53-5.65-5.65v-30.62c0-3.12,2.53-5.65,5.65-5.65Z"/>
      </g>
      <g class="st19">
        <g class="st19">
          <g class="st19">
            <g class="st19">
              <g class="st19">
                <g class="st19">
                  <g class="st19">
                    <g class="st19">
                      <text class="st16" transform="translate(112.52 606.28)"><tspan x="0" y="0">ir_plot_scans</tspan></text>
                    </g>
                  </g>
                </g>
              </g>
            </g>
          </g>
        </g>
      </g>
      <path class="st0" d="M77.96,591.58c0-.92-.74-1.66-1.66-1.66s-1.66.74-1.66,1.66v17.47c0,2.3,1.86,4.16,4.16,4.16h20.8c.92,0,1.66-.74,1.66-1.66s-.74-1.66-1.66-1.66h-20.8c-.46,0-.83-.37-.83-.83v-17.47h-.01,0ZM99.1,596.08c.65-.65.65-1.71,0-2.36s-1.71-.65-2.36,0l-5.47,5.48-2.98-2.98c-.65-.65-1.71-.65-2.36,0l-5.82,5.82c-.65.65-.65,1.71,0,2.36s1.71.65,2.36,0l4.65-4.64,2.98,2.98c.65.65,1.71.65,2.36,0l6.65-6.65h-.01Z"/>
    </g>
  </a>
  <g>
    <line class="st1" x1="384.77" y1="554.35" x2="323.86" y2="554.35"/>
    <polygon class="st13" points="326.77 544.38 309.5 554.35 326.77 564.32 326.77 544.38"/>
  </g>
  <a xlink:href="https://isoreader2.isoverse.org/reference/ir_plot_dual_inlet.html">
    <g>
      <g>
        <path class="st23" d="M73.32,531.99h215.03c3.12,0,11.65,2.53,11.65,5.65v30.62c0,3.12-8.53,5.65-11.65,5.65H73.32c-3.12,0-5.65-2.53-5.65-5.65v-30.62c0-3.12,2.53-5.65,5.65-5.65Z"/>
        <path class="st6" d="M73.32,531.99h215.03c3.12,0,11.65,2.53,11.65,5.65v30.62c0,3.12-8.53,5.65-11.65,5.65H73.32c-3.12,0-5.65-2.53-5.65-5.65v-30.62c0-3.12,2.53-5.65,5.65-5.65Z"/>
      </g>
      <g class="st19">
        <g class="st19">
          <g class="st19">
            <g class="st19">
              <g class="st19">
                <g class="st19">
                  <g class="st19">
                    <g class="st19">
                      <text class="st16" transform="translate(112.52 557.48)"><tspan x="0" y="0">ir_plot_dual_inlet</tspan></text>
                    </g>
                  </g>
                </g>
              </g>
            </g>
          </g>
        </g>
      </g>
      <path class="st0" d="M77.96,542.78c0-.92-.74-1.66-1.66-1.66s-1.66.74-1.66,1.66v17.47c0,2.3,1.86,4.16,4.16,4.16h20.8c.92,0,1.66-.74,1.66-1.66s-.74-1.66-1.66-1.66h-20.8c-.46,0-.83-.37-.83-.83v-17.47h-.01,0ZM99.1,547.28c.65-.65.65-1.71,0-2.36s-1.71-.65-2.36,0l-5.47,5.48-2.98-2.98c-.65-.65-1.71-.65-2.36,0l-5.82,5.82c-.65.65-.65,1.71,0,2.36s1.71.65,2.36,0l4.65-4.64,2.98,2.98c.65.65,1.71.65,2.36,0l6.65-6.65h-.01Z"/>
    </g>
  </a>
  <g>
    <line class="st1" x1="384.77" y1="602.22" x2="323.86" y2="602.22"/>
    <polygon class="st13" points="326.77 592.25 309.5 602.22 326.77 612.19 326.77 592.25"/>
  </g>
  <a xlink:href="https://isoreader2.isoverse.org/reference/ir_plot_continuous_flow.html">
    <g>
      <g>
        <path class="st23" d="M73.32,484.11h215.03c3.12,0,11.65,2.53,11.65,5.65v30.62c0,3.12-8.53,5.65-11.65,5.65H73.32c-3.12,0-5.65-2.53-5.65-5.65v-30.62c0-3.12,2.53-5.65,5.65-5.65Z"/>
        <path class="st6" d="M73.32,484.11h215.03c3.12,0,11.65,2.53,11.65,5.65v30.62c0,3.12-8.53,5.65-11.65,5.65H73.32c-3.12,0-5.65-2.53-5.65-5.65v-30.62c0-3.12,2.53-5.65,5.65-5.65Z"/>
      </g>
      <g class="st19">
        <g class="st19">
          <g class="st19">
            <g class="st19">
              <g class="st19">
                <g class="st19">
                  <g class="st19">
                    <g class="st19">
                      <text class="st16" transform="translate(112.52 509.6)"><tspan x="0" y="0">ir_plot_continuous_flow</tspan></text>
                    </g>
                  </g>
                </g>
              </g>
            </g>
          </g>
        </g>
      </g>
      <path class="st0" d="M77.96,494.9c0-.92-.74-1.66-1.66-1.66s-1.66.74-1.66,1.66v17.47c0,2.3,1.86,4.16,4.16,4.16h20.8c.92,0,1.66-.74,1.66-1.66s-.74-1.66-1.66-1.66h-20.8c-.46,0-.83-.37-.83-.83v-17.47h-.01,0ZM99.1,499.4c.65-.65.65-1.71,0-2.36s-1.71-.65-2.36,0l-5.47,5.48-2.98-2.98c-.65-.65-1.71-.65-2.36,0l-5.82,5.82c-.65.65-.65,1.71,0,2.36s1.71.65,2.36,0l4.65-4.64,2.98,2.98c.65.65,1.71.65,2.36,0l6.65-6.65h-.01Z"/>
    </g>
  </a>
  <a xlink:href="https://isoreader2.isoverse.org/reference/ir_find_isofiles.html">
    <g>
      <g>
        <rect class="st23" x="530.54" y="95.23" width="224.14" height="41.92" rx="4.23" ry="4.23"/>
        <rect class="st2" x="530.54" y="95.23" width="224.14" height="41.92" rx="4.23" ry="4.23"/>
      </g>
      <path class="st0" d="M544.86,117.19l1.12-10.98c.08-.77.61-1.35,1.25-1.35h6.61c.59,0,1.06.58,1.06,1.3,0,.15-.02.31-.07.46l-1.81,5.94h5.46c.79,0,1.44.79,1.44,1.77,0,.36-.09.7-.25,1l-7.53,13.53c-.23.41-.61.66-1.01.66h-.11c-.62,0-1.12-.62-1.12-1.37,0-.11,0-.22.04-.33l1.84-9.08h-5.64c-.69,0-1.25-.69-1.25-1.54h-.03,0Z"/>
      <g class="st19">
        <g class="st19">
          <g class="st19">
            <g class="st19">
              <g class="st19">
                <g class="st19">
                  <g class="st19">
                    <g class="st19">
                      <text class="st16" transform="translate(566.66 121.72)"><tspan x="0" y="0">ir_find_continuous_flow</tspan></text>
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
        <rect class="st23" x="40.04" y="49.11" width="195.14" height="41.92" rx="4.23" ry="4.23"/>
        <rect class="st2" x="40.04" y="49.11" width="195.14" height="41.92" rx="4.23" ry="4.23"/>
      </g>
      <path class="st0" d="M54.36,71.07l1.12-10.98c.08-.77.61-1.35,1.25-1.35h6.61c.59,0,1.06.58,1.06,1.3,0,.15-.02.31-.07.46l-1.81,5.94h5.46c.79,0,1.44.79,1.44,1.77,0,.36-.09.7-.25,1l-7.53,13.53c-.23.41-.61.66-1.01.66h-.11c-.62,0-1.12-.62-1.12-1.37,0-.11,0-.22.04-.33l1.84-9.08h-5.64c-.69,0-1.25-.69-1.25-1.54h-.03,0Z"/>
      <g class="st19">
        <g class="st19">
          <g class="st19">
            <g class="st19">
              <g class="st19">
                <g class="st19">
                  <g class="st19">
                    <g class="st19">
                      <text class="st16" transform="translate(76.15 75.6)"><tspan x="0" y="0">ir_examples_folder</tspan></text>
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
        <rect class="st23" x="530.73" y="145.61" width="178.14" height="41.92" rx="4.23" ry="4.23"/>
        <rect class="st2" x="530.73" y="145.61" width="178.14" height="41.92" rx="4.23" ry="4.23"/>
      </g>
      <path class="st0" d="M545.05,167.57l1.12-10.98c.08-.77.61-1.35,1.25-1.35h6.61c.59,0,1.06.58,1.06,1.3,0,.15-.02.31-.07.46l-1.81,5.94h5.46c.79,0,1.44.79,1.44,1.77,0,.36-.09.7-.25,1l-7.53,13.53c-.23.41-.61.66-1.01.66h-.11c-.62,0-1.12-.62-1.12-1.37,0-.11,0-.22.04-.33l1.84-9.08h-5.64c-.69,0-1.25-.69-1.25-1.54h-.03,0Z"/>
      <g class="st19">
        <g class="st19">
          <g class="st19">
            <g class="st19">
              <g class="st19">
                <g class="st19">
                  <g class="st19">
                    <g class="st19">
                      <text class="st16" transform="translate(566.72 172.1)"><tspan x="0" y="0">ir_find_dual_inlet</tspan></text>
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
        <rect class="st23" x="530.11" y="196.69" width="156.14" height="41.92" rx="4.23" ry="4.23"/>
        <rect class="st2" x="530.11" y="196.69" width="156.14" height="41.92" rx="4.23" ry="4.23"/>
      </g>
      <path class="st0" d="M544.43,218.65l1.12-10.98c.08-.77.61-1.35,1.25-1.35h6.61c.59,0,1.06.58,1.06,1.3,0,.15-.02.31-.07.46l-1.81,5.94h5.46c.79,0,1.44.79,1.44,1.77,0,.36-.09.7-.25,1l-7.53,13.53c-.23.41-.61.66-1.01.66h-.11c-.62,0-1.12-.62-1.12-1.37,0-.11,0-.22.04-.33l1.84-9.08h-5.64c-.69,0-1.25-.69-1.25-1.54h-.03,0Z"/>
      <g class="st19">
        <g class="st19">
          <g class="st19">
            <g class="st19">
              <g class="st19">
                <g class="st19">
                  <g class="st19">
                    <g class="st19">
                      <text class="st16" transform="translate(569.97 223.18)"><tspan x="0" y="0">ir_find_scans</tspan></text>
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
        <rect class="st23" x="116.48" y="689.42" width="187.52" height="41.92" rx="5.08" ry="5.08"/>
        <rect class="st12" x="116.48" y="689.42" width="187.52" height="41.92" rx="5.08" ry="5.08"/>
      </g>
      <g class="st19">
        <g class="st19">
          <g class="st19">
            <g class="st19">
              <g class="st19">
                <g class="st19">
                  <g class="st19">
                    <g class="st19">
                      <text class="st16" transform="translate(161.33 714.91)"><tspan x="0" y="0">ir_get_data</tspan></text>
                    </g>
                  </g>
                </g>
              </g>
            </g>
          </g>
        </g>
      </g>
      <path class="st0" d="M137.57,724.92c8.03,0,14.54-6.51,14.54-14.54s-6.51-14.54-14.54-14.54-14.54,6.51-14.54,14.54,6.51,14.54,14.54,14.54ZM135.3,714.92h1.36v-3.63h-1.36c-.76,0-1.36-.61-1.36-1.36s.61-1.36,1.36-1.36h2.73c.76,0,1.36.61,1.36,1.36v5h.45c.76,0,1.36.61,1.36,1.36s-.61,1.36-1.36,1.36h-4.54c-.76,0-1.36-.61-1.36-1.36s.61-1.36,1.36-1.36h0ZM137.57,703.11c1,0,1.82.81,1.82,1.82s-.81,1.82-1.82,1.82-1.82-.81-1.82-1.82.81-1.82,1.82-1.82Z"/>
    </g>
  </a>
  <a xlink:href="https://isoreader2.isoverse.org/reference/ir_get_data.html">
    <g>
      <g>
        <rect class="st23" x="116.48" y="739.27" width="206.52" height="41.92" rx="5.08" ry="5.08"/>
        <rect class="st12" x="116.48" y="739.27" width="206.52" height="41.92" rx="5.08" ry="5.08"/>
      </g>
      <g class="st19">
        <g class="st19">
          <g class="st19">
            <g class="st19">
              <g class="st19">
                <g class="st19">
                  <g class="st19">
                    <g class="st19">
                      <text class="st16" transform="translate(161.33 764.76)"><tspan x="0" y="0">ir_get_metadata</tspan></text>
                    </g>
                  </g>
                </g>
              </g>
            </g>
          </g>
        </g>
      </g>
      <path class="st0" d="M137.57,774.77c8.03,0,14.54-6.51,14.54-14.54s-6.51-14.54-14.54-14.54-14.54,6.51-14.54,14.54,6.51,14.54,14.54,14.54ZM135.3,764.77h1.36v-3.63h-1.36c-.76,0-1.36-.61-1.36-1.36s.61-1.36,1.36-1.36h2.73c.76,0,1.36.61,1.36,1.36v5h.45c.76,0,1.36.61,1.36,1.36s-.61,1.36-1.36,1.36h-4.54c-.76,0-1.36-.61-1.36-1.36s.61-1.36,1.36-1.36h0ZM137.57,752.96c1,0,1.82.81,1.82,1.82s-.81,1.82-1.82,1.82-1.82-.81-1.82-1.82.81-1.82,1.82-1.82Z"/>
    </g>
  </a>
  <a xlink:href="https://isoreader2.isoverse.org/reference/ir_get_data.html">
    <g>
      <g>
        <rect class="st23" x="116.48" y="789.12" width="252.52" height="41.92" rx="5.08" ry="5.08"/>
        <rect class="st12" x="116.48" y="789.12" width="252.52" height="41.92" rx="5.08" ry="5.08"/>
      </g>
      <g class="st19">
        <g class="st19">
          <g class="st19">
            <g class="st19">
              <g class="st19">
                <g class="st19">
                  <g class="st19">
                    <g class="st19">
                      <text class="st16" transform="translate(161.33 814.61)"><tspan x="0" y="0">ir_get_traces/cycles/scans</tspan></text>
                    </g>
                  </g>
                </g>
              </g>
            </g>
          </g>
        </g>
      </g>
      <path class="st0" d="M137.57,824.62c8.03,0,14.54-6.51,14.54-14.54s-6.51-14.54-14.54-14.54-14.54,6.51-14.54,14.54,6.51,14.54,14.54,14.54ZM135.3,814.62h1.36v-3.63h-1.36c-.76,0-1.36-.61-1.36-1.36s.61-1.36,1.36-1.36h2.73c.76,0,1.36.61,1.36,1.36v5h.45c.76,0,1.36.61,1.36,1.36s-.61,1.36-1.36,1.36h-4.54c-.76,0-1.36-.61-1.36-1.36s.61-1.36,1.36-1.36h0ZM137.57,802.81c1,0,1.82.81,1.82,1.82s-.81,1.82-1.82,1.82-1.82-.81-1.82-1.82.81-1.82,1.82-1.82Z"/>
    </g>
  </a>
  <a xlink:href="https://isoreader2.isoverse.org/reference/ir_get_data.html">
    <g>
      <g>
        <rect class="st23" x="116.48" y="837.16" width="171.52" height="41.92" rx="5.08" ry="5.08"/>
        <rect class="st12" x="116.48" y="837.16" width="171.52" height="41.92" rx="5.08" ry="5.08"/>
      </g>
      <g class="st19">
        <g class="st19">
          <g class="st19">
            <g class="st19">
              <g class="st19">
                <g class="st19">
                  <g class="st19">
                    <g class="st19">
                      <text class="st16" transform="translate(161.33 862.65)"><tspan x="0" y="0">ir_get_resistors</tspan></text>
                    </g>
                  </g>
                </g>
              </g>
            </g>
          </g>
        </g>
      </g>
      <path class="st0" d="M137.57,872.66c8.03,0,14.54-6.51,14.54-14.54s-6.51-14.54-14.54-14.54-14.54,6.51-14.54,14.54,6.51,14.54,14.54,14.54ZM135.3,862.66h1.36v-3.63h-1.36c-.76,0-1.36-.61-1.36-1.36s.61-1.36,1.36-1.36h2.73c.76,0,1.36.61,1.36,1.36v5h.45c.76,0,1.36.61,1.36,1.36s-.61,1.36-1.36,1.36h-4.54c-.76,0-1.36-.61-1.36-1.36s.61-1.36,1.36-1.36h0ZM137.57,850.85c1,0,1.82.81,1.82,1.82s-.81,1.82-1.82,1.82-1.82-.81-1.82-1.82.81-1.82,1.82-1.82Z"/>
    </g>
  </a>
  <g>
    <g>
      <rect class="st24" x="301.25" y="9.25" width="224.34" height="53.98" rx="4.45" ry="4.45"/>
      <rect class="st8" x="301.25" y="9.25" width="224.34" height="53.98" rx="4.45" ry="4.45"/>
    </g>
    <g class="st19">
      <g class="st19">
        <g class="st19">
          <g class="st19">
            <g class="st19">
              <g class="st19">
                <g class="st19">
                  <g class="st19">
                    <text class="st16" transform="translate(314.93 57.15)"><tspan x="0" y="0">isotope data files/archives</tspan></text>
                  </g>
                </g>
              </g>
            </g>
          </g>
        </g>
      </g>
    </g>
    <path class="st0" d="M324.68,39.84c.47,0,.86-.39.86-.86v-15.44h-4.29c-.95,0-1.72-.77-1.72-1.72v-4.29h-8.58c-.47,0-.86.39-.86.86v20.58c0,.47.39.86.86.86h13.73ZM307.53,18.4c0-1.89,1.54-3.43,3.43-3.43h8.87c.91,0,1.78.36,2.43,1l4.85,4.85c.64.64,1,1.52,1,2.43v15.73c0,1.89-1.54,3.43-3.43,3.43h-13.72c-1.89,0-3.43-1.54-3.43-3.43v-20.58h0Z"/>
    <g class="st19">
      <g class="st19">
        <g class="st19">
          <g class="st19">
            <g class="st19">
              <g class="st19">
                <g class="st19">
                  <text class="st20" transform="translate(317.82 38.54) rotate(-90)"><tspan x="0" y="0">dxf</tspan></text>
                </g>
              </g>
            </g>
          </g>
        </g>
      </g>
    </g>
    <path class="st0" d="M348.42,39.83c.47,0,.86-.39.86-.86v-15.44h-4.29c-.95,0-1.72-.77-1.72-1.72v-4.29h-8.58c-.47,0-.86.39-.86.86v20.58c0,.47.39.86.86.86h13.73ZM331.27,18.39c0-1.89,1.54-3.43,3.43-3.43h8.87c.91,0,1.78.36,2.43,1l4.85,4.85c.64.64,1,1.52,1,2.43v15.73c0,1.89-1.54,3.43-3.43,3.43h-13.72c-1.89,0-3.43-1.54-3.43-3.43v-20.58h0Z"/>
    <g class="st19">
      <g class="st19">
        <g class="st19">
          <g class="st19">
            <g class="st19">
              <g class="st19">
                <g class="st19">
                  <text class="st20" transform="translate(341.56 38.53) rotate(-90)"><tspan x="0" y="0">cf</tspan></text>
                </g>
              </g>
            </g>
          </g>
        </g>
      </g>
    </g>
    <path class="st0" d="M372.32,39.83c.47,0,.86-.39.86-.86v-15.44h-4.29c-.95,0-1.72-.77-1.72-1.72v-4.29h-8.58c-.47,0-.86.39-.86.86v20.58c0,.47.39.86.86.86h13.73ZM355.17,18.39c0-1.89,1.54-3.43,3.43-3.43h8.87c.91,0,1.78.36,2.43,1l4.85,4.85c.64.64,1,1.52,1,2.43v15.73c0,1.89-1.54,3.43-3.43,3.43h-13.72c-1.89,0-3.43-1.54-3.43-3.43v-20.58h0Z"/>
    <g class="st19">
      <g class="st19">
        <g class="st19">
          <g class="st19">
            <g class="st19">
              <g class="st19">
                <g class="st19">
                  <text class="st20" transform="translate(365.46 38.53) rotate(-90)"><tspan x="0" y="0">bch</tspan></text>
                </g>
              </g>
            </g>
          </g>
        </g>
      </g>
    </g>
    <path class="st0" d="M396.54,39.84c.47,0,.86-.39.86-.86v-15.44h-4.29c-.95,0-1.72-.77-1.72-1.72v-4.29h-8.58c-.47,0-.86.39-.86.86v20.58c0,.47.39.86.86.86h13.73ZM379.39,18.4c0-1.89,1.54-3.43,3.43-3.43h8.87c.91,0,1.78.36,2.43,1l4.85,4.85c.64.64,1,1.52,1,2.43v15.73c0,1.89-1.54,3.43-3.43,3.43h-13.72c-1.89,0-3.43-1.54-3.43-3.43v-20.58h0Z"/>
    <g class="st19">
      <g class="st19">
        <g class="st19">
          <g class="st19">
            <g class="st19">
              <g class="st19">
                <g class="st19">
                  <text class="st20" transform="translate(389.68 38.54) rotate(-90)"><tspan x="0" y="0">iarc</tspan></text>
                </g>
              </g>
            </g>
          </g>
        </g>
      </g>
    </g>
    <path class="st0" d="M420.62,39.84c.47,0,.86-.39.86-.86v-15.44h-4.29c-.95,0-1.72-.77-1.72-1.72v-4.29h-8.58c-.47,0-.86.39-.86.86v20.58c0,.47.39.86.86.86h13.73ZM403.47,18.4c0-1.89,1.54-3.43,3.43-3.43h8.87c.91,0,1.78.36,2.43,1l4.85,4.85c.64.64,1,1.52,1,2.43v15.73c0,1.89-1.54,3.43-3.43,3.43h-13.72c-1.89,0-3.43-1.54-3.43-3.43v-20.58h0Z"/>
    <g class="st19">
      <g class="st19">
        <g class="st19">
          <g class="st19">
            <g class="st19">
              <g class="st19">
                <g class="st19">
                  <text class="st20" transform="translate(413.76 38.54) rotate(-90)"><tspan x="0" y="0">larc</tspan></text>
                </g>
              </g>
            </g>
          </g>
        </g>
      </g>
    </g>
    <path class="st0" d="M444.36,39.83c.47,0,.86-.39.86-.86v-15.44h-4.29c-.95,0-1.72-.77-1.72-1.72v-4.29h-8.58c-.47,0-.86.39-.86.86v20.58c0,.47.39.86.86.86h13.73ZM427.21,18.39c0-1.89,1.54-3.43,3.43-3.43h8.87c.91,0,1.78.36,2.43,1l4.85,4.85c.64.64,1,1.52,1,2.43v15.73c0,1.89-1.54,3.43-3.43,3.43h-13.72c-1.89,0-3.43-1.54-3.43-3.43v-20.58h0Z"/>
    <g class="st19">
      <g class="st19">
        <g class="st19">
          <g class="st19">
            <g class="st19">
              <g class="st19">
                <g class="st19">
                  <text class="st21" transform="translate(435.5 38.53) rotate(-90)"><tspan x="0" y="0">imexp</tspan></text>
                </g>
              </g>
            </g>
          </g>
        </g>
      </g>
    </g>
    <path class="st0" d="M468.26,39.83c.47,0,.86-.39.86-.86v-15.44h-4.29c-.95,0-1.72-.77-1.72-1.72v-4.29h-8.58c-.47,0-.86.39-.86.86v20.58c0,.47.39.86.86.86h13.73ZM451.11,18.39c0-1.89,1.54-3.43,3.43-3.43h8.87c.91,0,1.78.36,2.43,1l4.85,4.85c.64.64,1,1.52,1,2.43v15.73c0,1.89-1.54,3.43-3.43,3.43h-13.72c-1.89,0-3.43-1.54-3.43-3.43v-20.58h0Z"/>
    <g class="st19">
      <g class="st19">
        <g class="st19">
          <g class="st19">
            <g class="st19">
              <g class="st19">
                <g class="st19">
                  <text class="st20" transform="translate(461.4 38.53) rotate(-90)"><tspan x="0" y="0">did</tspan></text>
                </g>
              </g>
            </g>
          </g>
        </g>
      </g>
    </g>
    <path class="st0" d="M492.47,39.84c.47,0,.86-.39.86-.86v-15.44h-4.29c-.95,0-1.72-.77-1.72-1.72v-4.29h-8.58c-.47,0-.86.39-.86.86v20.58c0,.47.39.86.86.86h13.73ZM475.32,18.4c0-1.89,1.54-3.43,3.43-3.43h8.87c.91,0,1.78.36,2.43,1l4.85,4.85c.64.64,1,1.52,1,2.43v15.73c0,1.89-1.54,3.43-3.43,3.43h-13.72c-1.89,0-3.43-1.54-3.43-3.43v-20.58h0Z"/>
    <g class="st19">
      <g class="st19">
        <g class="st19">
          <g class="st19">
            <g class="st19">
              <g class="st19">
                <g class="st19">
                  <text class="st20" transform="translate(485.61 38.54) rotate(-90)"><tspan x="0" y="0">caf</tspan></text>
                </g>
              </g>
            </g>
          </g>
        </g>
      </g>
    </g>
    <path class="st0" d="M516.12,39.84c.47,0,.86-.39.86-.86v-15.44h-4.29c-.95,0-1.72-.77-1.72-1.72v-4.29h-8.58c-.47,0-.86.39-.86.86v20.58c0,.47.39.86.86.86h13.73ZM498.98,18.4c0-1.89,1.54-3.43,3.43-3.43h8.87c.91,0,1.78.36,2.43,1l4.85,4.85c.64.64,1,1.52,1,2.43v15.73c0,1.89-1.54,3.43-3.43,3.43h-13.72c-1.89,0-3.43-1.54-3.43-3.43v-20.58h0Z"/>
    <g class="st19">
      <g class="st19">
        <g class="st19">
          <g class="st19">
            <g class="st19">
              <g class="st19">
                <g class="st19">
                  <text class="st20" transform="translate(509.27 38.54) rotate(-90)"><tspan x="0" y="0">scn</tspan></text>
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
      <rect class="st24" x="547.17" y="544.15" width="209.29" height="41.92" rx="4.73" ry="4.73"/>
      <rect class="st7" x="547.17" y="544.15" width="209.29" height="41.92" rx="4.73" ry="4.73"/>
    </g>
    <g class="st19">
      <g class="st19">
        <g class="st19">
          <g class="st19">
            <g class="st19">
              <g class="st19">
                <g class="st19">
                  <text class="st16" transform="translate(587.6 569.68)"><tspan x="0" y="0">storage file (parquet)</tspan></text>
                </g>
              </g>
            </g>
          </g>
        </g>
      </g>
    </g>
    <g>
      <g>
        <line class="st11" x1="561.77" y1="563.69" x2="576.61" y2="563.69"/>
        <line class="st11" x1="561.76" y1="566.69" x2="576.61" y2="566.69"/>
        <line class="st11" x1="561.79" y1="569.68" x2="576.62" y2="569.68"/>
        <line class="st11" x1="561.78" y1="572.68" x2="576.61" y2="572.68"/>
        <line class="st11" x1="565.17" y1="563.69" x2="565.17" y2="576.37"/>
        <line class="st11" x1="569.17" y1="563.69" x2="569.17" y2="576.37"/>
        <line class="st11" x1="573.4" y1="563.69" x2="573.4" y2="576.37"/>
      </g>
      <path class="st0" d="M561.82,574.8v-19.72c0-.46.37-.82.82-.82h8.22v4.1c0,.91.74,1.65,1.65,1.65h4.1v14.79c0,.46-.37.82-.82.82h-13.15c-.46,0-.82-.37-.82-.82h0ZM562.64,551.8c-1.81,0-3.28,1.47-3.28,3.28v19.72c0,1.81,1.47,3.28,3.28,3.28h13.15c1.81,0,3.28-1.47,3.28-3.28v-15.07c0-.88-.34-1.71-.96-2.33l-4.65-4.65c-.61-.61-1.44-.96-2.32-.96h-8.5Z"/>
    </g>
  </g>
  <g>
    <line class="st9" x1="575.16" y1="444.92" x2="531.95" y2="444.92"/>
    <polygon class="st13" points="535.46 432.95 514.73 444.92 535.46 456.89 535.46 432.95"/>
  </g>
  <g>
    <g>
      <rect class="st24" x="9.79" y="901.82" width="248.29" height="41.92" rx="4.73" ry="4.73"/>
      <rect class="st7" x="9.79" y="901.82" width="248.29" height="41.92" rx="4.73" ry="4.73"/>
    </g>
    <g class="st19">
      <g class="st19">
        <g class="st19">
          <g class="st19">
            <g class="st19">
              <g class="st19">
                <g class="st19">
                  <text class="st16" transform="translate(50.21 927.35)"><tspan x="0" y="0">output spreadsheet (xlsx)</tspan></text>
                </g>
              </g>
            </g>
          </g>
        </g>
      </g>
    </g>
    <g>
      <g>
        <line class="st11" x1="24.38" y1="921.36" x2="39.23" y2="921.36"/>
        <line class="st11" x1="24.37" y1="924.36" x2="39.22" y2="924.36"/>
        <line class="st11" x1="24.4" y1="927.35" x2="39.24" y2="927.35"/>
        <line class="st11" x1="24.39" y1="930.35" x2="39.23" y2="930.35"/>
        <line class="st11" x1="27.78" y1="921.36" x2="27.78" y2="934.04"/>
        <line class="st11" x1="31.79" y1="921.36" x2="31.79" y2="934.04"/>
        <line class="st11" x1="36.01" y1="921.36" x2="36.01" y2="934.04"/>
      </g>
      <path class="st0" d="M24.43,932.47v-19.72c0-.46.37-.82.82-.82h8.22v4.1c0,.91.74,1.65,1.65,1.65h4.1v14.79c0,.46-.37.82-.82.82h-13.15c-.46,0-.82-.37-.82-.82h0ZM25.25,909.47c-1.81,0-3.28,1.47-3.28,3.28v19.72c0,1.81,1.47,3.28,3.28,3.28h13.15c1.81,0,3.28-1.47,3.28-3.28v-15.07c0-.88-.34-1.71-.96-2.33l-4.65-4.65c-.61-.61-1.44-.96-2.32-.96h-8.5Z"/>
      <g class="st19">
        <g class="st19">
          <g class="st19">
            <g class="st19">
              <g class="st19">
                <g class="st19">
                  <text class="st18" transform="translate(31.23 932.05) rotate(-90)"><tspan x="0" y="0">xlsx</tspan></text>
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
                  <text class="st15" transform="translate(266.85 967.52)"><tspan x="0" y="0">to downstream processing</tspan></text>
                </g>
              </g>
            </g>
          </g>
        </g>
      </g>
    </g>
  </g>
  <g>
    <polyline class="st1" points="668.85 513.02 721.08 513.02 721.08 527.44"/>
    <polygon class="st13" points="711.11 524.53 721.08 541.8 731.04 524.53 711.11 524.53"/>
  </g>
  <g>
    <g>
      <rect class="st23" x="427.99" y="252.46" width="174.26" height="41.92" rx="4.23" ry="4.23"/>
      <rect class="st6" x="427.99" y="252.46" width="174.26" height="41.92" rx="4.23" ry="4.23"/>
    </g>
    <path class="st0" d="M442.31,273.42l1.12-10.98c.08-.77.61-1.35,1.25-1.35h6.61c.59,0,1.06.58,1.06,1.3,0,.15-.02.31-.07.46l-1.81,5.94h5.46c.79,0,1.44.79,1.44,1.77,0,.36-.09.7-.25,1l-7.53,13.53c-.23.41-.61.66-1.01.66h-.11c-.62,0-1.12-.62-1.12-1.37,0-.11,0-.22.04-.33l1.84-9.08h-5.64c-.69,0-1.25-.69-1.25-1.54h-.03,0Z"/>
    <g class="st19">
      <g class="st19">
        <g class="st19">
          <g class="st19">
            <g class="st19">
              <g class="st19">
                <g class="st19">
                  <g class="st19">
                    <text class="st16" transform="translate(466.85 277.95)"><tspan x="0" y="0">ir_save_isofiles</tspan></text>
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
      <rect class="st23" x="427.99" y="354.01" width="176.26" height="41.92" rx="4.23" ry="4.23"/>
      <rect class="st6" x="427.99" y="354.01" width="176.26" height="41.92" rx="4.23" ry="4.23"/>
    </g>
    <path class="st0" d="M442.31,374.97l1.12-10.98c.08-.77.61-1.35,1.25-1.35h6.61c.59,0,1.06.58,1.06,1.3,0,.15-.02.31-.07.46l-1.81,5.94h5.46c.79,0,1.44.79,1.44,1.77,0,.36-.09.7-.25,1l-7.53,13.53c-.23.41-.61.66-1.01.66h-.11c-.62,0-1.12-.62-1.12-1.37,0-.11,0-.22.04-.33l1.84-9.08h-5.64c-.69,0-1.25-.69-1.25-1.54h-.03,0Z"/>
    <g class="st19">
      <g class="st19">
        <g class="st19">
          <g class="st19">
            <g class="st19">
              <g class="st19">
                <g class="st19">
                  <g class="st19">
                    <text class="st16" transform="translate(466.85 379.5)"><tspan x="0" y="0">ir_load_isofiles</tspan></text>
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
    <line class="st1" x1="387.97" y1="273.11" x2="409.88" y2="273.11"/>
    <polygon class="st13" points="406.97 283.08 424.24 273.11 406.97 263.14 406.97 283.08"/>
  </g>
  <g>
    <line class="st1" x1="427.24" y1="375.87" x2="405.33" y2="375.87"/>
    <polygon class="st13" points="408.24 365.9 390.97 375.87 408.24 385.84 408.24 365.9"/>
  </g>
  <g>
    <polyline class="st1" points="234.47 72.14 325.71 72.14 325.71 84.56"/>
    <polygon class="st13" points="315.74 81.65 325.71 98.92 335.68 81.65 315.74 81.65"/>
  </g>
  <g>
    <g>
      <rect class="st24" x="547.17" y="303.73" width="182.29" height="41.92" rx="4.73" ry="4.73"/>
      <rect class="st7" x="547.17" y="303.73" width="182.29" height="41.92" rx="4.73" ry="4.73"/>
    </g>
    <g class="st19">
      <g class="st19">
        <g class="st19">
          <g class="st19">
            <g class="st19">
              <g class="st19">
                <g class="st19">
                  <text class="st16" transform="translate(587.6 329.26)"><tspan x="0" y="0">storage file (rds)</tspan></text>
                </g>
              </g>
            </g>
          </g>
        </g>
      </g>
    </g>
    <g>
      <g>
        <line class="st11" x1="561.77" y1="323.27" x2="576.61" y2="323.27"/>
        <line class="st11" x1="561.76" y1="326.27" x2="576.61" y2="326.27"/>
        <line class="st11" x1="561.79" y1="329.26" x2="576.62" y2="329.26"/>
        <line class="st11" x1="561.78" y1="332.26" x2="576.61" y2="332.26"/>
        <line class="st11" x1="565.17" y1="323.27" x2="565.17" y2="335.95"/>
        <line class="st11" x1="569.17" y1="323.27" x2="569.17" y2="335.95"/>
        <line class="st11" x1="573.4" y1="323.27" x2="573.4" y2="335.95"/>
      </g>
      <path class="st0" d="M561.82,334.38v-19.72c0-.46.37-.82.82-.82h8.22v4.1c0,.91.74,1.65,1.65,1.65h4.1v14.79c0,.46-.37.82-.82.82h-13.15c-.46,0-.82-.37-.82-.82h0ZM562.64,311.38c-1.81,0-3.28,1.47-3.28,3.28v19.72c0,1.81,1.47,3.28,3.28,3.28h13.15c1.81,0,3.28-1.47,3.28-3.28v-15.07c0-.88-.34-1.71-.96-2.33l-4.65-4.65c-.61-.61-1.44-.96-2.32-.96h-8.5Z"/>
    </g>
  </g>
  <g>
    <polyline class="st1" points="602.85 271.6 659.08 271.6 659.08 286.02"/>
    <polygon class="st13" points="649.11 283.11 659.08 300.38 669.04 283.11 649.11 283.11"/>
  </g>
</svg>

```




## Getting help

If you encounter a bug, please file an issue with a minimal reproducible example on [GitHub](https://github.com/isoverse/isoreader2/issues). Example files are very helpful for fixing bugs so please consider including an example data file (you will have to attach it as a zip archive).

## isoverse <a href='http://www.isoverse.org'><img src='man/figures/isoverse_logo_thumb.png' align="right" width="100" alt="isoverse logo"/></a>

This package is part of the isoverse suite of data tools for stable isotopes. If you like the functionality that isoverse packages provide, please help us spread the word and include an isoverse or individual package logo on one of your posters or slides. All logos are posted in high resolution in [this repository](https://github.com/isoverse/logos). If you have suggestions for new features or other constructive feedback, please let us know on this short [feeback form](https://www.isoverse.org/feedback/).

## Funding <a href='https://www.nsf.gov/'><img src='man/figures/NSF_logo.svg' align="right" width="100" alt="NSF logo"/></a>

This project is supported by a grant from the US National Science Foundation ([EAR-2411458](https://www.nsf.gov/awardsearch/show-award?AWD_ID=2411458)) to Sebastian Kopf. 

