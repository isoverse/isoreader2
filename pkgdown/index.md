---
output: github_document
params:
  generating_pkgdown_index: false
---

<!-- README.md is generated from README.Rmd. Please edit that file -->



# isoreader2 <a href='https://isoreader2.isoverse.org/'> <img src="man/figures/isoreader_logo_thumb.png" align="right" width="100"/> </a>

<!-- badges: start -->
  [![Documentation](https://img.shields.io/badge/docs-online-green.svg)](https://isoreader2.isoverse.org/)
  [![R-CMD-check](https://github.com/isoverse/isoreader2/workflows/R-CMD-check/badge.svg)](https://github.com/isoverse/isoreader2/actions)
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

## Package structure

<p>Click on the individual functions to jump straight to their documenation.</p>

```{=html}
<?xml version="1.0" encoding="UTF-8"?>
<svg id="Layer_1" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" version="1.1" viewBox="0 0 786.53 696.49">
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
  <rect class="st22" width="786.53" height="696.48"/>
  <g>
    <g>
      <rect class="st23" x="458.51" y="521.39" width="200.29" height="41.92" rx="4.23" ry="4.23"/>
      <rect class="st6" x="458.51" y="521.39" width="200.29" height="41.92" rx="4.23" ry="4.23"/>
    </g>
    <path class="st0" d="M472.83,542.35l1.12-10.98c.08-.77.61-1.35,1.25-1.35h6.61c.59,0,1.06.58,1.06,1.3,0,.15-.02.31-.07.46l-1.81,5.94h5.46c.79,0,1.44.79,1.44,1.77,0,.36-.09.7-.25,1l-7.53,13.53c-.23.41-.61.66-1.01.66h-.11c-.62,0-1.12-.62-1.12-1.37,0-.11,0-.22.04-.33l1.84-9.08h-5.64c-.69,0-1.25-.69-1.25-1.54h-.03,0Z"/>
    <g class="st19">
      <g class="st19">
        <g class="st19">
          <g class="st19">
            <g class="st19">
              <g class="st19">
                <text class="st16" transform="translate(497.37 546.88)"><tspan x="0" y="0">ir_filter</tspan></text>
              </g>
            </g>
          </g>
        </g>
      </g>
    </g>
  </g>
  <g>
    <g>
      <rect class="st23" x="458.51" y="569.51" width="200.29" height="41.92" rx="4.23" ry="4.23"/>
      <rect class="st6" x="458.51" y="569.51" width="200.29" height="41.92" rx="4.23" ry="4.23"/>
    </g>
    <path class="st0" d="M472.83,590.47l1.12-10.98c.08-.77.61-1.35,1.25-1.35h6.61c.59,0,1.06.58,1.06,1.3,0,.15-.02.31-.07.46l-1.81,5.94h5.46c.79,0,1.44.79,1.44,1.77,0,.36-.09.7-.25,1l-7.53,13.53c-.23.41-.61.66-1.01.66h-.11c-.62,0-1.12-.62-1.12-1.37,0-.11,0-.22.04-.33l1.84-9.08h-5.64c-.69,0-1.25-.69-1.25-1.54h-.03,0Z"/>
    <g class="st19">
      <g class="st19">
        <g class="st19">
          <g class="st19">
            <g class="st19">
              <g class="st19">
                <text class="st16" transform="translate(497.37 595)"><tspan x="0" y="0">ir_join_metadata</tspan></text>
              </g>
            </g>
          </g>
        </g>
      </g>
    </g>
  </g>
  <g>
    <g>
      <rect class="st23" x="8.56" y="485.71" width="234.17" height="41.92" rx="4.23" ry="4.23"/>
      <rect class="st6" x="8.56" y="485.71" width="234.17" height="41.92" rx="4.23" ry="4.23"/>
    </g>
    <path class="st0" d="M22.88,506.67l1.12-10.98c.08-.77.61-1.35,1.25-1.35h6.61c.59,0,1.06.58,1.06,1.3,0,.15-.02.31-.07.46l-1.81,5.94h5.46c.79,0,1.44.79,1.44,1.77,0,.36-.09.7-.25,1l-7.53,13.53c-.23.41-.61.66-1.01.66h-.11c-.62,0-1.12-.62-1.12-1.37,0-.11,0-.22.04-.33l1.84-9.08h-5.64c-.69,0-1.25-.69-1.25-1.54h-.03Z"/>
    <g class="st19">
      <g class="st19">
        <g class="st19">
          <g class="st19">
            <g class="st19">
              <g class="st19">
                <text class="st16" transform="translate(47.75 511.2)"><tspan x="0" y="0">ir_export_data_to_excel</tspan></text>
              </g>
            </g>
          </g>
        </g>
      </g>
    </g>
  </g>
  <g>
    <g>
      <rect class="st23" x="426.66" y="358.01" width="102.26" height="41.92" rx="4.23" ry="4.23"/>
      <rect class="st6" x="426.66" y="358.01" width="102.26" height="41.92" rx="4.23" ry="4.23"/>
    </g>
    <path class="st0" d="M440.98,378.97l1.12-10.98c.08-.77.61-1.35,1.25-1.35h6.61c.59,0,1.06.58,1.06,1.3,0,.15-.02.31-.07.46l-1.81,5.94h5.46c.79,0,1.44.79,1.44,1.77,0,.36-.09.7-.25,1l-7.53,13.53c-.23.41-.61.66-1.01.66h-.11c-.62,0-1.12-.62-1.12-1.37,0-.11,0-.22.04-.33l1.84-9.08h-5.64c-.69,0-1.25-.69-1.25-1.54h-.03,0Z"/>
    <g class="st19">
      <g class="st19">
        <g class="st19">
          <g class="st19">
            <g class="st19">
              <g class="st19">
                <text class="st16" transform="translate(465.52 383.5)"><tspan x="0" y="0">ir_save</tspan></text>
              </g>
            </g>
          </g>
        </g>
      </g>
    </g>
  </g>
  <g>
    <g>
      <rect class="st23" x="426.66" y="407.68" width="102.26" height="41.92" rx="4.23" ry="4.23"/>
      <rect class="st6" x="426.66" y="407.68" width="102.26" height="41.92" rx="4.23" ry="4.23"/>
    </g>
    <path class="st0" d="M440.98,428.64l1.12-10.98c.08-.77.61-1.35,1.25-1.35h6.61c.59,0,1.06.58,1.06,1.3,0,.15-.02.31-.07.46l-1.81,5.94h5.46c.79,0,1.44.79,1.44,1.77,0,.36-.09.7-.25,1l-7.53,13.53c-.23.41-.61.66-1.01.66h-.11c-.62,0-1.12-.62-1.12-1.37,0-.11,0-.22.04-.33l1.84-9.08h-5.64c-.69,0-1.25-.69-1.25-1.54h-.03,0Z"/>
    <g class="st19">
      <g class="st19">
        <g class="st19">
          <g class="st19">
            <g class="st19">
              <g class="st19">
                <text class="st16" transform="translate(465.52 433.17)"><tspan x="0" y="0">ir_load</tspan></text>
              </g>
            </g>
          </g>
        </g>
      </g>
    </g>
  </g>
  <g>
    <g>
      <rect class="st23" x="458.51" y="473.25" width="200.29" height="41.92" rx="4.23" ry="4.23"/>
      <rect class="st6" x="458.51" y="473.25" width="200.29" height="41.92" rx="4.23" ry="4.23"/>
    </g>
    <path class="st0" d="M472.83,494.21l1.12-10.98c.08-.77.61-1.35,1.25-1.35h6.61c.59,0,1.06.58,1.06,1.3,0,.15-.02.31-.07.46l-1.81,5.94h5.46c.79,0,1.44.79,1.44,1.77,0,.36-.09.7-.25,1l-7.53,13.53c-.23.41-.61.66-1.01.66h-.11c-.62,0-1.12-.62-1.12-1.37,0-.11,0-.22.04-.33l1.84-9.08h-5.64c-.69,0-1.25-.69-1.25-1.54h-.03,0Z"/>
    <g class="st19">
      <g class="st19">
        <g class="st19">
          <g class="st19">
            <g class="st19">
              <g class="st19">
                <text class="st16" transform="translate(497.37 498.74)"><tspan x="0" y="0">ir_filter_metadata</tspan></text>
              </g>
            </g>
          </g>
        </g>
      </g>
    </g>
  </g>
  <g>
    <g>
      <rect class="st23" x="458.51" y="521.37" width="200.29" height="41.92" rx="4.23" ry="4.23"/>
      <rect class="st6" x="458.51" y="521.37" width="200.29" height="41.92" rx="4.23" ry="4.23"/>
    </g>
    <path class="st0" d="M472.83,542.33l1.12-10.98c.08-.77.61-1.35,1.25-1.35h6.61c.59,0,1.06.58,1.06,1.3,0,.15-.02.31-.07.46l-1.81,5.94h5.46c.79,0,1.44.79,1.44,1.77,0,.36-.09.7-.25,1l-7.53,13.53c-.23.41-.61.66-1.01.66h-.11c-.62,0-1.12-.62-1.12-1.37,0-.11,0-.22.04-.33l1.84-9.08h-5.64c-.69,0-1.25-.69-1.25-1.54h-.03,0Z"/>
    <g class="st19">
      <g class="st19">
        <g class="st19">
          <g class="st19">
            <g class="st19">
              <g class="st19">
                <text class="st16" transform="translate(497.37 546.86)"><tspan x="0" y="0">ir_mutate_metadata</tspan></text>
              </g>
            </g>
          </g>
        </g>
      </g>
    </g>
  </g>
  <g>
    <line class="st9" x1="383.29" y1="320.59" x2="383.29" y2="642.71"/>
    <polygon class="st13" points="371.33 639.21 383.29 659.93 395.26 639.21 371.33 639.21"/>
  </g>
  <g>
    <line class="st9" x1="382.85" y1="63.25" x2="382.85" y2="83.8"/>
    <polygon class="st13" points="370.89 80.3 382.85 101.02 394.82 80.3 370.89 80.3"/>
  </g>
  <g>
    <line class="st9" x1="382.85" y1="149.28" x2="382.85" y2="169.83"/>
    <polygon class="st13" points="370.89 166.33 382.85 187.05 394.82 166.33 370.89 166.33"/>
  </g>
  <g>
    <line class="st9" x1="382.85" y1="234.08" x2="382.85" y2="254.63"/>
    <polygon class="st13" points="370.89 251.13 382.85 271.85 394.82 251.13 370.89 251.13"/>
  </g>
  <g>
    <line class="st1" x1="385.53" y1="582.1" x2="442.06" y2="582.1"/>
    <polygon class="st13" points="439.14 592.07 456.42 582.1 439.14 572.13 439.14 592.07"/>
  </g>
  <g>
    <line class="st1" x1="457.7" y1="597.55" x2="401.18" y2="597.55"/>
    <polygon class="st13" points="404.08 587.58 386.82 597.55 404.08 607.52 404.08 587.58"/>
  </g>
  <g>
    <line class="st1" x1="385.53" y1="535.12" x2="442.06" y2="535.12"/>
    <polygon class="st13" points="439.14 545.09 456.42 535.12 439.14 525.15 439.14 545.09"/>
  </g>
  <g>
    <line class="st1" x1="457.7" y1="550.57" x2="401.18" y2="550.57"/>
    <polygon class="st13" points="404.08 540.6 386.82 550.57 404.08 560.54 404.08 540.6"/>
  </g>
  <g>
    <line class="st1" x1="385.53" y1="489.39" x2="442.06" y2="489.39"/>
    <polygon class="st13" points="439.14 499.36 456.42 489.39 439.14 479.42 439.14 499.36"/>
  </g>
  <g>
    <line class="st1" x1="457.7" y1="504.84" x2="401.18" y2="504.84"/>
    <polygon class="st13" points="404.08 494.87 386.82 504.84 404.08 514.81 404.08 494.87"/>
  </g>
  <g>
    <line class="st1" x1="528.71" y1="216.78" x2="492.19" y2="216.78"/>
    <polygon class="st13" points="495.09 206.81 477.83 216.78 495.09 226.75 495.09 206.81"/>
  </g>
  <g>
    <line class="st1" x1="386.64" y1="378.66" x2="408.55" y2="378.66"/>
    <polygon class="st13" points="405.64 388.63 422.91 378.66 405.64 368.69 405.64 388.63"/>
  </g>
  <g>
    <line class="st1" x1="425.91" y1="429.55" x2="404" y2="429.55"/>
    <polygon class="st13" points="406.91 419.58 389.64 429.55 406.91 439.52 406.91 419.58"/>
  </g>
  <g>
    <line class="st1" x1="529.67" y1="378.66" x2="552.58" y2="378.66"/>
    <polygon class="st13" points="549.67 388.63 566.94 378.66 549.67 368.69 549.67 388.63"/>
  </g>
  <g>
    <line class="st1" x1="97.94" y1="528.4" x2="97.94" y2="579.31"/>
    <polygon class="st13" points="87.97 576.4 97.94 593.67 107.91 576.4 87.97 576.4"/>
  </g>
  <g>
    <line class="st1" x1="383.44" y1="359.67" x2="322.53" y2="359.67"/>
    <polygon class="st13" points="325.44 349.7 308.17 359.67 325.44 369.64 325.44 349.7"/>
  </g>
  <g>
    <line class="st1" x1="383.44" y1="565.72" x2="322.53" y2="565.72"/>
    <polygon class="st13" points="325.44 555.75 308.17 565.72 325.44 575.69 325.44 555.75"/>
  </g>
  <g>
    <polyline class="st1" points="525.69 35.78 581.92 35.78 581.92 69.2"/>
    <polygon class="st13" points="571.95 66.29 581.92 83.56 591.89 66.29 571.95 66.29"/>
  </g>
  <g>
    <polyline class="st1" points="266.2 509.12 287.62 509.12 287.62 545.35"/>
    <polygon class="st13" points="269.11 499.15 251.84 509.12 269.11 519.09 269.11 499.15"/>
  </g>
  <g>
    <polyline class="st1" points="618.74 400 618.74 429.23 552.32 429.23"/>
    <polygon class="st13" points="555.23 419.26 537.96 429.23 555.23 439.2 555.23 419.26"/>
  </g>
  <g>
    <polygon class="st4" points="665.36 268.47 603.19 268.47 572.11 299.05 603.19 329.63 665.36 329.63 696.44 299.05 665.36 268.47"/>
    <g class="st19">
      <g class="st19">
        <g class="st19">
          <g class="st19">
            <g class="st19">
              <g class="st19">
                <text class="st16" transform="translate(615.47 294.78)"><tspan x="0" y="0">V or A</tspan></text>
                <text class="st16" transform="translate(615.47 315.18)"><tspan x="0" y="0">or cps</tspan></text>
              </g>
            </g>
          </g>
        </g>
      </g>
    </g>
    <path class="st0" d="M596.77,288.8c0-1.41,1.15-2.56,2.56-2.56s2.56,1.15,2.56,2.56-1.15,2.56-2.56,2.56-2.56-1.15-2.56-2.56ZM594.2,296.49c0-.94.76-1.71,1.71-1.71h3.42c.94,0,1.71.76,1.71,1.71v11.96h1.71c.94,0,1.71.76,1.71,1.71s-.76,1.71-1.71,1.71h-6.83c-.94,0-1.71-.76-1.71-1.71s.76-1.71,1.71-1.71h1.71v-10.25h-1.71c-.94,0-1.71-.76-1.71-1.71h-.01Z"/>
  </g>
  <g>
    <g>
      <rect class="st17" x="1.53" y="1" width="283.19" height="322.82" rx="16.31" ry="16.31"/>
      <rect class="st5" x="1.53" y="1" width="283.19" height="322.82" rx="16.31" ry="16.31"/>
    </g>
    <g class="st19">
      <g class="st19">
        <g class="st19">
          <g class="st19">
            <g class="st19">
              <g class="st19">
                <text class="st14" transform="translate(62.89 206.79)"><tspan x="0" y="0">information functions</tspan></text>
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
                <text class="st14" transform="translate(62.89 167.35)"><tspan x="0" y="0">processing functions</tspan></text>
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
                <text class="st14" transform="translate(61.69 80.98)"><tspan x="0" y="0">isoreader core functions</tspan></text>
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
                <text class="st15" transform="translate(267.35 688.98)"><tspan x="0" y="0">to downstream processing</tspan></text>
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
                <text class="st14" transform="translate(61.69 110.74)"><tspan x="0" y="0">auxiliary functions</tspan></text>
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
                <text class="st14" transform="translate(61.69 134.67)"><tspan x="0" y="0">(optional)</tspan></text>
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
                <text class="st14" transform="translate(62.89 246.97)"><tspan x="0" y="0">visualization functions</tspan></text>
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
                <text class="st14" transform="translate(62.89 281.75)"><tspan x="0" y="0">core functions, essential</tspan></text>
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
                <text class="st14" transform="translate(62.89 305.68)"><tspan x="0" y="0">input from user</tspan></text>
              </g>
            </g>
          </g>
        </g>
      </g>
    </g>
    <path class="st0" d="M23.03,162.03l1.23-12.12c.09-.85.67-1.49,1.38-1.49h7.29c.65,0,1.17.64,1.17,1.44,0,.17-.03.35-.07.5l-2,6.56h6.03c.87,0,1.59.87,1.59,1.95,0,.39-.1.78-.28,1.1l-8.31,14.93c-.26.46-.67.73-1.12.73h-.13c-.68,0-1.23-.68-1.23-1.51,0-.12.01-.24.04-.37l2.03-10.03h-6.23c-.77,0-1.38-.76-1.38-1.7h-.01Z"/>
    <g>
      <rect class="st23" x="16.79" y="60.03" width="31.26" height="31.26" rx="5.69" ry="5.69"/>
      <rect class="st7" x="16.79" y="60.03" width="31.26" height="31.26" rx="5.69" ry="5.69"/>
    </g>
    <g>
      <rect class="st23" x="16.79" y="101.76" width="31.26" height="31.26" rx="5.69" ry="5.69"/>
      <rect class="st10" x="16.79" y="101.76" width="31.26" height="31.26" rx="5.69" ry="5.69"/>
    </g>
    <g>
      <polygon class="st3" points="33.77 270.77 32.52 270.77 14.61 288.39 32.52 306.02 33.77 306.02 51.69 288.39 33.77 270.77"/>
      <path class="st0" d="M31.89,282.49c0-.82.66-1.48,1.48-1.48s1.48.66,1.48,1.48-.66,1.48-1.48,1.48-1.48-.66-1.48-1.48ZM30.41,286.92c0-.54.44-.98.98-.98h1.97c.54,0,.98.44.98.98v6.89h.98c.54,0,.98.44.98.98s-.44.98-.98.98h-3.94c-.54,0-.98-.44-.98-.98s.44-.98.98-.98h.98v-5.91h-.98c-.54,0-.98-.44-.98-.98h0Z"/>
    </g>
    <path class="st0" d="M31.33,216.01c8.03,0,14.54-6.51,14.54-14.54s-6.51-14.54-14.54-14.54-14.54,6.51-14.54,14.54,6.51,14.54,14.54,14.54ZM29.06,206.01h1.36v-3.63h-1.36c-.76,0-1.36-.61-1.36-1.36s.61-1.36,1.36-1.36h2.73c.76,0,1.36.61,1.36,1.36v5h.45c.76,0,1.36.61,1.36,1.36s-.61,1.36-1.36,1.36h-4.54c-.76,0-1.36-.61-1.36-1.36s.61-1.36,1.36-1.36h0ZM31.33,194.2c1,0,1.82.81,1.82,1.82s-.81,1.82-1.82,1.82-1.82-.81-1.82-1.82.81-1.82,1.82-1.82Z"/>
    <path class="st0" d="M21.35,231.67c0-.92-.74-1.66-1.66-1.66s-1.66.74-1.66,1.66v17.47c0,2.3,1.86,4.16,4.16,4.16h20.8c.92,0,1.66-.74,1.66-1.66s-.74-1.66-1.66-1.66h-20.8c-.46,0-.83-.37-.83-.83v-17.47h-.01,0ZM42.49,236.17c.65-.65.65-1.71,0-2.36s-1.71-.65-2.36,0l-5.47,5.48-2.98-2.98c-.65-.65-1.71-.65-2.36,0l-5.82,5.82c-.65.65-.65,1.71,0,2.36s1.71.65,2.36,0l4.65-4.64,2.98,2.98c.65.65,1.71.65,2.36,0l6.65-6.65h-.01Z"/>
    <g class="st19">
      <g class="st19">
        <g class="st19">
          <g class="st19">
            <g class="st19">
              <g class="st19">
                <text class="st14" transform="translate(62.82 38.78)"><tspan x="0" y="0">data files</tspan></text>
              </g>
            </g>
          </g>
        </g>
      </g>
    </g>
    <g>
      <rect class="st24" x="16.47" y="17.2" width="32.52" height="32.52" rx="1.55" ry="1.55"/>
      <rect class="st8" x="16.47" y="17.2" width="32.52" height="32.52" rx="1.55" ry="1.55"/>
    </g>
  </g>
  <a xlink:href="https://isoreader2.isoverse.org/reference/ir_read_isofiles.html">
    <g>
      <g>
        <rect class="st23" x="299.71" y="193.54" width="172.29" height="41.92" rx="4.23" ry="4.23"/>
        <rect class="st7" x="299.71" y="193.54" width="172.29" height="41.92" rx="4.23" ry="4.23"/>
      </g>
      <path class="st0" d="M314.03,214.5l1.12-10.98c.08-.77.61-1.35,1.25-1.35h6.61c.59,0,1.06.58,1.06,1.3,0,.15-.02.31-.07.46l-1.81,5.94h5.46c.79,0,1.44.79,1.44,1.77,0,.36-.09.7-.25,1l-7.53,13.53c-.23.41-.61.66-1.01.66h-.11c-.62,0-1.12-.62-1.12-1.37,0-.11.01-.22.04-.33l1.84-9.08h-5.64c-.69,0-1.25-.69-1.25-1.54h-.03,0Z"/>
      <g class="st19">
        <g class="st19">
          <g class="st19">
            <g class="st19">
              <g class="st19">
                <g class="st19">
                  <text class="st16" transform="translate(339.56 219.04)"><tspan x="0" y="0">ir_read_isofiles</tspan></text>
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
        <rect class="st23" x="299.71" y="106.99" width="172.29" height="41.92" rx="4.23" ry="4.23"/>
        <rect class="st7" x="299.71" y="106.99" width="172.29" height="41.92" rx="4.23" ry="4.23"/>
      </g>
      <path class="st0" d="M314.03,127.95l1.12-10.98c.08-.77.61-1.35,1.25-1.35h6.61c.59,0,1.06.58,1.06,1.3,0,.15-.02.31-.07.46l-1.81,5.94h5.46c.79,0,1.44.79,1.44,1.77,0,.36-.09.7-.25,1l-7.53,13.53c-.23.41-.61.66-1.01.66h-.11c-.62,0-1.12-.62-1.12-1.37,0-.11.01-.22.04-.33l1.84-9.08h-5.64c-.69,0-1.25-.69-1.25-1.54h-.03,0Z"/>
      <g class="st19">
        <g class="st19">
          <g class="st19">
            <g class="st19">
              <g class="st19">
                <g class="st19">
                  <text class="st16" transform="translate(339.56 132.49)"><tspan x="0" y="0">ir_find_isofiles</tspan></text>
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
        <rect class="st23" x="299.52" y="278.09" width="207.29" height="41.92" rx="4.23" ry="4.23"/>
        <rect class="st7" x="299.52" y="278.09" width="207.29" height="41.92" rx="4.23" ry="4.23"/>
      </g>
      <path class="st0" d="M313.84,299.05l1.12-10.98c.08-.77.61-1.35,1.25-1.35h6.61c.59,0,1.06.58,1.06,1.3,0,.15-.02.31-.07.46l-1.81,5.94h5.46c.79,0,1.44.79,1.44,1.77,0,.36-.09.7-.25,1l-7.53,13.53c-.23.41-.61.66-1.01.66h-.11c-.62,0-1.12-.62-1.12-1.37,0-.11.01-.22.04-.33l1.84-9.08h-5.64c-.69,0-1.25-.69-1.25-1.54h-.03,0Z"/>
      <g class="st19">
        <g class="st19">
          <g class="st19">
            <g class="st19">
              <g class="st19">
                <g class="st19">
                  <text class="st16" transform="translate(338.37 303.59)"><tspan x="0" y="0">ir_aggregate_isofiles</tspan></text>
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
        <path class="st23" d="M71.99,434.92h215.03c3.12,0,11.65,2.53,11.65,5.65v30.62c0,3.12-8.53,5.65-11.65,5.65H71.99c-3.12,0-5.65-2.53-5.65-5.65v-30.62c0-3.12,2.53-5.65,5.65-5.65Z"/>
        <path class="st6" d="M71.99,434.92h215.03c3.12,0,11.65,2.53,11.65,5.65v30.62c0,3.12-8.53,5.65-11.65,5.65H71.99c-3.12,0-5.65-2.53-5.65-5.65v-30.62c0-3.12,2.53-5.65,5.65-5.65Z"/>
      </g>
      <g class="st19">
        <g class="st19">
          <g class="st19">
            <g class="st19">
              <g class="st19">
                <g class="st19">
                  <text class="st16" transform="translate(111.19 460.41)"><tspan x="0" y="0">ir_plot_scans</tspan></text>
                </g>
              </g>
            </g>
          </g>
        </g>
      </g>
      <path class="st0" d="M76.63,445.71c0-.92-.74-1.66-1.66-1.66s-1.66.74-1.66,1.66v17.47c0,2.3,1.86,4.16,4.16,4.16h20.8c.92,0,1.66-.74,1.66-1.66s-.74-1.66-1.66-1.66h-20.8c-.46,0-.83-.37-.83-.83v-17.47h-.01,0ZM97.77,450.21c.65-.65.65-1.71,0-2.36s-1.71-.65-2.36,0l-5.47,5.48-2.98-2.98c-.65-.65-1.71-.65-2.36,0l-5.82,5.82c-.65.65-.65,1.71,0,2.36s1.71.65,2.36,0l4.65-4.64,2.98,2.98c.65.65,1.71.65,2.36,0l6.65-6.65h-.01Z"/>
    </g>
  </a>
  <g>
    <line class="st1" x1="383.44" y1="408.48" x2="322.53" y2="408.48"/>
    <polygon class="st13" points="325.44 398.51 308.17 408.48 325.44 418.45 325.44 398.51"/>
  </g>
  <a xlink:href="https://isoreader2.isoverse.org/reference/ir_plot_dual_inlet.html">
    <g>
      <g>
        <path class="st23" d="M71.99,386.12h215.03c3.12,0,11.65,2.53,11.65,5.65v30.62c0,3.12-8.53,5.65-11.65,5.65H71.99c-3.12,0-5.65-2.53-5.65-5.65v-30.62c0-3.12,2.53-5.65,5.65-5.65Z"/>
        <path class="st6" d="M71.99,386.12h215.03c3.12,0,11.65,2.53,11.65,5.65v30.62c0,3.12-8.53,5.65-11.65,5.65H71.99c-3.12,0-5.65-2.53-5.65-5.65v-30.62c0-3.12,2.53-5.65,5.65-5.65Z"/>
      </g>
      <g class="st19">
        <g class="st19">
          <g class="st19">
            <g class="st19">
              <g class="st19">
                <g class="st19">
                  <text class="st16" transform="translate(111.19 411.61)"><tspan x="0" y="0">ir_plot_dual_inlet</tspan></text>
                </g>
              </g>
            </g>
          </g>
        </g>
      </g>
      <path class="st0" d="M76.63,396.91c0-.92-.74-1.66-1.66-1.66s-1.66.74-1.66,1.66v17.47c0,2.3,1.86,4.16,4.16,4.16h20.8c.92,0,1.66-.74,1.66-1.66s-.74-1.66-1.66-1.66h-20.8c-.46,0-.83-.37-.83-.83v-17.47h-.01,0ZM97.77,401.41c.65-.65.65-1.71,0-2.36s-1.71-.65-2.36,0l-5.47,5.48-2.98-2.98c-.65-.65-1.71-.65-2.36,0l-5.82,5.82c-.65.65-.65,1.71,0,2.36s1.71.65,2.36,0l4.65-4.64,2.98,2.98c.65.65,1.71.65,2.36,0l6.65-6.65h-.01Z"/>
    </g>
  </a>
  <g>
    <line class="st1" x1="383.44" y1="456.35" x2="322.53" y2="456.35"/>
    <polygon class="st13" points="325.44 446.38 308.17 456.35 325.44 466.32 325.44 446.38"/>
  </g>
  <a xlink:href="https://isoreader2.isoverse.org/reference/ir_plot_continuous_flow.html">
    <g>
      <g>
        <path class="st23" d="M71.99,338.24h215.03c3.12,0,11.65,2.53,11.65,5.65v30.62c0,3.12-8.53,5.65-11.65,5.65H71.99c-3.12,0-5.65-2.53-5.65-5.65v-30.62c0-3.12,2.53-5.65,5.65-5.65Z"/>
        <path class="st6" d="M71.99,338.24h215.03c3.12,0,11.65,2.53,11.65,5.65v30.62c0,3.12-8.53,5.65-11.65,5.65H71.99c-3.12,0-5.65-2.53-5.65-5.65v-30.62c0-3.12,2.53-5.65,5.65-5.65Z"/>
      </g>
      <g class="st19">
        <g class="st19">
          <g class="st19">
            <g class="st19">
              <g class="st19">
                <g class="st19">
                  <text class="st16" transform="translate(111.19 363.73)"><tspan x="0" y="0">ir_plot_continuous_flow</tspan></text>
                </g>
              </g>
            </g>
          </g>
        </g>
      </g>
      <path class="st0" d="M76.63,349.03c0-.92-.74-1.66-1.66-1.66s-1.66.74-1.66,1.66v17.47c0,2.3,1.86,4.16,4.16,4.16h20.8c.92,0,1.66-.74,1.66-1.66s-.74-1.66-1.66-1.66h-20.8c-.46,0-.83-.37-.83-.83v-17.47h-.01,0ZM97.77,353.53c.65-.65.65-1.71,0-2.36s-1.71-.65-2.36,0l-5.47,5.48-2.98-2.98c-.65-.65-1.71-.65-2.36,0l-5.82,5.82c-.65.65-.65,1.71,0,2.36s1.71.65,2.36,0l4.65-4.64,2.98,2.98c.65.65,1.71.65,2.36,0l6.65-6.65h-.01Z"/>
    </g>
  </a>
  <a xlink:href="https://isoreader2.isoverse.org/reference/ir_find_isofiles.html">
    <g>
      <g>
        <rect class="st23" x="529.22" y="94.36" width="224.14" height="41.92" rx="4.23" ry="4.23"/>
        <rect class="st2" x="529.22" y="94.36" width="224.14" height="41.92" rx="4.23" ry="4.23"/>
      </g>
      <path class="st0" d="M543.54,116.32l1.12-10.98c.08-.77.61-1.35,1.25-1.35h6.61c.59,0,1.06.58,1.06,1.3,0,.15-.02.31-.07.46l-1.81,5.94h5.46c.79,0,1.44.79,1.44,1.77,0,.36-.09.7-.25,1l-7.53,13.53c-.23.41-.61.66-1.01.66h-.11c-.62,0-1.12-.62-1.12-1.37,0-.11,0-.22.04-.33l1.84-9.08h-5.64c-.69,0-1.25-.69-1.25-1.54h-.03,0Z"/>
      <g class="st19">
        <g class="st19">
          <g class="st19">
            <g class="st19">
              <g class="st19">
                <g class="st19">
                  <text class="st16" transform="translate(565.33 120.85)"><tspan x="0" y="0">ir_find_continuous_flow</tspan></text>
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
        <rect class="st23" x="529.41" y="144.74" width="178.14" height="41.92" rx="4.23" ry="4.23"/>
        <rect class="st2" x="529.41" y="144.74" width="178.14" height="41.92" rx="4.23" ry="4.23"/>
      </g>
      <path class="st0" d="M543.73,166.7l1.12-10.98c.08-.77.61-1.35,1.25-1.35h6.61c.59,0,1.06.58,1.06,1.3,0,.15-.02.31-.07.46l-1.81,5.94h5.46c.79,0,1.44.79,1.44,1.77,0,.36-.09.7-.25,1l-7.53,13.53c-.23.41-.61.66-1.01.66h-.11c-.62,0-1.12-.62-1.12-1.37,0-.11,0-.22.04-.33l1.84-9.08h-5.64c-.69,0-1.25-.69-1.25-1.54h-.03,0Z"/>
      <g class="st19">
        <g class="st19">
          <g class="st19">
            <g class="st19">
              <g class="st19">
                <g class="st19">
                  <text class="st16" transform="translate(565.39 171.23)"><tspan x="0" y="0">ir_find_dual_inlet</tspan></text>
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
        <rect class="st23" x="528.79" y="195.82" width="156.14" height="41.92" rx="4.23" ry="4.23"/>
        <rect class="st2" x="528.79" y="195.82" width="156.14" height="41.92" rx="4.23" ry="4.23"/>
      </g>
      <path class="st0" d="M543.11,217.78l1.12-10.98c.08-.77.61-1.35,1.25-1.35h6.61c.59,0,1.06.58,1.06,1.3,0,.15-.02.31-.07.46l-1.81,5.94h5.46c.79,0,1.44.79,1.44,1.77,0,.36-.09.7-.25,1l-7.53,13.53c-.23.41-.61.66-1.01.66h-.11c-.62,0-1.12-.62-1.12-1.37,0-.11,0-.22.04-.33l1.84-9.08h-5.64c-.69,0-1.25-.69-1.25-1.54h-.03,0Z"/>
      <g class="st19">
        <g class="st19">
          <g class="st19">
            <g class="st19">
              <g class="st19">
                <g class="st19">
                  <text class="st16" transform="translate(568.64 222.31)"><tspan x="0" y="0">ir_find_scans</tspan></text>
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
        <rect class="st23" x="138.15" y="543.55" width="160.52" height="41.92" rx="5.08" ry="5.08"/>
        <rect class="st12" x="138.15" y="543.55" width="160.52" height="41.92" rx="5.08" ry="5.08"/>
      </g>
      <g class="st19">
        <g class="st19">
          <g class="st19">
            <g class="st19">
              <g class="st19">
                <g class="st19">
                  <text class="st16" transform="translate(183 569.04)"><tspan x="0" y="0">ir_get_data</tspan></text>
                </g>
              </g>
            </g>
          </g>
        </g>
      </g>
      <path class="st0" d="M159.24,579.05c8.03,0,14.54-6.51,14.54-14.54s-6.51-14.54-14.54-14.54-14.54,6.51-14.54,14.54,6.51,14.54,14.54,14.54ZM156.97,569.05h1.36v-3.63h-1.36c-.76,0-1.36-.61-1.36-1.36s.61-1.36,1.36-1.36h2.73c.76,0,1.36.61,1.36,1.36v5h.45c.76,0,1.36.61,1.36,1.36s-.61,1.36-1.36,1.36h-4.54c-.76,0-1.36-.61-1.36-1.36s.61-1.36,1.36-1.36h0ZM159.24,557.24c1,0,1.82.81,1.82,1.82s-.81,1.82-1.82,1.82-1.82-.81-1.82-1.82.81-1.82,1.82-1.82Z"/>
    </g>
  </a>
  <g>
    <g>
      <rect class="st24" x="299.92" y="8.38" width="224.34" height="53.98" rx="4.45" ry="4.45"/>
      <rect class="st8" x="299.92" y="8.38" width="224.34" height="53.98" rx="4.45" ry="4.45"/>
    </g>
    <g class="st19">
      <g class="st19">
        <g class="st19">
          <g class="st19">
            <g class="st19">
              <g class="st19">
                <text class="st16" transform="translate(313.61 56.28)"><tspan x="0" y="0">isotope data files/archives</tspan></text>
              </g>
            </g>
          </g>
        </g>
      </g>
    </g>
    <path class="st0" d="M323.35,38.97c.47,0,.86-.39.86-.86v-15.44h-4.29c-.95,0-1.72-.77-1.72-1.72v-4.29h-8.58c-.47,0-.86.39-.86.86v20.58c0,.47.39.86.86.86h13.73ZM306.2,17.53c0-1.89,1.54-3.43,3.43-3.43h8.87c.91,0,1.78.36,2.43,1l4.85,4.85c.64.64,1,1.52,1,2.43v15.73c0,1.89-1.54,3.43-3.43,3.43h-13.72c-1.89,0-3.43-1.54-3.43-3.43v-20.58h0Z"/>
    <g class="st19">
      <g class="st19">
        <g class="st19">
          <g class="st19">
            <g class="st19">
              <text class="st20" transform="translate(316.49 37.67) rotate(-90)"><tspan x="0" y="0">dxf</tspan></text>
            </g>
          </g>
        </g>
      </g>
    </g>
    <path class="st0" d="M347.09,38.96c.47,0,.86-.39.86-.86v-15.44h-4.29c-.95,0-1.72-.77-1.72-1.72v-4.29h-8.58c-.47,0-.86.39-.86.86v20.58c0,.47.39.86.86.86h13.73ZM329.94,17.52c0-1.89,1.54-3.43,3.43-3.43h8.87c.91,0,1.78.36,2.43,1l4.85,4.85c.64.64,1,1.52,1,2.43v15.73c0,1.89-1.54,3.43-3.43,3.43h-13.72c-1.89,0-3.43-1.54-3.43-3.43v-20.58h0Z"/>
    <g class="st19">
      <g class="st19">
        <g class="st19">
          <g class="st19">
            <g class="st19">
              <text class="st20" transform="translate(340.23 37.66) rotate(-90)"><tspan x="0" y="0">cf</tspan></text>
            </g>
          </g>
        </g>
      </g>
    </g>
    <path class="st0" d="M370.99,38.96c.47,0,.86-.39.86-.86v-15.44h-4.29c-.95,0-1.72-.77-1.72-1.72v-4.29h-8.58c-.47,0-.86.39-.86.86v20.58c0,.47.39.86.86.86h13.73ZM353.84,17.52c0-1.89,1.54-3.43,3.43-3.43h8.87c.91,0,1.78.36,2.43,1l4.85,4.85c.64.64,1,1.52,1,2.43v15.73c0,1.89-1.54,3.43-3.43,3.43h-13.72c-1.89,0-3.43-1.54-3.43-3.43v-20.58h0Z"/>
    <g class="st19">
      <g class="st19">
        <g class="st19">
          <g class="st19">
            <g class="st19">
              <text class="st20" transform="translate(364.13 37.66) rotate(-90)"><tspan x="0" y="0">bch</tspan></text>
            </g>
          </g>
        </g>
      </g>
    </g>
    <path class="st0" d="M395.21,38.97c.47,0,.86-.39.86-.86v-15.44h-4.29c-.95,0-1.72-.77-1.72-1.72v-4.29h-8.58c-.47,0-.86.39-.86.86v20.58c0,.47.39.86.86.86h13.73ZM378.06,17.53c0-1.89,1.54-3.43,3.43-3.43h8.87c.91,0,1.78.36,2.43,1l4.85,4.85c.64.64,1,1.52,1,2.43v15.73c0,1.89-1.54,3.43-3.43,3.43h-13.72c-1.89,0-3.43-1.54-3.43-3.43v-20.58h0Z"/>
    <g class="st19">
      <g class="st19">
        <g class="st19">
          <g class="st19">
            <g class="st19">
              <text class="st20" transform="translate(388.35 37.67) rotate(-90)"><tspan x="0" y="0">iarc</tspan></text>
            </g>
          </g>
        </g>
      </g>
    </g>
    <path class="st0" d="M419.29,38.97c.47,0,.86-.39.86-.86v-15.44h-4.29c-.95,0-1.72-.77-1.72-1.72v-4.29h-8.58c-.47,0-.86.39-.86.86v20.58c0,.47.39.86.86.86h13.73ZM402.14,17.53c0-1.89,1.54-3.43,3.43-3.43h8.87c.91,0,1.78.36,2.43,1l4.85,4.85c.64.64,1,1.52,1,2.43v15.73c0,1.89-1.54,3.43-3.43,3.43h-13.72c-1.89,0-3.43-1.54-3.43-3.43v-20.58h0Z"/>
    <g class="st19">
      <g class="st19">
        <g class="st19">
          <g class="st19">
            <g class="st19">
              <text class="st20" transform="translate(412.43 37.67) rotate(-90)"><tspan x="0" y="0">larc</tspan></text>
            </g>
          </g>
        </g>
      </g>
    </g>
    <path class="st0" d="M443.03,38.96c.47,0,.86-.39.86-.86v-15.44h-4.29c-.95,0-1.72-.77-1.72-1.72v-4.29h-8.58c-.47,0-.86.39-.86.86v20.58c0,.47.39.86.86.86h13.73ZM425.88,17.52c0-1.89,1.54-3.43,3.43-3.43h8.87c.91,0,1.78.36,2.43,1l4.85,4.85c.64.64,1,1.52,1,2.43v15.73c0,1.89-1.54,3.43-3.43,3.43h-13.72c-1.89,0-3.43-1.54-3.43-3.43v-20.58h0Z"/>
    <g class="st19">
      <g class="st19">
        <g class="st19">
          <g class="st19">
            <g class="st19">
              <text class="st21" transform="translate(434.17 37.66) rotate(-90)"><tspan x="0" y="0">imexp</tspan></text>
            </g>
          </g>
        </g>
      </g>
    </g>
    <path class="st0" d="M466.93,38.96c.47,0,.86-.39.86-.86v-15.44h-4.29c-.95,0-1.72-.77-1.72-1.72v-4.29h-8.58c-.47,0-.86.39-.86.86v20.58c0,.47.39.86.86.86h13.73ZM449.78,17.52c0-1.89,1.54-3.43,3.43-3.43h8.87c.91,0,1.78.36,2.43,1l4.85,4.85c.64.64,1,1.52,1,2.43v15.73c0,1.89-1.54,3.43-3.43,3.43h-13.72c-1.89,0-3.43-1.54-3.43-3.43v-20.58h0Z"/>
    <g class="st19">
      <g class="st19">
        <g class="st19">
          <g class="st19">
            <g class="st19">
              <text class="st20" transform="translate(460.07 37.66) rotate(-90)"><tspan x="0" y="0">did</tspan></text>
            </g>
          </g>
        </g>
      </g>
    </g>
    <path class="st0" d="M491.14,38.97c.47,0,.86-.39.86-.86v-15.44h-4.29c-.95,0-1.72-.77-1.72-1.72v-4.29h-8.58c-.47,0-.86.39-.86.86v20.58c0,.47.39.86.86.86h13.73ZM473.99,17.53c0-1.89,1.54-3.43,3.43-3.43h8.87c.91,0,1.78.36,2.43,1l4.85,4.85c.64.64,1,1.52,1,2.43v15.73c0,1.89-1.54,3.43-3.43,3.43h-13.72c-1.89,0-3.43-1.54-3.43-3.43v-20.58h0Z"/>
    <g class="st19">
      <g class="st19">
        <g class="st19">
          <g class="st19">
            <g class="st19">
              <text class="st20" transform="translate(484.28 37.67) rotate(-90)"><tspan x="0" y="0">caf</tspan></text>
            </g>
          </g>
        </g>
      </g>
    </g>
    <path class="st0" d="M514.8,38.97c.47,0,.86-.39.86-.86v-15.44h-4.29c-.95,0-1.72-.77-1.72-1.72v-4.29h-8.58c-.47,0-.86.39-.86.86v20.58c0,.47.39.86.86.86h13.73ZM497.65,17.53c0-1.89,1.54-3.43,3.43-3.43h8.87c.91,0,1.78.36,2.43,1l4.85,4.85c.64.64,1,1.52,1,2.43v15.73c0,1.89-1.54,3.43-3.43,3.43h-13.72c-1.89,0-3.43-1.54-3.43-3.43v-20.58h0Z"/>
    <g class="st19">
      <g class="st19">
        <g class="st19">
          <g class="st19">
            <g class="st19">
              <text class="st20" transform="translate(507.94 37.67) rotate(-90)"><tspan x="0" y="0">scn</tspan></text>
            </g>
          </g>
        </g>
      </g>
    </g>
  </g>
  <g>
    <g>
      <rect class="st24" x="575.74" y="358.01" width="209.29" height="41.92" rx="4.73" ry="4.73"/>
      <rect class="st7" x="575.74" y="358.01" width="209.29" height="41.92" rx="4.73" ry="4.73"/>
    </g>
    <g class="st19">
      <g class="st19">
        <g class="st19">
          <g class="st19">
            <g class="st19">
              <text class="st16" transform="translate(616.16 383.54)"><tspan x="0" y="0">storage file (parquet)</tspan></text>
            </g>
          </g>
        </g>
      </g>
    </g>
    <g>
      <g>
        <line class="st11" x1="590.33" y1="377.55" x2="605.18" y2="377.55"/>
        <line class="st11" x1="590.32" y1="380.55" x2="605.17" y2="380.55"/>
        <line class="st11" x1="590.35" y1="383.54" x2="605.19" y2="383.54"/>
        <line class="st11" x1="590.34" y1="386.54" x2="605.18" y2="386.54"/>
        <line class="st11" x1="593.73" y1="377.55" x2="593.73" y2="390.23"/>
        <line class="st11" x1="597.74" y1="377.55" x2="597.74" y2="390.23"/>
        <line class="st11" x1="601.96" y1="377.55" x2="601.96" y2="390.23"/>
      </g>
      <path class="st0" d="M590.38,388.66v-19.72c0-.46.37-.82.82-.82h8.22v4.1c0,.91.74,1.65,1.65,1.65h4.1v14.79c0,.46-.37.82-.82.82h-13.15c-.46,0-.82-.37-.82-.82h0ZM591.2,365.66c-1.81,0-3.28,1.47-3.28,3.28v19.72c0,1.81,1.47,3.28,3.28,3.28h13.15c1.81,0,3.28-1.47,3.28-3.28v-15.07c0-.88-.34-1.71-.96-2.33l-4.65-4.65c-.61-.61-1.44-.96-2.32-.96h-8.5Z"/>
    </g>
  </g>
  <g>
    <line class="st9" x1="573.83" y1="299.05" x2="530.62" y2="299.05"/>
    <polygon class="st13" points="534.13 287.08 513.4 299.05 534.13 311.02 534.13 287.08"/>
  </g>
  <g>
    <g>
      <rect class="st24" x="1.5" y="600.46" width="248.29" height="41.92" rx="4.73" ry="4.73"/>
      <rect class="st7" x="1.5" y="600.46" width="248.29" height="41.92" rx="4.73" ry="4.73"/>
    </g>
    <g class="st19">
      <g class="st19">
        <g class="st19">
          <g class="st19">
            <g class="st19">
              <text class="st16" transform="translate(41.92 625.99)"><tspan x="0" y="0">output spreadsheet (xlsx)</tspan></text>
            </g>
          </g>
        </g>
      </g>
    </g>
    <g>
      <g>
        <line class="st11" x1="16.09" y1="620" x2="30.94" y2="620"/>
        <line class="st11" x1="16.08" y1="623" x2="30.93" y2="623"/>
        <line class="st11" x1="16.11" y1="625.99" x2="30.95" y2="625.99"/>
        <line class="st11" x1="16.1" y1="628.99" x2="30.94" y2="628.99"/>
        <line class="st11" x1="19.49" y1="620" x2="19.49" y2="632.68"/>
        <line class="st11" x1="23.5" y1="620" x2="23.5" y2="632.68"/>
        <line class="st11" x1="27.72" y1="620" x2="27.72" y2="632.68"/>
      </g>
      <path class="st0" d="M16.14,631.11v-19.72c0-.46.37-.82.82-.82h8.22v4.1c0,.91.74,1.65,1.65,1.65h4.1v14.79c0,.46-.37.82-.82.82h-13.15c-.46,0-.82-.37-.82-.82h0ZM16.96,608.11c-1.81,0-3.28,1.47-3.28,3.28v19.72c0,1.81,1.47,3.28,3.28,3.28h13.15c1.81,0,3.28-1.47,3.28-3.28v-15.07c0-.88-.34-1.71-.96-2.33l-4.65-4.65c-.61-.61-1.44-.96-2.32-.96h-8.5Z"/>
      <g class="st19">
        <g class="st19">
          <g class="st19">
            <g class="st19">
              <text class="st18" transform="translate(22.94 630.69) rotate(-90)"><tspan x="0" y="0">xlsx</tspan></text>
            </g>
          </g>
        </g>
      </g>
    </g>
  </g>
</svg>

```




## Getting help

If you encounter a bug, please file an issue with a minimal reproducible example on [GitHub](https://github.com/isoverse/isoreader2/issues). Example files are very helpful for fixing bugs so please consider including an example data file (you will have to attach it as a zip archive).

## isoverse <a href='http://www.isoverse.org'><img src='man/figures/isoverse_logo_thumb.png' align="right" width="100"/></a>

This package is part of the isoverse suite of data tools for stable isotopes. If you like the functionality that isoverse packages provide, please help us spread the word and include an isoverse or individual package logo on one of your posters or slides. All logos are posted in high resolution in [this repository](https://github.com/isoverse/logos). If you have suggestions for new features or other constructive feedback, please let us know on this short [feeback form](https://www.isoverse.org/feedback/).

## Funding <a href='https://www.nsf.gov/'><img src='man/figures/NSF_logo.svg' align="right" width="100"/></a>

This project is supported by a grant from the US National Science Foundation ([EAR-2411458](https://www.nsf.gov/awardsearch/show-award?AWD_ID=2411458)) to Sebastian Kopf. 

