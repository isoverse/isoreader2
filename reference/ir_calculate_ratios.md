# Calculate isotope ratios

Calculate intensity ratios of each mass relative to a base mass for
every measurement in an
[`ir_aggregate_isofiles()`](https://isoreader2.isoverse.org/reference/ir_aggregate_isofiles.md)
result. Ratios are added directly to the `traces` (continuous flow),
`cycles` (dual inlet), and/or `scans` data present in the aggregated
data as two extra columns: `ratio_name` (e.g. `"29/28"`) and `ratio`.
The ratio at each `time.s`/`cycle`/`x` position (within every `uidx` and
`analysis`) is

## Usage

``` r
ir_calculate_ratios(
  aggregated_data,
  ...,
  num_add.V = 100,
  denom_add.V = num_add.V,
  num_add.nA = 0,
  denom_add.nA = num_add.nA,
  num_add.cps = 0,
  denom_add.cps = num_add.cps,
  normalize_ratios = NULL
)
```

## Arguments

- aggregated_data:

  datasets aggregated from
  [`ir_aggregate_isofiles()`](https://isoreader2.isoverse.org/reference/ir_aggregate_isofiles.md)
  (must include at least one of `traces`, `cycles`, or `scans`)

- ...:

  named base masses for individual species, e.g. `SO2 = 64, N2 = 28`.
  Species not listed here use their numerically lowest measured mass as
  the base mass.

- num_add.V, denom_add.V:

  additive offset (in volts) for the numerator and denominator
  intensities of voltage-unit data (`V`/`mV`). Applies to `traces` and
  `scans` only, not to `cycles` (dual inlet). Default `num_add.V = 100`,
  `denom_add.V = num_add.V`.

- num_add.nA, denom_add.nA:

  additive offset (in nanoamperes) for the numerator and denominator
  intensities of current-unit data (`A`/`mA`/`µA`/`nA`/`pA`/`fA`).
  Applies to `traces` and `scans` only, not to `cycles` (dual inlet).
  Default `num_add.nA = 0`, `denom_add.nA = num_add.nA`.

- num_add.cps, denom_add.cps:

  additive offset (in cps) for the numerator and denominator intensities
  of count-unit data (`cps`). Applies to `traces` and `scans` only, not
  to `cycles` (dual inlet). Default `num_add.cps = 0`,
  `denom_add.cps = num_add.cps`.

- normalize_ratios:

  `NULL` (default) for no normalization, or a function (e.g. `mean`,
  `median`, `min`, `max`) applied per `uidx`/`analysis`/`ratio_name`
  group; each ratio is divided by the value the function returns for its
  group's non-`NA` ratios.

## Value

the `aggregated_data` with `ratio_name` and `ratio` columns added to
each of the `traces`, `cycles`, and/or `scans` datasets that is present.
Both columns are `NA` for base mass rows (and for any species whose
requested base mass could not be found).

## Details

\$\$ratio = (I\_{mass} + num\\add) / (I\_{base} + denom\\add)\$\$

i.e. the intensity of the mass divided by the intensity of the base mass
of the same species, after adding an additive offset to numerator and
denominator (see below). Base mass rows are kept and have `NA` in both
columns. Calling this function again recomputes (overwrites) the
`ratio_name`/`ratio` columns. The resulting ratios are not constrained
in any way (they can be any value).

The base mass for a species is, by default, the numerically lowest mass
measured for that species. Override it for individual species via `...`
(e.g. `SO2 = 64`, `N2 = 28`).

## Additive offsets

The additive offsets apply to continuous-flow (`traces`) and `scans`
data **only**. Dual inlet (`cycles`) data is **not** offset and always
uses the plain ratio `I_mass / I_base` regardless of the
`num_add.*`/`denom_add.*` settings.

For traces and scans, which pair of additive offsets is used depends on
the intensity unit family of the data: voltage (`V`, `mV`) uses
`num_add.V`/`denom_add.V`, current (`A`, `mA`, `µA`, `nA`, `pA`, `fA`)
uses `num_add.nA`/`denom_add.nA`, and counts (`cps`) uses
`num_add.cps`/`denom_add.cps`. The offsets are specified in their
family's reference unit (volts, nanoamperes, cps) and are automatically
scaled to the data's actual intensity unit before being added. For
example, with `intensity.mV` data the default `num_add.V = 100` (volts)
is multiplied by 1000 and 100000 mV are added; with `intensity.pA` data
the default `num_add.nA = 0` would be multiplied by 1000 (1 nA = 1000
pA).

## Normalization

`normalize_ratios` is `NULL` by default (no normalization). Pass a
function to divide every ratio by the value that function returns for
its `uidx`/`analysis`/`ratio_name` group (the function receives the
group's non-`NA` ratios). For example `normalize_ratios = mean` centers
each ratio around 1, while `median`, `min`, or `max` normalize to the
group median, minimum, or maximum, respectively.
