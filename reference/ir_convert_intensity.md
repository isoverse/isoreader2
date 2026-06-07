# Convert intensity between units

Note: this function is rarely called directly, it's run as part of
ir_aggregate_isofiles to standardize the trace/cycle/scan datasets
before aggregation.

## Usage

``` r
ir_convert_intensity(
  dataset,
  resistors = NULL,
  units = c("mV", "V", "fA", "pA", "nA", "\u00b5A", "mA", "A", "cps")
)
```

## Arguments

- dataset:

  a data frame with columns `species`, `channel`, and an
  `intensity.<unit>` column (e.g. `intensity.mV`, `intensity.nA`).

- resistors:

  a data frame with columns `species`, `channel`, and `resistance.Ohm`.
  Required when converting between voltage and current/CPS; ignored
  otherwise. If a `config` column is present in both `dataset` and
  `resistors` it is included in the join key.

- units:

  target unit, one of `"mV"`, `"V"`, `"fA"`, `"pA"`, `"nA"`,
  `"\u00b5A"`, `"mA"`, `"A"`, `"cps"`.

## Details

Converts the intensity column of a dataset between voltage, current, and
count-per-second units using `A = CPS * e` and `V = A * R`.
Automatically detects the source unit from any `intensity.<unit>` column
present in `dataset`. Joins `resistors` by `species` and `channel` (plus
`config` when present in both) only when the conversion path crosses the
A/V boundary. Returns `dataset` with the source intensity column
replaced by `intensity.<units>`.
