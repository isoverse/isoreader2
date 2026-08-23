# isoreader2 0.7.0

## Syntax changes

Reading, aggregating, and all data operations behave exactly as in 0.6. However, there are three syntax changes to the plotting functions (and the corresponding `ir_generate_*_tibble()` helpers):

| 0.6 | 0.7 |
|---|---|
| `mass = NULL` / `ratio = NULL` selected **all** | they now select **none**; use `everything()` (or just drop the argument) for all |
| a partly matching `mass = c(44, 99)` silently kept `44` | it is now an error listing the available masses; use `any_of()` to ignore missing ones |
| the colour aesthetic defaulted to `trace` | it defaults to the new `color` column, so a trace and its ratios share one legend entry; pass `color = trace` for the old behavior |

## New features and fixes

* New `ir_plot_traces()`, the preferred name for what used to be
  `ir_plot_continuous_flow()`. It is the same function with the same arguments
  and return value, just named after the data it plots (`traces`) rather than
  the acquisition mode, which leaves room for trace data from other kinds of
  files in the future.

  `ir_plot_continuous_flow()` keeps working as an alias but is now
  soft-deprecated and passes everything on to `ir_plot_traces()`. Calling it
  directly warns (at most once every 8 hours):

  ```r
  dataset |> ir_plot_traces(facet = file_name)          # preferred
  dataset |> ir_plot_continuous_flow(facet = file_name) # deprecated
  ```

* The `mass` and `ratio` arguments of `ir_plot_traces()`,
  `ir_plot_dual_inlet()`, `ir_plot_scans()`, and the `ir_generate_*_tibble()`
  helpers now accept the full **tidyselect** syntax, evaluated as if the masses
  and ratio names present in the data were column names:

  ```r
  ir_plot_traces(dataset, mass = 44:48)              # a range of masses
  ir_plot_traces(dataset, mass = -"45")              # all but mass 45
  ir_plot_traces(dataset, ratio = starts_with("45")) # all 45/x ratios
  ```

  Both default to `everything()` (all masses / all available ratios) and both
  take `NULL` (or `c()`) for none. Unlike plain tidyselect, numbers select by
  name rather than by position, so `mass = 44:48` means the masses 44 to 48, not
  the 44th to 48th mass; a bare vector held in a variable also works without
  wrapping it in `all_of()`.

  **This inverts the previous meaning of `NULL`**, which used to select *all*
  masses/ratios: replace `mass = NULL` with `mass = everything()` (or just drop
  it, since it is the default) to keep the old behavior.

* `ir_generate_traces_tibble()`, `ir_generate_cycles_tibble()`, and
  `ir_generate_scans_tibble()` now also add a `color` column alongside `trace`.
  It groups every trace that shares a species and (numerator) mass into a single
  factor level labelled with all of them, so `"CO2: 45"` and `"CO2: 45/44"` both
  get `"CO2: 45, 45/44"`. Its levels follow the `trace` order (ascending
  species/mass).

* `ir_plot_traces()`, `ir_plot_dual_inlet()`, and `ir_plot_scans()` now
  map the colour aesthetic to that `color` column by default (previously
  `trace`). An intensity trace and its ratios are still drawn in the same colour
  and still as separate lines, but they now share **one** legend entry
  (`"CO2: 45, 45/44"`) instead of appearing as separate entries with a repeated
  colour. Pass `color = trace` to give every trace its own colour and legend entry.

* `drop_unused_levels` now applies at colour-group granularity: a group stays in
  the legend as long as any of its traces is visible, and keeps its full label.

* New `ir_filter_masses()`, which keeps only the requested masses in the
  `traces`, `cycles`, and `scans` of a dataset. Where `ir_filter_metadata()`
  selects files, this selects masses, **tidyselect** syntax is allowed:

  ```r
  dataset |> ir_filter_masses(44:46)                  # the CO2 masses
  dataset |> ir_filter_masses(-"45")                  # all but mass 45
  ```

  Any metadata record left without data afterwards is removed, and that removal
  cascades to the other datasets exactly as in `ir_filter_metadata()`. Like the
  metadata operations it works on both `ir_aggregated_data` (from
  `ir_aggregate_isofiles()`) and `ir_isofiles` (from `ir_read_isofiles()`)
  objects. The selection is resolved once against the whole object, so a mass
  that only some of the files or datasets contain is fine - the others simply
  end up without data. `resistors` are not filtered by mass since they describe
  the instrument configuration rather than measured data.

* Fixed the metadata operations (`ir_filter_metadata()`, `ir_mutate_metadata()`,
  `ir_join_metadata()`) silently skipping the `traces`, `cycles`, and `scans` of
  an `ir_isofiles` collection that **mixes measurement types**. Since a dataset
  only some of the file types have is `NULL` in the other rows (e.g. `traces`
  for a dual inlet file), such a column was not recognized as a nested dataset
  at all, so `ir_filter_metadata()` did not cascade its filter into it. Reading
  a single measurement type at a time was unaffected.

* Fixed deprecation warnings from using the `.data` pronoun in tidyselect
  expressions (e.g. in `ir_read_isofiles()`).

* Fixed `mass`/`ratio` selections that mean "none" only being recognized when
  written as a literal `NULL`. `c()`, and any expression evaluating to `NULL`
  (e.g. a variable holding it, or an argument forwarded on with `{{ }}`), now
  behave identically: they select nothing, no longer error with "calculate
  ratios first" when the `ratio_name`/`ratio` columns are absent, and no longer
  trigger tidyselect's "external vector in selections" deprecation warning.

* Fixed the plotting functions and `ir_generate_*_tibble()` erroring with
  "Can't combine `..1$trace` \<character\> and `..2$trace` \<logical\>" on data
  whose `ratio_name`/`ratio` columns are present but hold no ratios at all - for
  example after `ir_calculate_ratios()` on a species with only a single mass,
  where the one row is the base mass and its `ratio_name` is `NA`.

* Upgraded to `isoextract` version 0.3.2 to enable successful reading of files generated with Isodat systems that use a GC II-III Interface


# isoreader2 0.6.1

* Addressed CRAN feedback on the initial submission:

  * `ir_export_to_excel()`, `ir_save_aggregated_data()`, and
    `ir_load_aggregated_data()` no longer try to install their suggested
    packages (`openxlsx` and `arrow`) automatically. They now fail with an
    informative error telling you to run `install.packages()` yourself.

  * The examples for `ir_copy_examples()` and `ir_export_to_excel()` are now
    runnable (writing to `tempdir()`) instead of wrapped in `\dontrun{}`.

  * The package description now cites Kopf et al. (2021)
    <doi:10.21105/joss.02878>.

* Added a full `LICENSE.md` for the AGPL (>= 3) license.

* `ir_calculate_ratios()` now reports the parameters it used in its info
  message: any non-zero additive offsets (for traces and scans) and the
  normalization function, if one was given (#3).

* Fixed the aggregated data print output listing each mass (or channel) once per
  cycle and per standard/sample instead of once in total (#2).

# isoreader2 0.6.0

* Initial CRAN release.

* Reads stable isotope data files from many vendor formats: Isodat
  (`.dxf`, `.cf`, `.did`, `.caf`, `.scn`), IonOS (`.iarc`), LyticOS (`.larc`),
  Callisto (`.bch`), and Qtegra (`.imexp`), via the external `isoextract`
  helper (`ir_read_isofiles()`, `ir_extract_isofiles()`).

* Provides a consistent object model (`ir_isofiles`, `ir_aggregated_data`) with
  tools to aggregate (`ir_aggregate_isofiles()`), convert signal units
  (`ir_convert_intensity()`), calculate ratios (`ir_calculate_ratios()`),
  filter and extend metadata, and access the extracted data
  (`ir_get_*()`).

* Visualizes continuous flow, dual inlet, and scan data
  (`ir_plot_continuous_flow()`, `ir_plot_dual_inlet()`, `ir_plot_scans()`), and
  builds the underlying plotting tibbles directly (`ir_generate_*_tibble()`).

* Saves/loads data and exports to Excel (`ir_save_isofiles()`,
  `ir_load_isofiles()`, `ir_export_to_excel()`).
