# Changelog

## isoreader2 0.6.0

- Initial CRAN release.

- Reads stable isotope data files from many vendor formats: Isodat
  (`.dxf`, `.cf`, `.did`, `.caf`, `.scn`), IonOS (`.iarc`), LyticOS
  (`.larc`), Callisto (`.bch`), and Qtegra (`.imexp`), via the external
  `isoextract` helper
  ([`ir_read_isofiles()`](https://isoreader2.isoverse.org/reference/ir_read_isofiles.md),
  [`ir_extract_isofiles()`](https://isoreader2.isoverse.org/reference/ir_extract_isofiles.md)).

- Provides a consistent object model (`ir_isofiles`,
  `ir_aggregated_data`) with tools to aggregate
  ([`ir_aggregate_isofiles()`](https://isoreader2.isoverse.org/reference/ir_aggregate_isofiles.md)),
  convert signal units
  ([`ir_convert_intensity()`](https://isoreader2.isoverse.org/reference/ir_convert_intensity.md)),
  calculate ratios
  ([`ir_calculate_ratios()`](https://isoreader2.isoverse.org/reference/ir_calculate_ratios.md)),
  filter and extend metadata, and access the extracted data
  (`ir_get_*()`).

- Visualizes continuous flow, dual inlet, and scan data
  ([`ir_plot_continuous_flow()`](https://isoreader2.isoverse.org/reference/ir_plot_traces.md),
  [`ir_plot_dual_inlet()`](https://isoreader2.isoverse.org/reference/ir_plot_dual_inlet.md),
  [`ir_plot_scans()`](https://isoreader2.isoverse.org/reference/ir_plot_scans.md)),
  and builds the underlying plotting tibbles directly
  (`ir_generate_*_tibble()`).

- Saves/loads data and exports to Excel
  ([`ir_save_isofiles()`](https://isoreader2.isoverse.org/reference/ir_isofiles_storage.md),
  [`ir_load_isofiles()`](https://isoreader2.isoverse.org/reference/ir_isofiles_storage.md),
  [`ir_export_to_excel()`](https://isoreader2.isoverse.org/reference/ir_export_to_excel.md)).
