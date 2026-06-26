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
