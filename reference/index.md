# Package index

## Read isofiles

These functions allows reading of stable isotope data files/archives

- [`ir_examples_folder()`](https://isoreader2.isoverse.org/reference/ir_examples_folder.md)
  : Path to the bundled example files
- [`ir_find_isofiles()`](https://isoreader2.isoverse.org/reference/ir_find_isofiles.md)
  [`ir_find_continuous_flow()`](https://isoreader2.isoverse.org/reference/ir_find_isofiles.md)
  [`ir_find_dual_inlet()`](https://isoreader2.isoverse.org/reference/ir_find_isofiles.md)
  [`ir_find_scans()`](https://isoreader2.isoverse.org/reference/ir_find_isofiles.md)
  : Find isodat files
- [`ir_read_isofiles()`](https://isoreader2.isoverse.org/reference/ir_read_isofiles.md)
  : Read isotope data files
- [`c(`*`<ir_isofiles>`*`)`](https://isoreader2.isoverse.org/reference/c.ir_isofiles.md)
  : Combine isofiles
- [`ir_check_isoextract()`](https://isoreader2.isoverse.org/reference/ir_check_isoextract.md)
  : Check for the isoextract executable
- [`ir_extract_isofiles()`](https://isoreader2.isoverse.org/reference/ir_extract_isofiles.md)
  : run the isoextract executable on a vector of file paths this is
  usually not called directly
- [`ir_get_supported_file_types()`](https://isoreader2.isoverse.org/reference/ir_get_supported_file_types.md)
  : Get supported file types
- [`ir_get_problems()`](https://isoreader2.isoverse.org/reference/problems.md)
  [`ir_show_problems()`](https://isoreader2.isoverse.org/reference/problems.md)
  : Retrieve parsing problems

## Aggregate isofiles data

These functions combine the data from multiple isofiles for combined use

- [`ir_aggregate_isofiles()`](https://isoreader2.isoverse.org/reference/ir_aggregate_isofiles.md)
  : Aggregate data from isofiles
- [`ir_convert_intensity()`](https://isoreader2.isoverse.org/reference/ir_convert_intensity.md)
  : Convert intensity between units
- [`ir_start_aggregator()`](https://isoreader2.isoverse.org/reference/ir_aggregator.md)
  [`ir_add_to_aggregator()`](https://isoreader2.isoverse.org/reference/ir_aggregator.md)
  [`ir_register_aggregator()`](https://isoreader2.isoverse.org/reference/ir_aggregator.md)
  [`ir_get_aggregator()`](https://isoreader2.isoverse.org/reference/ir_aggregator.md)
  : Dynamic data agreggator
- [`ir_get_data()`](https://isoreader2.isoverse.org/reference/ir_get_data.md)
  [`ir_get_metadata()`](https://isoreader2.isoverse.org/reference/ir_get_data.md)
  [`ir_get_resistors()`](https://isoreader2.isoverse.org/reference/ir_get_data.md)
  [`ir_get_traces()`](https://isoreader2.isoverse.org/reference/ir_get_data.md)
  [`ir_get_cycles()`](https://isoreader2.isoverse.org/reference/ir_get_data.md)
  [`ir_get_scans()`](https://isoreader2.isoverse.org/reference/ir_get_data.md)
  : Get data frame from aggregated data

## Metadata operations

These functions filter, modify, or extend the metadata of aggregated
isofiles.

- [`ir_filter_metadata()`](https://isoreader2.isoverse.org/reference/ir_metadata.md)
  [`ir_mutate_metadata()`](https://isoreader2.isoverse.org/reference/ir_metadata.md)
  [`ir_join_metadata()`](https://isoreader2.isoverse.org/reference/ir_metadata.md)
  : Filter, mutate, or join the metadata of isofiles

## Visualize isofiles data

These functions plot aggregated isofiles data.

- [`ir_default_theme()`](https://isoreader2.isoverse.org/reference/ir_default_theme.md)
  : Default isoreader2 plotting theme
- [`ir_plot_scans()`](https://isoreader2.isoverse.org/reference/ir_plot_scans.md)
  : Plot scan data
- [`ir_plot_continuous_flow()`](https://isoreader2.isoverse.org/reference/ir_plot_continuous_flow.md)
  : Plot continuous flow data
- [`ir_plot_dual_inlet()`](https://isoreader2.isoverse.org/reference/ir_plot_dual_inlet.md)
  : Plot dual inlet cycle data

## Export & storage

These functions save aggregated isofiles data to disk.

- [`ir_export_to_excel()`](https://isoreader2.isoverse.org/reference/ir_export_to_excel.md)
  : Export data to Excel
- [`ir_save_aggregated_data()`](https://isoreader2.isoverse.org/reference/ir_storage.md)
  [`ir_load_aggregated_data()`](https://isoreader2.isoverse.org/reference/ir_storage.md)
  : Save and load aggregated isofile data
- [`ir_save_isofiles()`](https://isoreader2.isoverse.org/reference/ir_isofiles_storage.md)
  [`ir_load_isofiles()`](https://isoreader2.isoverse.org/reference/ir_isofiles_storage.md)
  : Save and load isofiles

## Package options

These functions are available to simplify or customize the data
analysis.

- [`isoreader2`](https://isoreader2.isoverse.org/reference/isoreader2-package.md)
  [`isoreader2-package`](https://isoreader2.isoverse.org/reference/isoreader2-package.md)
  : isoreader2: Read Stable Isotope Data Files
- [`ir_options()`](https://isoreader2.isoverse.org/reference/ir_options.md)
  [`ir_get_options()`](https://isoreader2.isoverse.org/reference/ir_options.md)
  [`ir_get_option()`](https://isoreader2.isoverse.org/reference/ir_options.md)
  : Package options
