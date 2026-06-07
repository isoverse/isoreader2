# ir_start_aggregator() / cli [plain]

    Code
      ir_start_aggregator("test")
    Message
      -------------------------------- Aggregator test -------------------------------

# ir_start_aggregator() / cli [fancy]

    Code
      ir_start_aggregator("test")
    Message
      ──────────────────────────────── [1mAggregator [3mtest[23m[22m ───────────────────────────────

# ir_add_to_aggregator() / cli [plain]

    Code
      ir_add_to_aggregator(ir_add_to_aggregator(ir_add_to_aggregator(
        ir_add_to_aggregator(ir_add_to_aggregator(ir_start_aggregator("test"),
        "metadata", "col"), "metadata", "num", cast = "as.integer"), "metadata",
        "new", source = c("def", "alt def"), default = 4), "metadata", "w\\1_\\2",
      "(\\d+)-(.*)", regexp = TRUE), "metadata", "from_fun", cast = "as.integer",
      source = list(c("a", "b"), "x"), func = "mean")
    Message
      -------------------------------- Aggregator test -------------------------------
      Dataset metadata:
       > col = as.character(col)
       > num = as.integer(num)
       > new = as.character(one_of(def, `alt def`)) - if source is missing: new =
      as.character(4)
       > w(\\d+)_(.*) = as.character(all_matches("(\\d+)-(.*)"))
       > from_fun = as.integer(mean(one_of(a, b), x))

# ir_add_to_aggregator() / cli [fancy]

    Code
      ir_add_to_aggregator(ir_add_to_aggregator(ir_add_to_aggregator(
        ir_add_to_aggregator(ir_add_to_aggregator(ir_start_aggregator("test"),
        "metadata", "col"), "metadata", "num", cast = "as.integer"), "metadata",
        "new", source = c("def", "alt def"), default = 4), "metadata", "w\\1_\\2",
      "(\\d+)-(.*)", regexp = TRUE), "metadata", "from_fun", cast = "as.integer",
      source = list(c("a", "b"), "x"), func = "mean")
    Message
      ──────────────────────────────── [1mAggregator [3mtest[23m[22m ───────────────────────────────
      [1mDataset[22m [34mmetadata[39m:
       → [32mcol[39m = [3mas.character(col)[23m
       → [32mnum[39m = [3mas.integer(num)[23m
       → [32mnew[39m = [3mas.character(one_of(def, `alt def`))[23m - [33mif source is missing[39m: [32mnew[39m =
      [3mas.character(4)[23m
       → [35mw(\\d+)_(.*)[39m = [3mas.character(all_matches("(\\d+)-(.*)"))[23m
       → [32mfrom_fun[39m = [3mas.integer(mean(one_of(a, b), x))[23m

