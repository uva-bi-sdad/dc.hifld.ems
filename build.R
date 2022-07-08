library(community)

data_reformat_sdad("data/distribution/hifld.csv.xz", "docs/data")

data_add(
  c(block_group = "block_group.csv.xz"),
  meta = list(
    ids = list(variable = "ID", map = "data/entity_info.json"),
    time = "time",
    variables = "data/distribution/measure_info.json"
  ),
  dir = "docs/data",
  refresh = TRUE
)

site_build(".", serve = TRUE, include_api = FALSE, options = list(
  polygon_outline = .5, color_scale_center = "median", summary_selection = "dataset"
))
