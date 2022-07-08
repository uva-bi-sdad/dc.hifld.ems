page_menu(
  default_open = TRUE,
  input_select("Variable", options = "variables", default = 1, id = "selected_variable"),
  input_slider("Outline Width", min = 0, max = 5, default = .5, step = .5, id = "settings.polygon_outline")
)

output_info("variables.short_name", variable = "selected_variable", variable_info = TRUE)

output_info(
  title = "features.id",
  body = c("variables.short_name" = "value"),
  row_style = "stack",
  subto = "main_map",
  variable_info = FALSE, floating = TRUE
)

output_legend(variable = "selected_variable", id = "main_legend", subto = "main_map", show_na = FALSE)

output_map(
  list(name = "block_group", url = "docs/map_2020.geojson", id_property = "geoid"),
  lapply(paste0("hifld:hospital", c("s", "s_min_drivetime", "_beds_per_100k")), function(v) {
    list(
      variable = v, source = list(list(url = "points_2020.geojson"))
    )
  }),
  color = "selected_variable",
  id = "main_map",
  subto = "main_legend",
  options = list(
    attributionControl = FALSE,
    height = "1000px",
    center = c(38.14751758025121, -79.43389892578126),
    zoom = 8
  ),
  tiles = list(
    light = list(url = "https://stamen-tiles-{s}.a.ssl.fastly.net/toner-lite/{z}/{x}/{y}{r}.png"),
    dark = list(url = "https://{s}.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}{r}.png")
  )
)
