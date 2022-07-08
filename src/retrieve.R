library(jsonlite)
library(sf)
library(catchment)
library(osrm)

dir.create("docs", FALSE)
dir.create("data/original", FALSE)
dir.create("data/original/shapes", FALSE)

year <- 2020
states <- c("DC", "DE", "KY", "MD", "NC", "NJ", "PA", "TN", "VA", "WV")

# define health districts and focal counties
va_id_map <- read_json("https://uva-bi-sdad.github.io/community/dist/shapes/VA/virginia_2010.json")
districts <- c(
  unlist(lapply(va_id_map$county, "[[", "district")),
  "11001" = "11_hd_01", "24017" = "24_hd_01", "24021" = "24_hd_01",
  "24031" = "24_hd_01", "24033" = "24_hd_01"
)

#
# providers
#

# download and/or load data
# https://hifld-geoplatform.opendata.arcgis.com/datasets/geoplatform::hospitals/about
hospitals_file <- "data/working/hospitals.csv.xz"
if (file.exists(hospitals_file)) {
  hospitals <- read.csv(gzfile(hospitals_file))
} else {
  hospitals_original_file <- "data/original/hospitals.geojson"
  if (!file.exists(hospitals_original_file)) {
    download.file(
      "https://opendata.arcgis.com/api/v3/datasets/6ac5e325468c4cb9b905f1728d6fbf0f_0/downloads/data?format=geojson&spatialRefId=4326",
      hospitals_original_file
    )
  }
  hospitals <- st_drop_geometry(st_read(hospitals_original_file))

  # inclusion criteria
  hospitals <- hospitals[
    hospitals$STATE %in% states &
      hospitals$TYPE %in% c("CRITICAL ACCESS", "GENERAL ACUTE CARE") &
      hospitals$STATUS == "OPEN",
  ]

  # bed imputation
  hospitals[hospitals$BEDS == -999, "BEDS"] <- median(hospitals[hospitals$BEDS != -999, "BEDS"])

  hospitals$VAL_DATE <- substring(hospitals$VAL_DATE, 1, 10)
  colnames(hospitals)[colnames(hospitals) == "LATITUDE"] <- "Y"
  colnames(hospitals)[colnames(hospitals) == "LONGITUDE"] <- "X"
  write.csv(hospitals, hospitals_file, row.names = FALSE)
}

# write point layer
point_file <- paste0("docs/points_", year, ".geojson")
if (!file.exists(point_file)) {
  write_sf(st_as_sf(data.frame(
    hospitals[, c("ID", "NAME", "TYPE", "SOURCE", "VAL_METHOD", "VAL_DATE", "BEDS", "TRAUMA", "HELIPAD")],
    X = round(hospitals$X, 6),
    Y = round(hospitals$Y, 6)
  ), coords = c("X", "Y")), point_file)
}

#
# consumers
#

# get population data
pop_file <- paste0("data/original/population", year)
dir.create(pop_file, FALSE)
pop_bg_file <- "data/working/population_bg.csv.xz"
if (file.exists(pop_bg_file)) {
  population <- read.csv(gzfile(pop_bg_file))
} else {
  hospitals_geo <- st_as_sf(hospitals, coords = c("X", "Y"), crs = "NAD83")
  population <- do.call(rbind, lapply(states, function(s) {
    pop <- download_census_population(pop_file, s, year)$estimates
    pop$GEOID <- as.character(pop$GEOID)
    rownames(pop) <- pop$GEOID
    shapes <- download_census_shapes("data/original/shapes", s, "bg", year = year)
    shapes <- rmapshaper::ms_simplify(shapes, keep_shapes = TRUE)
    rownames(shapes) <- shapes$GEOID
    d <- pop[shapes$GEOID, ]
    d[is.na(d)] <- 0
    total <- d$TOTAL.POPULATION_Total
    total[total == 0] <- 1
    data.frame(
      GEOID = shapes$GEOID,
      state = s,
      hospitals = rowSums(st_contains(shapes, hospitals_geo, sparse = FALSE)),
      population = d$TOTAL.POPULATION_Total,
      percent_female = d$SEX.BY.AGE_Female_Female / total * 100,
      percent_white = d$RACE_Total_White.alone / total * 100,
      percent_over_49 = rowSums(d[, grep("[5-8][05]", colnames(d))]) / total * 100,
      st_coordinates(st_centroid(shapes))
    )
  }))
  write.csv(population, xzfile(pop_bg_file), row.names = FALSE)
}

# calculate tract-level population data for comparison
pop_tr_file <- "data/working/population_tr.csv.xz"
if (file.exists(pop_tr_file)) {
  population_tracts <- read.csv(gzfile(pop_tr_file))
} else {
  population_tracts <- do.call(rbind, lapply(states, function(s) {
    shapes <- download_census_shapes("data/original/shapes", s, "tr", year = year)
    rownames(shapes) <- shapes$GEOID
    pop <- population[population$state == s, ]
    do.call(rbind, lapply(split(pop, substring(pop$GEOID, 1, 11)), function(d) {
      geoid <- substring(d[1, "GEOID"], 1, 11)
      if (nrow(d) == 1) {
        d$GEOID <- geoid
        d
      } else {
        data.frame(
          GEOID = geoid,
          state = d[1, "state"],
          hospitals = sum(d$hospitals),
          population = sum(d$population),
          t(colMeans(d[, c("percent_female", "percent_white", "percent_over_49")])),
          st_coordinates(st_centroid(shapes[shapes$GEOID == geoid, ]))
        )
      }
    }))
  }))
  write.csv(population_tracts, xzfile(pop_tr_file), row.names = FALSE)
}

#
# cost
#

# calculate travel times
## break up output by state to reduce size
states_pref <- unique(substring(population$GEOID, 1, 2))
cost_files <- paste0("data/working/traveltimes_", year, "_", states_pref, ".csv.xz")
if (all(file.exists(cost_files))) {
  traveltimes <- do.call(rbind, lapply(
    cost_files,
    function(f) read.csv(gzfile(f), row.names = 1, check.names = FALSE)
  ))
} else {
  options(osrm.server = Sys.getenv("OSRM_SERVER"))
  traveltimes <- osrmTable(
    src = population[, c("GEOID", "X", "Y")],
    dst = hospitals[, c("ID", "X", "Y")]
  )$duration
  if (is.null(traveltimes)) stop("failed to calculate travel times")
  for (i in seq_along(states_pref)) {
    st <- traveltimes[startsWith(rownames(traveltimes), states_pref[i]), ]
    write.csv(
      cbind(GEOID = rownames(st), as.data.frame(as.matrix(st))),
      xzfile(cost_files[i]),
      row.names = FALSE
    )
  }
}

## tract-level travel times
tract_cost_files <- paste0("data/working/traveltimes_", year, "_", states_pref, "_tract.csv.xz")
if (all(file.exists(tract_cost_files))) {
  traveltimes_tract <- do.call(rbind, lapply(
    cost_files,
    function(f) read.csv(gzfile(f), row.names = 1, check.names = FALSE)
  ))
} else {
  options(osrm.server = Sys.getenv("OSRM_SERVER"))
  traveltimes_tract <- osrmTable(
    src = population_tracts[, c("GEOID", "X", "Y")],
    dst = hospitals[, c("ID", "X", "Y")]
  )$duration
  if (is.null(traveltimes_tract)) stop("failed to calculate travel times")
  for (i in seq_along(states_pref)) {
    st <- traveltimes_tract[startsWith(rownames(traveltimes_tract), states_pref[i]), ]
    write.csv(
      cbind(GEOID = rownames(st), as.data.frame(as.matrix(st))),
      xzfile(tract_cost_files[i]),
      row.names = FALSE
    )
  }
}

#
# base results
#

# get minimum travel times
# ensure populations and travel times align
all(rownames(traveltimes) == population$GEOID)
population$hospitals_min_drivetime <- apply(
  traveltimes, 1, min,
  na.rm = TRUE
)

# calculate catchment ratios
population$hospital_beds_per_100k <- catchment_ratio(
  population, hospitals, traveltimes,
  weight = "gaussian", scale = 18, normalize_weight = TRUE, return_type = 1e5,
  consumers_value = "population", providers_id = "ID", providers_value = "BEDS"
)

#
# output
#

# aggregate
agger <- function(d, part = NULL) {
  total <- sum(d$population, na.rm = TRUE)
  total[total == 0] <- 1
  as.data.frame(c(
    GEOID = if (missing(part)) districts[[substring(d[1, "GEOID"], 1, 5)]] else substring(d[1, "GEOID"], 1, part),
    as.list(c(
      hospitals = sum(d$hospitals),
      hospitals_min_drivetime = mean(d$hospitals_min_drivetime),
      hospital_beds_per_100k = sum(d$hospital_beds_per_100k * d$population) / total
    ))
  ))
}
block_groups <- population[substring(population$GEOID, 1, 5) %in% names(districts), ]
data <- list(
  block_groups = block_groups,
  tracts = do.call(rbind, lapply(split(block_groups, substring(block_groups$GEOID, 1, 11)), agger, 11)),
  counties = do.call(rbind, lapply(split(block_groups, substring(block_groups$GEOID, 1, 5)), agger, 5)),
  districts = do.call(rbind, lapply(split(block_groups, districts[substring(block_groups$GEOID, 1, 5)]), agger))
)

# download and/or load maps
dir.create("data/original/reference_shapes", FALSE)
entity_info <- lapply(va_id_map$district, function(e) list(region_name = e$name))
for (location in c("dc", "md", "va")) {
  for (level in c("census_block_groups", "census_tracts", "counties")) {
    name <- paste0(location, "_geo_census_cb_", year, "_", level)
    file <- paste0(
      "data/original/reference_shapes/", location, "_",
      sub("census_", "", level, fixed = TRUE), "_", year, ".geojson"
    )
    if (!file.exists(file)) {
      tryCatch(download.file(paste0(
        "https://raw.githubusercontent.com/uva-bi-sdad/dc.geographies/main/data/",
        name, "/distribution/", name, ".geojson"
      ), file), error = function(e) NULL)
    }
    if (file.exists(file)) {
      d <- read_json(file)
      for (e in d$features) entity_info[[e$properties$geoid]] <- e$properties
    }
  }
}
entity_names <- unlist(lapply(entity_info, "[[", "region_name"))

# reformat and save
final <- (function(d = data) {
  d$block_groups <- d$block_groups[, colnames(d$districts)]
  d <- do.call(rbind, d)
  varnames <- colnames(d)[-1]
  d$year <- year
  d$region_type <- c(
    "5" = "county", "8" = "health district", "11" = "tract", "12" = "block group"
  )[as.character(nchar(d$GEOID))]
  rownames(d) <- d$GEOID
  d$region_name <- d$GEOID
  present_ids <- d$GEOID[d$GEOID %in% names(entity_names)]
  d[present_ids, "region_name"] <- entity_names[present_ids]
  do.call(rbind, lapply(split(d, seq_len(nrow(d))), function(r) {
    data.frame(
      geoid = r$GEOID,
      region_type = r$region_type,
      region_name = r$region_name,
      year = year,
      measure = varnames,
      value = as.numeric(r[varnames]),
      measure_type = ifelse(grepl("drivetime", varnames, fixed = TRUE), "minutes", "per 100k")
    )
  }))
})()
write.csv(final, xzfile("data/distribution/hifld.csv.xz"), row.names = FALSE)

# make combined block group map
if (!file.exists("docs/map_2020.geojson")) {
  st_write(
    rmapshaper::ms_simplify(st_read(toJSON(list(
      type = "FeatureCollection",
      crs = list(type = "name", properties = list(name = "urn:ogc:def:crs:OGC:1.3:CRS84")),
      features = unlist(lapply(
        list.files("data/original/reference_shapes", "block_groups_2020", full.names = TRUE),
        function(f) Filter(function(e) e$properties$geoid %in% final$geoid, read_json(f)$features)
      ), FALSE, FALSE)
    ), auto_unbox = TRUE), quiet = TRUE), keep_shapes = TRUE),
    "docs/map_2020.geojson"
  )
}
