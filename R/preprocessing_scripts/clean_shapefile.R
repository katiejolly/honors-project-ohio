library(tidyverse)
library(sf)
library(cleangeo)

precincts_results <- st_read("shp/precincts_results.shp")

validity_reason <- st_is_valid(precincts_results, reason = TRUE)

unique(validity_reason)

precincts_valid <- precincts_results %>%
  lwgeom::st_make_valid() %>%
  filter(!is.na(st_dimension(.)))

unique(st_geometry_type(precincts_valid))

sum(st_is_valid(precincts_valid) == FALSE)


precincts_collection <- precincts_valid %>%
  st_cast("GEOMETRYCOLLECTION")

st_write(precincts_collection, "shp/precincts_results_val.gpkg")

