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

st_write(obj = precincts_collection, dsn = "shp/precincts_results_val.gpkg")

ggplot(precincts_collection) +
  geom_sf()

collections <- precincts_valid %>% filter(st_geometry_type(.) == "GEOMETRYCOLLECTION")

gpkg_test <- st_read("shp/precincts_results_val.gpkg")

st_write(obj = precincts_no_collections, "shp/precincts_multipolygon.gpkg")


ggplot(gpkg_test %>% rename(geometry = geom)) +
  geom_sf()

precincts_no_collections <- anti_join(precincts_valid, collections %>% st_set_geometry(NULL)) %>%
  st_cast("MULTIPOLYGON")
