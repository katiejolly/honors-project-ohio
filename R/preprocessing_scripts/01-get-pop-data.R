library(tidycensus)
library(tidyverse)
library(tigris)
library(sf)


v10 <- load_variables(2010, "sf1", cache = TRUE)

oh_counties <- counties(state = "oh") 

county_names <- oh_counties$COUNTYFP

totalpop_sf <- reduce(
  map(county_names, function(x) {
    get_decennial(year = 2010, variables = "P0010001", state = "OH", geography = "block", county = x)
  }), 
  bind_rows
)

write_csv(totalpop_sf, "R/data/totalpop_2010_blocks.csv")

blocks_oh_sf <- blocks(state = "ohio", year = 2010)

blocks_oh_sf <- blocks_oh_sf %>%
  st_as_sf() %>%
  left_join(totalpop_sf, by = c("GEOID10" = "GEOID")) %>%
  rename(TOTAL_POP = value)

st_write(blocks_oh_sf, "R/data/shp/ohio_block_pop.gpkg")
