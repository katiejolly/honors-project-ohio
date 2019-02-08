library(tidycensus)
library(tidyverse)
library(tigris)
library(sf)

oh_counties <- counties(state = "oh") %>%
  st_as_sf()

county_names <- oh_counties$COUNTYFP

totalpop_sf_tracts <- reduce(
  map(county_names, function(x) {
    get_acs(year = 2017, table = "B01003", state = "oh", geography = "tract", county = x)
  }), 
  bind_rows
)

totalpop <- tibble(GEOID = totalpop_sf_tracts$GEOID,
                   pop17 = totalpop_sf_tracts$estimate,
                   moe = totalpop_sf_tracts$moe)

tracts_oh <- tracts(state = "oh", year = 2017) %>%
  st_as_sf()

join <- left_join(tracts_oh %>% select(GEOID), totalpop) %>%
  st_transform(26917)




st_write(join, "pop_approx/spatial-ops/totalpop_2017_tracts.gpkg")

blocks <- blocks(state = "mn")

blocks_sf <- blocks %>%
  st_as_sf() %>%
  st_transform(26915)

blocks_join <- totalpop_sf %>%
  left_join(shp, by = c("GEOID" = "GEOID10"))

View(totalpop_sf)

shp <- st_read("mn/shp_vtd/vtd_prorated.shp") %>%
  st_transform(26915)

join <- st_join(totalpop_sf, shp)

GEOID <- rep(NA, 4139)
POP <- rep(NA, 4139)
CD <- rep(NA, 4139)

vtd_df <- data_frame(GEOID = GEOID,
                     POP = POP,
                     CD = CD)


for (i in 1:length(vtd)){
  vtd_df$GEOID[[i]] <- vtd[[i]]$id
  vtd_df$POP[[i]] <- vtd[[i]]$POP10
  vtd_df$CD[[i]] <- vtd[[i]]$CD113
}


vtd_join <- vtd_df %>%
  left_join(shp, by = c("GEOID" = "GEOID10")) %>%
  st_as_sf() 

st_write(vtd_join, "mn/shp_vtd/vtd_katie.shp")
