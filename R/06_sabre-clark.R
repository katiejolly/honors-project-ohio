library(tidyverse)
library(sabre)
library(sf)

clark_approx <- st_read("R/data/shp/approx/clark_valid_approx.shp") %>%
  st_transform(26917)

st_crs(clark_approx)

clark_base <- st_read("R/data/shp/base/ClarkCounty_VotingPrecinct.shp") %>%
  st_transform(26917)

########

clark_vmeasure <- vmeasure_calc(clark_base, PRECINCT, clark_approx, PRECINCT)

clark_vmeasure

base_vm <- clark_vmeasure$map1

mapview::mapview(base_vm, zcol = "rih", legend = TRUE) # view the map!!


approx_vm <- clark_vmeasure$map2

mapview::mapview(approx_vm, zcol = "rih", legend = TRUE) # view the map!!


########### mapcurves method

clark_mapcurves <- mapcurves_calc(clark_base, PRECINCT, clark_approx, PRECINCT)

clark_mapcurves # goodness of fit = 0.88


########## measure area difference

clark_base_area <- clark_base %>%
  mutate(area_base = st_area(.)) %>%
  st_set_geometry(NULL) %>%
  arrange(desc(PRECINCT))

clark_approx_area <- clark_approx %>%
  mutate(area_approx = st_area(.)) %>%
  st_set_geometry(NULL) %>%
  arrange(desc(PRECINCT))

clark_join <- clark_approx_area %>%
  bind_cols(clark_base_area)

clark_join <- clark_join %>%
  mutate(diff = as.numeric(abs(area_base - area_approx)))

(sum(clark_join$diff)/2) / sum(clark_join$area_base) # divide by 2 to avoid double counting 

ggplot() +
  geom_sf(data = clark_base, fill = NA, color = "#fbb4ae", alpha = 1) +
  geom_sf(data = clark_approx, fill = NA, color = "#ccebc5", alpha = 1) +
  theme_minimal() +
  theme(panel.grid.major = element_line(color = "transparent"),
        axis.text = element_blank()) 
