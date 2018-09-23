library(tidyverse)
library(sabre)
library(sf)

clark_approx <- st_read("R/data/shp/approx/clark_valid_approx.shp") %>%
  st_transform(4326)

st_crs(clark_approx)

clark_base <- st_read("R/data/shp/base/ClarkCounty_VotingPrecinct.shp") %>%
  st_transform(4326)

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
