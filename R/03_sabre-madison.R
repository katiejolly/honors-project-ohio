library(tidyverse)
library(sf)
library(sabre)

# load the data

madison_valid <- st_read("R/data/shp/approx/madison_valid_approx.shp")

madison_base <- st_read("R/data/shp/base/Madison_Precincts.shp")


####################

# sabre calculations

st_crs(madison_base) == st_crs(madison_valid) # make sure the shapefiles have the same crs

# v-measure

precincts_vm <- vmeasure_calc(madison_base, PRECINCT, madison_valid, PRECINCT)

precincts_vm 

# mapcurve measurement

precincts_mapcurve <- mapcurves_calc(madison_base, PRECINCT, madison_valid, PRECINCT)

precincts_mapcurve

# plot the v-measure results 

par(mar = rep(2, 4))
plot(precincts_vm$map1["rih"], main = "Actual precincts: rih")

par(mar = rep(2, 4))
plot(precincts_vm$map2["rih"], main = "Approximated precincts: rih")
