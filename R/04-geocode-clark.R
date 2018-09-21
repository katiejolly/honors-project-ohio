library(tidyverse)
library(tigris)

clark <- read_csv("https://www6.sos.state.oh.us/ords/f?p=VOTERFTP:DOWNLOAD::FILE:NO:2:P2_PRODUCT_NUMBER:12")

for (i in 1:nrow(clark)) {
  clark$GEOID10[i] <- tigris::call_geolocator(clark[['RESIDENTIAL_ADDRESS1']][i], clark[['RESIDENTIAL_CITY']][i], clark[['RESIDENTIAL_STATE']][i])
}

clark_1 <- clark[c(1:66312),]

clark_2 <- anti_join(clark, clark_1)

for (i in 1:nrow(clark_2)) {
  clark_2$GEOID10[i] <- tigris::call_geolocator(clark_2[['RESIDENTIAL_ADDRESS1']][i], clark_2[['RESIDENTIAL_CITY']][i], clark_2[['RESIDENTIAL_STATE']][i])
}