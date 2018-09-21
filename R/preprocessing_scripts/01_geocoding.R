library(tidyverse)
library(tigris)
library(sabre)

# get geoid data for Madison County, Ohio

blocks <- blocks(state = "OH", county = "madison")

madison <- read_csv("https://www6.sos.state.oh.us/ords/f?p=VOTERFTP:DOWNLOAD::FILE:NO:2:P2_PRODUCT_NUMBER:49")

madison$GEOID10 <- NA

vec <- purrr::map_chr(1:nrow(madison), function(i) tigris::call_geolocator(madison[['RESIDENTIAL_ADDRESS1']][i], madison[['RESIDENTIAL_CITY']][i], madison[['RESIDENTIAL_STATE']][i]))


for (i in 1:nrow(madison)) {
  madison$GEOID10[i] <- tigris::call_geolocator(madison[['RESIDENTIAL_ADDRESS1']][i], madison[['RESIDENTIAL_CITY']][i], madison[['RESIDENTIAL_STATE']][i])
}

madison_1 <- madison[c(1:5541),]

madison_2 <- madison %>% anti_join(madison_1)

for (i in 1:nrow(madison_2)) {
  madison_2$GEOID10[i] <- tigris::call_geolocator(madison_2[['RESIDENTIAL_ADDRESS1']][i], madison_2[['RESIDENTIAL_CITY']][i], madison_2[['RESIDENTIAL_STATE']][i])
}

madison_3 <- madison_2[c(18829:nrow(madison_2)),]

madison_2_done <- madison_2[c(1:18829),]

madison_progress <- bind_rows(madison_1, madison_2_done)

for (i in 1:nrow(madison_3)) {
  madison_3$GEOID10[i] <- tigris::call_geolocator(madison_3[['RESIDENTIAL_ADDRESS1']][i], madison_3[['RESIDENTIAL_CITY']][i], madison_3[['RESIDENTIAL_STATE']][i])
}

madison_done <- bind_rows(madison_progress, madison_3)

write_csv(madison_done, "R/data/madison_done.csv")

