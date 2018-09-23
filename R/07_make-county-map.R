library(tigris)
library(sf)
library(tidyverse)

ohio_counties <- st_read("R/data/shp/tl_2016_ohio_cnty/tl_2016_39_cousub.shp") %>%
  st_transform(26917)

st_crs(ohio_counties)

ggplot(ohio_counties) +
  geom_sf(fill = "#F8D9CD", color = "#595959") +
  theme_minimal() +
  theme(panel.grid.major = element_line(color = "transparent"),
        axis.text = element_blank()) 

ggsave("R/plots/counties.png")  
