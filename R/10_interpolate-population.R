library(sf)
library(tidyverse)

options(scipen=999)

precincts <- st_read("pop_approx/spatial-ops/precincts_results_valid.gpkg") %>%
  st_transform(26917)

precincts <- st_buffer(precincts, 0.0)

table(st_is_valid(precincts))

tracts <- st_read("pop_approx/spatial-ops/totalpop_2017_tracts.gpkg") %>%
  st_transform(26917)

int <- st_interpolate_aw(tracts["pop17"], precincts, extensive = TRUE)


precincts_noble <- precincts %>%
  filter(COUNTY == "noble")

tracts_noble <- tracts %>%
  filter(stringr::str_detect(as.character(GEOID), "39121")) %>%
  mutate(total_area = st_area(.))

ggplot(tracts_noble) +
  geom_sf(aes(geometry = geom)) +
  geom_sf(data = precincts_noble)

int_noble <- st_intersection(tracts_noble, precincts_noble)

int_noble <- int_noble %>%
  mutate(small_area = st_area(.),
         area_ratio = small_area/total_area,
         pop_approx = round(pop17 * area_ratio))

blocks <- tigris::blocks(state = "oh", county = "noble")

blocks <- blocks %>%
  st_as_sf() %>%
  st_transform(26917)

totalpop_blocks <- read_csv("R/data/totalpop_2010_blocks.csv")

blocks_pop_noble <- blocks %>%
  select(GEOID10) %>%
  mutate(GEOID = as.numeric(GEOID10)) %>%
  left_join(totalpop_blocks) %>%
  filter(stringr::str_detect(as.character(GEOID), "39121")) %>%
  mutate(total_area = st_area(.))

int_noble_blocks <- st_intersection(blocks_pop_noble, precincts_noble) %>%
  mutate(small_area = st_area(.),
         area_ratio = small_area/total_area,
         pop_approx = round(value * area_ratio))


###### clark county

blocks <- tigris::blocks(state = "oh", county = "clark")

blocks <- blocks %>%
  st_as_sf() %>%
  st_transform(26917)

blocks_pop_clark <- blocks %>%
  select(GEOID10) %>%
  mutate(GEOID = as.numeric(GEOID10)) %>%
  left_join(totalpop_blocks) %>%
  mutate(total_area = st_area(.))

precincts_clark <- precincts %>%
  filter(COUNTY == "clark")

int_clark_blocks <- st_intersection(blocks_pop_clark, precincts_clark) %>%
  mutate(small_area = st_area(.),
         area_ratio = small_area/total_area,
         pop_approx = round(value * area_ratio))

View(int_clark_blocks %>% select(value, area_ratio, pop_approx))

int_clark_blocks_sumPrecinct <- int_clark_blocks %>%
  group_by(precinct_code) %>%
  summarise(total_pop_approx = sum(pop_approx)) %>%
  mutate(total_pop_approx = as.numeric(total_pop_approx))

precincts_gg <- ggplot(int_clark_blocks_sumPrecinct) + 
  geom_sf(aes(fill = total_pop_approx, color = total_pop_approx)) +
  scale_fill_gradient(low = "#FAF2FF", high = "#87005A") + theme_minimal() + ggtitle("Population Distributed Across Precincts") + scale_color_gradient(low = "#FAF2FF", high = "#87005A")

blocks_gg <- ggplot(blocks_pop_clark, aes(fill = value, color = value)) +geom_sf() + scale_fill_gradient(low = "#FAF2FF", high = "#87005A") + theme_minimal() + ggtitle("Population Distributed Across Blocks") + scale_color_gradient(low = "#FAF2FF", high = "#87005A")

