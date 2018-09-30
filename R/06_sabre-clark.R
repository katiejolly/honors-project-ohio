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

ggplot(base_vm) +
  geom_sf(aes(fill = rih), color = "gray90") +
  scale_fill_distiller(palette = "YlGnBu", guide = "colourbar", breaks = c(0, 0.05, 0.1, 0.15, 0.2, 0.25, 0.28), labels = c("", "0.05 (less)",  "", "","", "", "0.3 (more)")) +
  theme_minimal() +
  theme(panel.grid.major = element_line(color = "transparent"),
        axis.text = element_blank(),
        text = element_text(family = "Century Gothic", color = "#a7aeba", size = 20),
        plot.title = element_text(face="bold"),
        plot.subtitle = element_text(size = 14),
        legend.title = element_text(size = 16)) +
  guides(fill = guide_colorbar(title = "Regional inhomogeneity \nin the base layer\n", barheight = 10, ticks = FALSE, barwidth = 1.5)) +
  ggtitle("Regional inhomogeneity in Clark County \nmeasured by the information-theoretical V-measure") +
  labs(subtitle = "\nComparing the base map to the approximated map")

ggplot(base_vm) +
  geom_histogram(aes(x = rih, fill = rih), color = "white", fill = "#A7AEC3", binwidth = 0.01) +
  theme_minimal() +
  theme(panel.grid.major = element_line(color = "transparent"),
        text = element_text(family = "Century Gothic", color = "#a7aeba", size = 20),
        plot.title = element_text(face="bold"),
        plot.subtitle = element_text(size = 14),
        legend.title = element_text(size = 16)) +
  labs(title = "RIH Distribution",
       subtitle = "Base map\n\n",
       x = "regional inhomogeneity",
       y = "Precincts") +
  ylim(0, 15)

approx_vm <- clark_vmeasure$map2

mapview::mapview(approx_vm, zcol = "rih", legend = TRUE) # view the map!!

ggplot(approx_vm) +
  geom_sf(aes(fill = rih), color = "gray90") +
  scale_fill_distiller(palette = "YlGnBu", guide = "colourbar", breaks = c(0, 0.05, 0.1, 0.15, 0.2, 0.25, 0.28), labels = c("", "0.05 (less)",  "", "","", "0.3 (more)", "")) +
  theme_minimal() +
  theme(panel.grid.major = element_line(color = "transparent"),
        axis.text = element_blank(),
        text = element_text(family = "Century Gothic", color = "#a7aeba", size = 20),
        plot.title = element_text(face="bold"),
        plot.subtitle = element_text(size = 14),
        legend.title = element_text(size = 16)) +
  guides(fill = guide_colorbar(title = "Regional inhomogeneity \nin the approximated layer\n", barheight = 10, ticks = FALSE, barwidth = 1.5)) +
  ggtitle("Regional inhomogeneity in Clark County \nmeasured by the information-theoretical V-measure") +
  labs(subtitle = "\nComparing the approximated map to the base map") 

ggplot(approx_vm) +
  geom_histogram(aes(x = rih, fill = rih), color = "white", fill = "#A7AEC3", binwidth = 0.01) +
  theme_minimal() +
  theme(panel.grid.major = element_line(color = "transparent"),
        text = element_text(family = "Century Gothic", color = "#a7aeba", size = 20),
        plot.title = element_text(face="bold"),
        plot.subtitle = element_text(size = 14),
        legend.title = element_text(size = 16)) +
  labs(title = "RIH Distribution",
       subtitle = "Approximated map\n\n",
       x = "regional inhomogeneity",
       y = "Precincts")

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
