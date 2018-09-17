library(tidyverse)
library(sf)
library(e1071)
library(skimr)


ohio_shp <- st_read("shp/precincts_results.shp")

dim(ohio_shp)

# 9298 rows, 25 columns

sum(is.na(ohio_shp$PREC_SHP)) / nrow(ohio_shp)

# 2.25% precincts (209) in election results missing from shapefile

sum(is.na(ohio_shp$PREC_ELEC)) / nrow(ohio_shp)

# 3.915% precincts (364) in shapefile missing from election results

mean(ohio_shp$TRNOUT_PCT, na.rm = TRUE)

# mean turnout percent = 71.46%

sd(ohio_shp$TRNOUT_PCT, na.rm = TRUE)

# sd turnout percent = 10.43

skewness(ohio_shp$TRNOUT_PCT, na.rm = TRUE)

# skewness turnout percent = -0.923 (left skewed)

ggplot(ohio_shp, aes(x = TRNOUT_PCT)) +
  geom_histogram(fill = "cornflowerblue", alpha = 0.6, color = "white") +
  theme_minimal() +
  labs(title = "Distribution of turnout rates in Ohio precincts",
       subtitle = "General election in 2016",
       x = "Turnout rate of registered voters (%)",
       y = "Density")


