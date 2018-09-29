library(tidyverse)
library(sf)
library(tigris)
library(extrafont)


noble <- read_csv("R/data/geocoded/noble_geo.csv")

noble_blocks <- blocks(state = "oh", county = "noble") 

noble_sf <- noble_blocks %>%
  st_as_sf() %>%
  st_transform(26917)

######################## statistics about noble

sum(is.na(noble$BLOCK_GEOID)) / nrow(noble) # 19.24% of addresses were NA

geoid <- noble %>%
  mutate(BLOCK_GEOID = as.character(BLOCK_GEOID)) %>%
  group_by(BLOCK_GEOID) %>%
  summarise(total = n())

geoid %>%
  drop_na() %>%
  summarise(mean = mean(total),
            sd = sd(total),
            median = median(total)) # summary statistics

geoids_vf <- unique(as.character(noble$BLOCK_GEOID))

geoids_sf <- unique(noble_sf$GEOID10)

diff <- setdiff(geoids_vf, geoids_sf)

diff <- diff[!is.na(diff)]

diff_voters <- noble %>%
  filter(BLOCK_GEOID %in% diff)

diff_blocks <- diff_voters %>%
  mutate(BLOCK_GEOID = as.character(BLOCK_GEOID)) %>%
  group_by(BLOCK_GEOID) %>%
  count() 

# There are 13 blocks with voters who don't live in Noble County, filter them out

noble2 <- noble %>%
  filter(! BLOCK_GEOID %in% diff)

nrow(noble) - nrow(noble2) # 45 fewer voters

noble_join <- noble %>%
  mutate(BLOCK_GEOID = as.character(BLOCK_GEOID)) %>%
  full_join(noble_sf, by = c("BLOCK_GEOID" = "GEOID10"))
  

geoids_grouped <- noble_join %>%
  group_by(BLOCK_GEOID) %>%
  summarise(voters = n())

map_voters <- noble_sf %>%
  left_join(geoids_grouped, by = c("GEOID10" = "BLOCK_GEOID"))
  
  
############################## setting up the functions

# functions I'll use later

st_rook = function(a, b = a) sf::st_relate(a, b, pattern = "F***1****")

st_queen <- function(a, b = a) sf::st_relate(a, b, pattern = "F***T****")

lookup_precinct <- function(index, precinct_name_var = "PRECINCT_NAME"){
  precinct <- precincts_nb[index, precinct_name_var]
  return(precinct)
}

create_precinct_vector <- function(){
  prec <- purrr::map(precincts_nb$NB_ROOK, lookup_precinct)
  prec_vec <- purrr::map(prec, dplyr::pull)
  return(prec_vec)
}


clean_precincts_fun <- function(i){
  y <- precincts_nb_sub$precinct_nn[[i]]
  print(y)
  new_y <- y[!is.na(y)]
  return(new_y)
}


join_voters_to_blocks <- function(voters, blocks, block_geoid_voters = "BLOCK_GEOID", precinct_name = "PRECINCT_NAME"){
  colnames(voters)[colnames(voters)==block_geoid_voters] <- 'BLOCK_GEOID'
  colnames(voters)[colnames(voters)==precinct_name] <- 'PRECINCT_NAME'
  precincts <- voters %>%
    # mutate(BLOCK_GEOID = as.character(!! block_geoid_q)) %>%
    # rename(PRECINCT_NAME = (!! precinct_name_q)) %>%
    dplyr::group_by(BLOCK_GEOID, PRECINCT_NAME) %>% # for each block, precinct combination
    dplyr::summarise(c=n()) %>% # counts the number of times a precinct is counted for a particular block
    dplyr::filter(row_number(desc(c))==1) # dataframe of precincts from voterfile, takes the most common precinct assignment for a block
  
  precincts_geo <- blocks %>%
    dplyr::mutate(GEOID10 = (GEOID10)) %>%
    dplyr::left_join(precincts, by = c("GEOID10" = "BLOCK_GEOID")) %>% # combine precincts with block shapefile
    dplyr::mutate(dimension = st_dimension(.)) %>%
    dplyr::filter(!(is.na(dimension))) # take out empty polygons
  
  return(precincts_geo)
  
}

find_neighbors <- function(x, type = "rook"){
  if (type == "rook"){
    NB <- st_rook(x)
  } else if (type == "queen") {
    NB <- st_queen(x)
  } else {
    stop("Please enter 'rook' or 'queen' for neighbor type")
  }
  x$NB <- NA
  for (i in 1:length(NB)){
    x$NB[i] <- NB[i]
  }
  
  return(x)
}


lookup_precincts_nn <- function(x){
  precinct_nn <- list()
  
  for (i in 1:nrow(x)) {
    precincts <- c()
    
    for (y in x$NB[[i]]){
      precincts <- c(precincts, x[[y, 'PRECINCT_NAME']])
    }
    
    precinct_nn[[i]] <- precincts
    
  }
  
  x$precinct_nn <- precinct_nn
  
  return(x)
  
}


classify_nn <- function(x){
  precincts_nb_sub <- x %>%
    filter(!is.na(precinct_nn),
           !identical(precinct_nn, character(0)))
  
  clean_precincts_fun <- function(i){
    y <- precincts_nb_sub$precinct_nn[[i]]
    new_y <- y[!is.na(y)]
    return(new_y)
  }
  
  p_new <- map(1:nrow(precincts_nb_sub), clean_precincts_fun)
  
  p_lengths <- map(1:length(p_new), function(x) length(p_new[[x]]))
  
  precincts_nb_sub <- precincts_nb_sub %>%
    mutate(precinct_nn_clean = p_new,
           precinct_nn_length = p_lengths) %>%
    filter(precinct_nn_length > 0) # take out the blocks without meaningful neighbors
  
  for (i in 1:nrow(precincts_nb_sub)){
    precincts_nb_sub$PRECINCT_NAME[i] <- ifelse(is.na(precincts_nb_sub$PRECINCT_NAME[i]), names(which.max(table(precincts_nb_sub$precinct_nn[[i]]))), precincts_nb_sub$PRECINCT_NAME[i])
  } # assign a precinct as the max of the vector of precincts
  
  precincts_nb_full <- x %>%
    left_join(precincts_nb_sub %>% st_set_geometry(NULL) %>% select(GEOID10, PRECINCT_NAME), by = "GEOID10") %>%
    mutate(PRECINCT_NAME.x = if_else(is.na(PRECINCT_NAME.x), PRECINCT_NAME.y, PRECINCT_NAME.x)) %>%
    select(-PRECINCT_NAME.y) %>%
    rename(PRECINCT_NAME = PRECINCT_NAME.x)
  
  message(paste0("There are ", sum(is.na(x$PRECINCT_NAME)), " unclassified blocks."))
  
  return(precincts_nb_full)
}


#############################

noble_nn <- noble %>%
  mutate(BLOCK_GEOID = as.character(BLOCK_GEOID)) %>%
  join_voters_to_blocks(noble_sf)


noble %>%
  mutate(BLOCK_GEOID = as.character(BLOCK_GEOID)) %>%
  group_by(BLOCK_GEOID) %>%
  summarise(total = n()) %>% 
  full_join(noble_sf, by = c("BLOCK_GEOID" = "GEOID10")) %>%
  ggplot() +
  geom_sf(aes(fill = total, colour=""), color = "gray90") +
  scale_fill_gradient(low = "#e0ecf4", high = "#4d004b", na.value = "#fff8dd", guide = guide_colorbar(ticks = FALSE, title = "Valid addresses \nper census block")) +
  theme_minimal() +
  theme(panel.grid.major = element_line(color = "transparent"),
        axis.text = element_blank(),
        text = element_text(family = "Century Gothic", color = "#a7aeba")
        ) +
  scale_colour_manual(values=NA) + 
  guides(colour=guide_legend("No data", override.aes=list(colour="#fff8dd"))) +
  ggtitle("Geocoded addresses in Noble County, Ohio")

ggsave("R/plots/noble1.png")



noble_nn1 <- noble %>%
  mutate(BLOCK_GEOID = as.character(BLOCK_GEOID)) %>%
  join_voters_to_blocks(noble_sf) %>%
  find_neighbors() %>%
  lookup_precincts_nn() %>%
  classify_nn()

palette <- randomcoloR::randomColor(19, luminosity = "light")

ggplot(noble_nn1, aes(fill = PRECINCT_NAME)) +
  geom_sf(color = "gray90") +
  scale_fill_manual(values = palette) +
  theme_minimal() +
  theme(panel.grid.major = element_line(color = "transparent"),
        axis.text = element_blank(),
        text = element_text(family = "Century Gothic", color = "#a7aeba", size = 16),
        legend.position = "none") +
  ggtitle("Precinct classifications after one iteration in Noble County, OH")

ggsave("R/plots/noble2.png")  

noble_nn2 <- noble_nn1 %>%
  lookup_precincts_nn() %>%
  classify_nn()

ggplot(noble_nn2, aes(fill = PRECINCT_NAME)) +
  geom_sf(color = "gray90") +
  scale_fill_manual(values = palette) +
  theme_minimal() +
  theme(panel.grid.major = element_line(color = "transparent"),
        axis.text = element_blank(),
        text = element_text(family = "Century Gothic", color = "#a7aeba", size = 16),
        legend.position = "none") +
  ggtitle("Precinct classifications after two iterations in Noble County, OH")

ggsave("R/plots/noble3.png")

noble_nn3 <- noble_nn2 %>%
  lookup_precincts_nn() %>%
  classify_nn()

ggplot(noble_nn3, aes(fill = PRECINCT_NAME)) +
  geom_sf(color = "gray90") +
  scale_fill_manual(values = palette) +
  theme_minimal() +
  theme(panel.grid.major = element_line(color = "transparent"),
        axis.text = element_blank(),
        text = element_text(family = "Century Gothic", color = "#a7aeba", size = 16),
        legend.position = "none") +
  ggtitle("Precinct classifications after three iterations in Noble County, OH")

ggsave("R/plots/noble4.png")

noble_nn4 <- noble_nn3 %>%
  lookup_precincts_nn() %>%
  classify_nn()

ggplot(noble_nn4, aes(fill = PRECINCT_NAME)) +
  geom_sf(color = "gray90") +
  scale_fill_manual(values = palette) +
  theme_minimal() +
  theme(panel.grid.major = element_line(color = "transparent"),
        axis.text = element_blank(),
        text = element_text(family = "Century Gothic", color = "#a7aeba", size = 16),
        legend.position = "none") +
  ggtitle("Precinct classifications after four iterations in Noble County, OH")

ggsave("R/plots/noble5.png")

noble_nn5 <- noble_nn4 %>%
  lookup_precincts_nn() %>%
  classify_nn()

ggplot(noble_nn5, aes(fill = PRECINCT_NAME)) +
  geom_sf(color = "gray90") +
  scale_fill_manual(values = palette) +
  theme_minimal() +
  theme(panel.grid.major = element_line(color = "transparent"),
        axis.text = element_blank(),
        text = element_text(family = "Century Gothic", color = "#a7aeba", size = 16),
        legend.position = "none") +
  ggtitle("Precinct classifications after five iterations in Noble County, OH")

ggsave("R/plots/noble6.png")

noble_nn6 <- noble_nn5 %>%
  lookup_precincts_nn() %>%
  classify_nn()

ggplot(noble_nn6, aes(fill = PRECINCT_NAME)) +
  geom_sf(color = "gray90") +
  scale_fill_manual(values = palette) +
  theme_minimal() +
  theme(panel.grid.major = element_line(color = "transparent"),
        axis.text = element_blank(),
        text = element_text(family = "Century Gothic", color = "#a7aeba", size = 16),
        legend.position = "none") +
  ggtitle("Precinct classifications after six iterations in Noble County, OH")

ggsave("R/plots/noble7.png")

noble_nn7 <- noble_nn6 %>%
  lookup_precincts_nn() %>%
  classify_nn()

ggplot(noble_nn7, aes(fill = PRECINCT_NAME)) +
  geom_sf(color = "gray90") +
  scale_fill_manual(values = palette) +
  theme_minimal() +
  theme(panel.grid.major = element_line(color = "transparent"),
        axis.text = element_blank(),
        text = element_text(family = "Century Gothic", color = "#a7aeba", size = 16),
        legend.position = "none") +
  ggtitle("Precinct classifications after seven iterations in Noble County, OH")

ggsave("R/plots/noble8.png")

