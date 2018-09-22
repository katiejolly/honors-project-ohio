library(tidyverse)
library(sf)
library(tigris)

# load all necessary data

madison_geo <- read_csv("R/data/geocoded/madison_done.csv")

madison_blocks <- blocks(state = "OH", county = "madison") %>%
  st_as_sf()

#################

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


############################

# run the nearest neighbor code from untidyprecinct

madison_nn <- madison_geo %>%
  mutate(BLOCK_GEOID = as.character(GEOID10)) %>%
  join_voters_to_blocks(madison_blocks) %>%
  find_neighbors() %>%
  lookup_precincts_nn() %>%
  classify_nn()

madison_nn2 <- madison_nn %>%
  lookup_precincts_nn() %>%
  classify_nn()

madison_nn3 <- madison_nn2 %>%
  lookup_precincts_nn() %>%
  classify_nn()

madison_nn4 <- madison_nn3 %>%
  lookup_precincts_nn() %>%
  classify_nn()

madison_nn5 <- madison_nn4 %>%
  lookup_precincts_nn() %>%
  classify_nn()


########################

# function to dissolve the precincts


dissolve_precincts <- function(x){
  if (sum(is.na(x$PRECINCT_NAME)) != 0) {
    stop("There are still unclassified blocks in this shapefile")
  }
  dis <- x %>% 
    dplyr::group_by(PRECINCT_NAME) %>%
    summarise(n_blocks = n()) %>%
    rename(PRECINCT = PRECINCT_NAME)
  return(dis)
}

####################

# dissolve the madison precincts

madison_dissolved <- madison_nn5 %>%
  dissolve_precincts()

madison_valid <- lwgeom::st_make_valid(madison_dissolved) %>%
  st_transform(4326) %>% # WGS 84
  st_cast("MULTIPOLYGON")

mapview::mapview(madison_valid["PRECINCT"], col.regions = sf.colors(24)) # view the map!!

st_write(madison_valid, "R/data/madison_valid_approx.shp")