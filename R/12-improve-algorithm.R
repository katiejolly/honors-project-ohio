library(tidyverse)
library(sf)

# helper functions

st_rook = function(a, b = a) sf::st_relate(a, b, pattern = "F***1****") # find rook-contiguity neighbors

lookup_precinct <- function(index, precinct_name_var = "PRECINCT_NAME"){ # for a given row index, find the precinct name
  precinct <- precincts_nb[index, precinct_name_var]
  return(precinct)
}

create_precinct_vector <- function(){ # create a vector of precinct names from lookup_precinct
  prec <- purrr::map(precincts_nb$NB_ROOK, lookup_precinct)
  prec_vec <- purrr::map(prec, dplyr::pull)
  return(prec_vec)
}


clean_precincts_fun <- function(i){ # clean the precincts, take out NA values
  y <- precincts_nb_sub$precinct_nn[[i]]
  print(y)
  new_y <- y[!is.na(y)]
  return(new_y)
}
#################################################

# Use this function to assign 2010 census blocks to a particular precinct based on the most common precinct assignment of the voters geotagged there.

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

# Assign a neighborhood to each block

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
    x$NB_length[i] <- length(x$NB[[i]])
  }
  
  
  return(x)
}

# Lookup the precinct names of each block in a neighborhood.


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
  
  for (i in 1:nrow(x)){
    x$na_length[i] = sum(is.na(x$precinct_nn[[i]])) # count the number of NAs
  }
  
  x$percent_nn_preclassified <- 1 - ( x$na_length / x$NB_length)
  
  
  return(x)
  
}

# Classify any unassigned block with the most common precinct name of the rook-neighborhood.

classify_nn <- function(x, alpha = 0.5){
  
  x <- x %>%
    mutate(less_than_alpha = percent_nn_preclassified <= alpha) # percent of neighbors is less than the required number
  
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
           precinct_nn_length = p_lengths) %>% # finds the number of classified neighbors
    filter(precinct_nn_length > 0)# take out the blocks without meaningful neighbors
  


  for (i in 1:nrow(precincts_nb_sub)){
    precincts_nb_sub$PRECINCT_NAME[i] <- ifelse(is.na(precincts_nb_sub$PRECINCT_NAME[i]), 
                                                ifelse(precincts_nb_sub$less_than_alpha == FALSE, 
                                                       names(which.max(table(precincts_nb_sub$precinct_nn[[i]]))), 
                                                       precincts_nb_sub$PRECINCT_NAME[i]), 
                                                precincts_nb_sub$PRECINCT_NAME[i])
  } # assign a precinct as the max of the vector of precincts
  
  precincts_nb_full <- x %>%
    left_join(precincts_nb_sub %>% st_set_geometry(NULL) %>% select(GEOID10, PRECINCT_NAME, NB_length, precinct_nn_length, na_length, less_than_alpha), by = "GEOID10") %>%
    mutate(PRECINCT_NAME.x = if_else(is.na(PRECINCT_NAME.x), PRECINCT_NAME.y, PRECINCT_NAME.x)) %>%
    select(-PRECINCT_NAME.y) %>%
    rename(PRECINCT_NAME = PRECINCT_NAME.x)
  
  message(paste0("There are ", sum(is.na(x$PRECINCT_NAME)), " unclassified blocks."))
  
  return(precincts_nb_full)
}




########### test on Noble County

#noble <- read_csv("R/data/geocoded/noble_geo.csv")

#noble_blocks <- tigris::blocks(county = "noble", state = "oh")

#noble_sf <- noble_blocks %>%
  #st_as_sf()

noble_join <- noble %>% 
  mutate(BLOCK_GEOID = as.character(BLOCK_GEOID)) %>%
  join_voters_to_blocks(noble_sf) %>%
  find_neighbors() %>%
  lookup_precincts_nn() %>%
  classify_nn(alpha = 0.5)


