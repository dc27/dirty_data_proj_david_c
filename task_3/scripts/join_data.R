# load in the tidyverse
library(tidyverse)


# there are two datasets to join
# one relates to bird sightings
# the other relates to the ships the birds were seen from
bird_data <- read_csv("data/clean_data/seabirds.csv")
ship_data <- read_csv("data/clean_data/ships.csv")


# create function
join_birds_and_ships <- function(clean_bird_data, clean_ship_data) {
  birds_and_ships <- clean_bird_data %>% 
    inner_join(clean_ship_data,
               by = c("record_id" = "record_id"))
  
  birds_and_ships %>% 
    write_csv("data/clean_data/birds_and_ship_data.csv")
}


join_birds_and_ships(bird_data, ship_data)