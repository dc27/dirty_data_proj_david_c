# load in the tidyverse
library(tidyverse)
library(assertr)

# read in the dirty ship data
# 
# it's located in the first sheet of the seabirds xls file 
ship_data <- readxl::read_excel("data/raw_data/seabirds.xls", 1)

# create cleaning function
make_ship_data_clean <- function(dirty_data) {
    
  # 1. take a subset of the wide ship data
  ship_data_subset <- ship_data %>% 
    select(RECORD,
           `RECORD ID`,
           LAT, LONG)
  
  # 2. rename columns according to style guide
  ships_clean_names <- ship_data_subset %>% 
    rename(ship_record = RECORD,
           record_id = `RECORD ID`,
           lat = LAT,
           long = LONG)
  
  # verify n cols, and col types and then write the clean data to a new csv
  ships_clean_names %>% 
    verify(ncol(ships_clean_names) == 4) %>% 
    verify(mode(record_id) == "numeric") %>% 
    verify(mode(lat) == "numeric") %>% 
    verify(mode(long) == "numeric") %>%
    verify(is.na(long) | (long >= -180 & long <= 180)) %>% 
    verify(is.na(lat) | (lat >= -90 & long <= 90)) %>% 
    write_csv("data/clean_data/ships.csv")
}  

make_ship_data_clean(ship_data)

