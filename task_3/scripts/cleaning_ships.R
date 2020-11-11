# load in the tidyverse
library(tidyverse)

# read in the dirty ship data
ship_data <- readxl::read_excel("data/raw_data/seabirds.xls", 1)

# create cleaning function
make_ship_data_clean <- function(dirty_data) {
    
  # 1. take a subset of the wide ship data
  ship_data_subset <- ship_data %>% 
    select(RECORD,
           `RECORD ID`,
           LAT, LONG)
  
  # 2. renmae columns according to style guide
  ships_clean_names <- ship_data_subset %>% 
    rename(ship_record = RECORD,
           record_id = `RECORD ID`,
           lat = LAT,
           long = LONG)
  
  # write the clean data to a new csv
  ships_clean_names %>% 
    write_csv("data/clean_data/ships.csv")
}  

make_ship_data_clean(ship_data)

