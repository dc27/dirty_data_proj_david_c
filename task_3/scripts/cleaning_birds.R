# load in the tidyverse
library(tidyverse)
library(assertr)

# read in the raw bird data 
# 
# it's located in the second sheet of the seabirds xls file
bird_data <- readxl::read_excel("data/raw_data/seabirds.xls", 2)

# create cleaning function
make_bird_data_clean <- function (dirty_bird_data) {
  # 1. take a subset of the data with only the desired columns
  bird_data_subset <- dirty_bird_data %>%
    select(RECORD,
         `RECORD ID`,
         `Species common name (taxon [AGE / SEX / PLUMAGE PHASE])`,
         `Species  scientific name (taxon [AGE /SEX /  PLUMAGE PHASE])`,
         `Species abbreviation`,
         COUNT)
    
  # 2. clean the col_names according to style guide
  birds_clean_col_names <- bird_data_subset %>% 
    rename(bird_record = RECORD,
         record_id = `RECORD ID`,
         species = `Species common name (taxon [AGE / SEX / PLUMAGE PHASE])`,
         species_scientific =
           `Species  scientific name (taxon [AGE /SEX /  PLUMAGE PHASE])`,
         species_abbreviation = `Species abbreviation`,
         count = COUNT)
  
  # declare patterns that offer extra information
  age_patterns <- " AD| SUBAD| IMM| JUV"
  plumage_patterns <- " PL[0-9]+"
  colour_patterns <- " DRK| INT| LGHT| WHITE| LIGHT"
  
  # 3. remove the extra information by removing occurences of each pattern
  birds_clean_data <- birds_clean_col_names %>% 
    mutate(species = str_remove(species, age_patterns),
           species = str_remove(species, plumage_patterns),
           species = str_remove(species, colour_patterns),
           species_scientific = str_remove(species_scientific, age_patterns),
           species_scientific = str_remove(species_scientific, plumage_patterns),
           species_scientific = str_remove(species_scientific, colour_patterns),
           species_abbreviation = str_remove(species_abbreviation, age_patterns),
           species_abbreviation = str_remove(species_abbreviation, plumage_patterns),
           species_abbreviation = str_remove(species_abbreviation, colour_patterns))
  
  # verify n cols, write the clean data to new csv
  birds_clean_data %>%
    verify(ncol(birds_clean_data) == 6) %>%
    verify(!str_detect(names(birds_clean_data),"[A-Z]")) %>% 
    verify(mode(count) == "numeric") %>%
    verify(is.na(count) | count > 0) %>%
    write_csv("data/clean_data/seabirds.csv")
  
}

make_bird_data_clean(bird_data)