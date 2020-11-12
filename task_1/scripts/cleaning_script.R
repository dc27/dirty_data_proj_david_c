# load in libraries and the raw (dirty) data
library(tidyverse)
library(readr)
library(assertr)

decathalon_data <- read_rds("data/raw_data/decathlon.rds")

# create cleaning and tidying function
make_decathalon_data_clean <- function(dirty_data) {
  # clean the col names
  decath_data_clean_titles <- decathalon_data %>%
    janitor::clean_names()
  
  # clean the data. change column name, add new column for name, change format of
  # name, change order of columns
  clean_decath_data <- decath_data_clean_titles %>% 
    rename(javelin = javeline) %>% 
    rownames_to_column("name")%>% 
    mutate(name = str_to_upper(name))
  
  # convert the data to long form:
  # instead of a column for each individual event, have one column for event
  # type and then another for the result/score/distance/time
  # rank isn't needed for analysis - drop it.
  long_decath_data <- clean_decath_data %>%
    select(-rank) %>% 
    pivot_longer(c(-name & -points & -competition),
                 names_to = "event",
                 values_to = "distance_or_time") 
  
  long_decath_data %>% 
    verify(ncol(long_decath_data) == 5) %>%
    verify(!is.na(distance_or_time)) %>%
    verify(mode(distance_or_time) == "numeric") %>%
    verify(mode(points) == "numeric") %>% 
    write_csv("data/clean_data/decathalon_results_data.csv")
}

make_decathalon_data_clean(decathalon_data)