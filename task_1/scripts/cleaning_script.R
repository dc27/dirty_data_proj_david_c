# load in libraries and the raw (dirty) data
library(tidyverse)
library(readr)

decathalon_data <- read_rds("data/decathlon.rds")

# clean the col names
decath_data_clean_titles <- decathalon_data %>%
  janitor::clean_names()

# clean the data. change column name, add new column for name, change format of
# name, change order of columns
clean_data <- decath_data_clean_titles %>% 
  rename(javelin = javeline) %>% 
  rownames_to_column("name") %>% 
  mutate(name = str_to_upper(name)) %>%
  select(name, long_jump, high_jump, pole_vault, shot_put, javelin, discus,
         x100m, x110m_hurdle, x400m, x1500m, points, rank, competition)

write_csv(clean_data, "data/clean_data")