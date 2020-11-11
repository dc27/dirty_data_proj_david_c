# load in the tidyverse

library(tidyverse)
library(assertr)

# load in the data

dataset_3 <- readxl::read_xlsx("data/dirty_data/boing-boing-candy-2017.xlsx", 1)

# create the cleaning function
make_data3_clean <- function(dirty_data) {
  
  # 1. clean column names
  clean_names <- dirty_data %>% 
    janitor::clean_names()
  
  # 2. take a subset of the data with only desired columns
  subset_clean_names <- clean_names %>% 
    select(starts_with("q"))
  
  # 3. remove initial "q_" from all column names
  names(subset_clean_names) <- str_remove(names(subset_clean_names), "q[0-9]+_")
  
  # 4. take a further subset of the data
  further_subset_clean_names <- subset_clean_names %>% 
    select(-state_province_county_etc &
             !starts_with("media") & 
             -dress, -joy_other, -despair_other, -other_comments, -day)
  
  # 5. rename columns for consistency across datasets
  furth_sub_clean_better_var_names <- further_subset_clean_names %>% 
    rename(anonymous_brown_globs_that_come_in_black_and_orange_wrappers =
             anonymous_brown_globs_that_come_in_black_and_orange_wrappers_a_k_a_mary_janes,
           peanut_m_ms = peanut_m_m_s,
           box_o_raisins = boxo_raisins,
           hersheys_milk_chocolate = hershey_s_milk_chocolate,
           licorice = licorice_yes_black,
           sweetums = sweetums_a_friend_to_diabetes,
           x100_grand_bar = "100_grand_bar")
  clean_names_and_age <- furth_sub_clean_better_var_names %>% 
    mutate(age = as.integer(age),
           year = 2017) %>% 
  mutate(going_out = as.logical(
    case_when(
      going_out == "Yes" ~ TRUE,
      going_out == "No" ~ FALSE,
      TRUE ~ NA)))
  
  # verify data types of columns before writing
  clean_names_and_age %>% 
    verify(mode(gender) == "character") %>% 
    verify(mode(age) == "numeric") %>% 
    verify(mode(country) =="character") %>% 
    verify(mode(going_out) == "logical")
  
  # write the clean data
  clean_names_and_age %>%
    write_csv("data/clean_data/candy_2017.csv")
  
}

make_data3_clean(dataset_3)