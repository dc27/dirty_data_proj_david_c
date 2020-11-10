library(tidyverse)

dataset_1 <- readxl::read_xlsx("data/dirty_data/boing-boing-candy-2015.xlsx", 1)

make_data1_clean <- function(dirty_data) {
  
  clean_names <- dirty_data %>% 
    janitor::clean_names()
  
  # remove extraneous columns
  subset_clean_names <- clean_names %>% 
    select(-timestamp & !starts_with("please") &
             !starts_with("check_all") &
             !starts_with("betty") &
             !starts_with("that_dress_that") &
             !starts_with("fill_in") &
             !contains("favourite_font") &
             !starts_with("which_day") &
             !starts_with("guess") &
             !starts_with("if_you_squint"))
  
  # rename some columns
  subset_clean_better_var_names <- subset_clean_names %>% 
    rename(age = how_old_are_you,
           going_out = are_you_going_actually_going_trick_or_treating_yourself,
           hersheys_dark_chocolate = dark_chocolate_hershey,
           hersheys_milk_chocolate = hershey_s_milk_chocolate,
           hersheys_kissables = hershey_s_kissables,
           peanut_m_ms = peanut_m_m_s,
           bonkers_the_candy = bonkers,
           sea_salt_flavored_stuff = sea_salt_flavored_stuff_probably_chocolate_since_this_is_the_it_flavor_of_the_year)
    
  subset_clean_names_and_age <- subset_clean_better_var_names %>%     
    mutate(age = as.integer(age),
           year = 2015,
           country = NA,
           gender = NA) %>%
    mutate(going_out = as.logical(
      case_when(
        going_out == "Yes" ~ TRUE,
        going_out == "No" ~ FALSE,
        TRUE ~ NA)),
      country = as.character(country),
      gender = as.character(gender))
  # someone put "fifty.nine.ish" - "I don't know what number this is so it's been treated as NA.
  
  
  subset_clean_names_and_age %>% 
    write_csv("data/clean_data/candy_2015.csv")

}

make_data1_clean(dataset_1)