# load in the tidyverse

library(tidyverse)
library(assertr)

# load in the data

dataset_2 <- readxl::read_xlsx("data/dirty_data/boing-boing-candy-2016.xlsx", 1)

# create cleaning function
make_data2_clean <- function(dirty_data) {
  
  # 1. clean column names
  clean_names <- dirty_data %>% 
    janitor::clean_names()
  
  # 2. clean column names
  subset_clean_names <- clean_names %>% 
    select(-timestamp &
             !starts_with("please") &
             !starts_with("betty") &
             !starts_with("that_dress") &
             !contains("favourite_font") &
             !starts_with("do_you") &
             !starts_with("guess") &
             !starts_with("when_you") &
             !contains("ignore"))
  
  # 3. rename columns for consistency
  subset_clean_better_var_names <- subset_clean_names %>%
    rename(country = which_country_do_you_live_in,
           age = how_old_are_you,
           gender = your_gender,
           going_out = are_you_going_actually_going_trick_or_treating_yourself,
           licorice = licorice_yes_black,
           person_of_interest_season_3 =
             person_of_interest_season_3_dvd_box_set_not_including_disc_4_with_hilarious_outtakes,
           box_o_raisins = boxo_raisins,
           peanut_m_ms = peanut_m_m_s,
           sweetums = sweetums_a_friend_to_diabetes,
           hersheys_milk_chocolate = hershey_s_milk_chocolate,
           sourpatch_kids = sourpatch_kids_i_e_abominations_of_nature
    ) %>% 
    select(!starts_with("which"))
  
  # 4. clean up the ages, convert going_out to logical and add a column for the
  # year the data was taken
  subset_clean_names_and_age <- subset_clean_better_var_names %>% 
    mutate(age = as.integer(age),
           year = 2016) %>% 
    mutate(going_out = as.logical(
      case_when(
        going_out == "Yes" ~ TRUE,
        going_out == "No" ~ FALSE,
        TRUE ~ NA)))
  
  # verify data types of columns before writing
  subset_clean_names_and_age %>% 
    verify(mode(gender) == "character") %>% 
    verify(mode(age) == "numeric") %>% 
    verify(mode(country) =="character") %>% 
    verify(mode(going_out) == "logical")
  
  # write the clean data
  subset_clean_names_and_age %>%
    write_csv("data/clean_data/candy_2016.csv")
  
}

make_data2_clean(dataset_2)