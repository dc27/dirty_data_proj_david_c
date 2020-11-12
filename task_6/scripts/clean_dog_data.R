
# load in the tidyverse
library(tidyverse)

# read in the data
dog_data <- read_csv("data/raw_data/dog_survey.csv")

make_dog_data_clean <- function (dirty_data) {
  
  unique_rows <- dirty_data  %>% 
    unique() %>% 
    select(!starts_with("X")) %>%
    rename("title" = Title) %>% 
    separate_rows(dog_size, dog_gender, dog_age, convert = TRUE, sep = ",| and ")
  
  small_pattern <- "smal"
  medium_pattern <- "medi"
  large_pattern <- "large"
  male_pattern <- "male|boy"
  female_pattern <- "female|girl"
  
  clean_sizes_dog_data <- unique_rows %>% 
    mutate(dog_size = str_to_lower(dog_size),
           dog_gender = str_to_lower(dog_gender)) %>% 
    mutate(dog_size = case_when(
      dog_size == "s" ~ dog_size,
      dog_size == "xs" ~ dog_size,
      dog_size == "m" ~ dog_size,
      dog_size == "l" ~ dog_size,
      dog_size == "xl" ~ dog_size,
      str_detect(dog_size, small_pattern) ~ "s",
      str_detect(dog_size, medium_pattern) ~ "m",
      str_detect(dog_size, large_pattern) ~ "l"
    ))
  
  clean_genders_and_sizes <- clean_sizes_dog_data %>% 
    mutate(dog_gender = case_when(
      dog_gender == "f" ~ dog_gender,
      dog_gender == "m" ~ dog_gender,
      str_detect(dog_gender, female_pattern) ~ "f",
      str_detect(dog_gender, male_pattern) ~ "m"))
  
  clean_dog_food_col <- clean_genders_and_sizes %>% 
    mutate(amount_spent_on_dog_food = as.numeric(str_remove(amount_spent_on_dog_food,"£"))) %>% 
    mutate(amount_spent_on_dog_food = ifelse(amount_spent_on_dog_food < 0, NA, amount_spent_on_dog_food))
  
  clean_dog_data <- clean_dog_food_col %>% 
    mutate(title = ifelse(title == "Honorable", "Hnr", title)) %>% 
    mutate(dog_age = as.numeric(str_extract(dog_age,"[0-9]+")))
  
  clean_dog_data %>% 
    write_csv("data/clean_data/dog_survey_data.csv")
  
}

make_dog_data_clean(dog_data)

