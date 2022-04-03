library(tidyverse)
library(readxl)

source("scripts//helpers.R")

raw_data_path <- (
  "data/dirty_data/"
)

# get names of files within raw data path
files <- list.files(raw_data_path)

# create full paths for each file
filepaths <- map(.x = files, ~paste0(raw_data_path, .x))

# year info is included in filenames. extract and use for sensible list names
years <- sapply(files, str_extract, pattern = "\\d+")
names(filepaths) <- years

# read all files. store in list to perform future cleaning with apply functions.
candy_data <- sapply(filepaths, read_xlsx)


# add id column to each dataset
candy_with_id <- map2(candy_data, names(candy_data), ~add_id_column(.x, .y))

respondent_info <- sapply(
  candy_with_id, select,
  id,
  age = matches("How old are you|Q3: AGE"),
  sex = matches("gender|sex"),
  trick_treating = matches("going"),
  country = matches("country")
) %>% 
  bind_rows(.id = "year")

# some columns contain "JOY", "DESPAIR", or "MEH". These are the ratings for
# different candies. columns containing these values should be pivoted to long
# format
pivot_col_contents <- c("JOY", "DESPAIR", "MEH")

# apply pivot and shrink function to each dataset before combining
long_candy_data <- sapply(
  candy_with_id, pivot_df, pivot_column_contents = pivot_col_contents,
  new_col_names = c("candy", "rating")
)

candy_info <- sapply(
  long_candy_data, select, c(id, candy, rating), simplify = FALSE
  ) %>% 
  bind_rows(.id = NULL)


clean_age <- respondent_info %>% 
  relocate(country, .after = age) %>%
  relocate(sex, .after = age) %>% 
  # cleaning the age column. sometimes the age has been entered in the country
  # column. Move these and state NA in country column before cleaning.
  mutate(age = case_when(
    is.na(age) ~ as.integer(str_extract(country, "[0-9]+")),
    TRUE ~ as.integer(age))) %>% 
  mutate(country = as.character(ifelse(str_detect(country,"[0-9]{2}"), NA, country))) %>% 
  mutate(age = str_extract(age, "[0-9]+[\\.,]*[0-9]*")) %>% 
  mutate(age = as.numeric(age)) %>%
  mutate(age = as.integer(floor(age))) %>% 
  mutate(age = ifelse(age > 102 | age < 1, NA_integer_, age))

clean_sex <- clean_age %>% 
  mutate(sex = case_when(
    sex == "I'd rather not say" ~ "Unspecified",
    is.na(sex) ~ "Unspecified",
    TRUE ~ sex))

us_patterns <- c(
  "USA",
  "U[\\.]*S",
  "United S+",
  "merica",
  "Murica",
  "States",
  "Yoo Ess",
  "Trump",
  "Merca",
  "New [JY]+",
  "murrika",
  "Alaska",
  "North Carolina",
  "u s a",
  "u s",
  "Cascadia"
)

uk_patterns <- c(
  "U[\\.]*K",
  "United K+",
  "scotland",
  "england"
)

canada_patterns <- c(
  "can[a]*"
)


us_regex <- paste0("(?i)", us_patterns, collapse = "|")
uk_regex <- paste0("(?i)", uk_patterns, collapse = "|")
canada_regex <- paste0("(?i)", canada_patterns, collapse = "|") 

clean_country <- clean_sex %>% 
  mutate(country = case_when(
    str_detect(country, us_regex) ~ "USA",
    str_detect(country, uk_regex) ~ "UK",
    str_detect(country, canada_regex) ~ "CANADA",
    is.na(country) ~ NA_character_,
    TRUE ~ "OTHER"
  ))

clean_country %>% 
  write_csv("data/clean_data/respondent_info.csv")


clean_candy <- candy_info %>%
  mutate(candy = str_extract(candy, "\\w+[’*'*\\s*\\w*&*\\(*\\)*\\-*]*")) %>%
  mutate(candy = case_when(
    str_detect(candy, "Q6") ~ NA_character_,
    str_detect(candy, "(JOY)|(DESPAIR)") ~ NA_character_,
    TRUE ~ candy)) %>% 
  filter(!is.na(candy))


clean_candy <- clean_candy %>% 
  mutate(across(where(is.character), ~replace_na(., "Unspecified")))


clean_candy %>% 
  write_csv("data/clean_data/candy.csv")

