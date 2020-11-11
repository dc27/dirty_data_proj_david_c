# load in the tidyverse and each of the datasets
# there is one dataset per year

library(tidyverse)
library(assertr)

dataset_2015 <- read_csv("data/clean_data/candy_2015.csv")
dataset_2016 <- read_csv("data/clean_data/candy_2016.csv")
dataset_2017 <- read_csv("data/clean_data/candy_2017.csv")

# function that takes the wide data (with many columns that refer to candies)
# and produces a long data (where candy is now one column with many candy types)
make_data_long <- function (wide_data) {
  long_data <- wide_data %>% 
    pivot_longer(-year & -going_out & -gender & -age & -country,
                 names_to = "candy",
                 values_to = "reaction") %>%
    select(year, age, gender, country, going_out, candy, reaction)
}

# some rows have no observations for any candy, and include no gender, age, or
# country information
remove_empty_rows <- function(data) {
  data %>%
  verify(ncol(data)==7)
  
  filtered_data <- data %>% 
    filter(!(is.na(going_out) &
               is.na(gender) &
               is.na(age) &
               is.na(country) &
               is.na(reaction)))
}

make_data_tidy <- function(dataset) {
  long_data <- make_data_long(dataset)
  filtered_long_data <- remove_empty_rows(long_data)
}

# process each dataset
# 
tidy_2015 <- make_data_tidy(dataset_2015)
tidy_2016 <- make_data_tidy(dataset_2016)
tidy_2017 <- make_data_tidy(dataset_2017)

  # bind the tidy datasets
all_candy <- bind_rows(tidy_2015, tidy_2016, tidy_2017)


# final wrangling - countries
# 
# declare country-specific patterns
USA_patterns <- "u[ .]*s|ica|state|trump|ates|d s|the yoo|new york|california|pittsburgh|north carolina|new jersey|amerca|murrika|alaska"
UK_patterns <- "u[.]*k|in[g]*[d]*om|england|scotland"
Canada_patterns <- "canada"

# convert countries to lower case for easier wrangling
all_candy_low_country <- all_candy %>%
  mutate(country = str_to_lower(country))

# collate countries that fit patterns
all_candy_tidy <- all_candy_low_country %>% 
  mutate(country = case_when(
    str_detect(country, USA_patterns) ~ "USA",
    str_detect(country, UK_patterns) ~ "UK",
    str_detect(country, Canada_patterns) ~ "canada",
    TRUE ~ country))

countries <- "CANADA|USA|UK"

# make sure the desired list of countries has entries
# countries %>% 
#  verify(!countries == "")

# convert countries to upper case, only allow ages within a certain range
# in some case age has been entered into the country column, extract where there
# is no information for age but there is a number in the country column
all_candy_tidy <- all_candy_tidy %>% 
  mutate(country = str_to_upper(country)) %>% 
  mutate(age = case_when(
    is.na(age) ~ as.integer(str_extract(country, "[0-9]+")),
    TRUE ~ as.integer(age))) %>% 
  mutate(country = as.character(ifelse(str_detect(country,"[0-9]{2}"), NA, country))) %>% 
  mutate(age = ifelse((age > 111 | age < 6), NA, age)) %>%
  mutate(country = ifelse(str_detect(country, countries), country, "OTHER"))


# check that data is in the desired form and
# save the big and clean and tidy dataset for analysis
all_candy_tidy %>%
  verify(is.na(country) | !str_detect(country, "[a-z]")) %>% 
  verify((age >= 6 & age <= 111) | is.na(age)) %>% 
  write_csv("data/clean_data/all_candy.csv")


