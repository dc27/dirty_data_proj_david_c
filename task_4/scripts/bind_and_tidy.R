library(tidyverse)

dataset_2015 <- read_csv("data/clean_data/candy_2015.csv")
dataset_2016 <- read_csv("data/clean_data/candy_2016.csv")
dataset_2017 <- read_csv("data/clean_data/candy_2017.csv")

make_data_long <- function (wide_data) {
  long_data <- wide_data %>% 
    pivot_longer(-year & -going_out & -gender & -age & -country,
                 names_to = "candy",
                 values_to = "reaction") %>%
    select(year, age, gender, country, going_out, candy, reaction)
}

remove_empty_rows <- function(data) {
  filtered_data <- data %>% 
    filter(!(is.na(going_out) &
               is.na(gender) &
               is.na(age) &
               is.na(country) &
               is.na(reaction)))
}

long_2015 <- make_data_long(dataset_2015)
long_2016 <- make_data_long(dataset_2016)
long_2017 <- make_data_long(dataset_2017)

filtered_long_2015 <- remove_empty_rows(long_2015)
filtered_long_2016 <- remove_empty_rows(long_2016)
filtered_long_2017 <- remove_empty_rows(long_2017)

all_candy <- bind_rows(filtered_long_2015,
                       filtered_long_2016,
                       filtered_long_2017)


USA_patterns <- "u[ .]*s|ica|state|trump|ates|d s|the yoo|new york|california|pittsburgh|north carolina|new jersey|amerca|murrika|alaska"
UK_patterns <- "u[.]*k|in[g]*[d]*om|england|scotland"
spain_patterns <- "spain|espa"
Canada_patterns <- "canada"
netherlands_patterns <- "netherl"
countries <- "CANADA|USA|UK"

all_candy_low_country <- all_candy %>%
  mutate(country = str_to_lower(country))

all_candy_tidy <- all_candy_low_country %>% 
  mutate(country = case_when(
    str_detect(country, USA_patterns) ~ "USA",
    str_detect(country, UK_patterns) ~ "UK",
    str_detect(country, spain_patterns) ~ "spain",
    str_detect(country, Canada_patterns) ~ "canada",
    str_detect(country, netherlands_patterns) ~ "the netherlands",
    TRUE ~ country)) %>% 
  mutate(country = str_to_upper(country)) %>% 
  mutate(age = case_when(
    is.na(age) ~ as.integer(country),
    TRUE ~ as.integer(age))) %>% 
  mutate(country = as.character(ifelse(str_detect(country,"[0-9]{2}"), NA, country))) %>% 
  mutate(age = ifelse(age > 111 | age < 6, NA, age)) %>% 
  mutate(country = ifelse(str_detect(country, countries), country, "OTHER"))

all_candy_tidy %>%
  write_csv("data/clean_data/all_candy.csv")

