library(tidyverse)
library(readxl)

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


# define functions
make_regexpr <- function(char_vector) {
  # function takes a vector of multiple patterns and returns one
  # concatenated character string suitable for regex operations
  return(paste0(char_vector, collapse = "|"))
}


pivot_and_shrink_df <- function(
  df, pivot_column_contents, columns_to_keep,
  new_col_names = c("measure_type", "value")
) {
  require(dplyr)
  # function to convert df from wide to long format and remove columns
  # pivot_column_contents allows for specification of values which after being
  # provided can be used to search through all the columns and only pivot
  # those containing these values. columns_to_keep specifies columns which
  # should NOT be removed from the df. new_col_names are the names given to
  # the pivot_longer operation and allow the user to specify the names of the 
  # new columns
  
  
  cols_to_keep_re <- make_regexpr(columns_to_keep)
  
  df %>% 
    select(
      where(~any(. %in% pivot_column_contents)),
      matches(cols_to_keep_re)
    ) %>% 
    pivot_longer(
      where(~any(. %in% pivot_column_contents)),
      names_to = new_col_names[1],
      values_to = new_col_names[2]
    )
}

# columns in the data that are required for analysis
keep <- c(
  "country",
  "how old",
  " age",
  "gender",
  "sex",
  "candy",
  "rating",
  "treating", 
  "going out"
)

# some columns contain "JOY", "DESPAIR", or "MEH". These are the ratings for
# different candies. columns containing these values should be pivoted to long
# format
pivot_col_contents <- c("JOY", "DESPAIR", "MEH")


long_candy_data <- sapply(
  candy_data, pivot_and_shrink_df, pivot_column_contents = pivot_col_contents,
  columns_to_keep = keep, new_col_names = c("candy", "rating")
)

# coerce columns for all datasets before binding
ordered_candy <- sapply(
  long_candy_data, select,
  age = matches("How old are you|Q3: AGE"),
  sex = matches("gender|sex"),
  trick_treating = matches("going"),
  country = matches("country"),
  candy,
  rating
)

# bind separate datasets into single one
all_candy <- bind_rows(ordered_candy, .id = "year")

clean_age <- all_candy %>% 
  relocate(country, .after = age) %>%
  relocate(sex, .after = age) %>% 
  # cleaning the age column. sometiems the age has been entered in the country
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

clean_candy <- clean_country %>%
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