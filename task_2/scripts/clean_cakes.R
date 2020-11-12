# load in the tidyverse
library(tidyverse)
library(assertr)

# there are two datasets
# one has cakes and a list of ingredient codes
# the other has ingredients and their codes
cake_data <- read_csv("data/raw_data/cake-ingredients-1961.csv")
cake_ingredient_codes <- read_csv("data/raw_data/cake_ingredient_code.csv")

# create cleaning function
make_cake_data_tidy <- function (data, info_tibble) {
  
  # 1.make the data long and add the names of ingredients by joining
  long_data <- data %>% 
    pivot_longer(-Cake,
                 names_to = "ingredient_code",
                 values_to = "quantity") %>% 
    rename(cake = Cake) %>%
    left_join(info_tibble,
              by = c("ingredient_code" = "code"))
  
  # 2.take a subset of the long_data and flush missing values
  subset_long_data <- long_data %>% 
    select(cake, ingredient, measure, quantity) %>% 
    filter(!is.na(quantity)) %>% 
    mutate(ingredient = str_to_lower(ingredient))
  
  # 3.introduce standardisation for analysis
  with_standard_measures <- subset_long_data %>% 
    mutate(liquid_quantity = case_when(
      measure == "cup" ~ 48 * quantity,
      measure == "tablespoon" ~ 16 * quantity,
      measure == "teaspoon" ~ 1*quantity)) %>% 
    mutate(solid_quantity = case_when(
      measure == "once" ~ 1 * quantity,
      measure == "pound" ~ 16 * quantity,
      measure == "quart" ~ 32 * quantity
    ))
  
  # 4. verify and write clean data
  with_standard_measures %>%
    verify(ncol(with_standard_measures) == 6) %>%
    verify(mode(liquid_quantity) == "numeric") %>% 
    verify(mode(solid_quantity) == "numeric") %>%
    write_csv("data/clean_data/clean_cake_data.csv")

}

make_cake_data_tidy(cake_data, cake_ingredient_codes)