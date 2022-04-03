# define functions
add_id_column <- function(df, year) {
  # function to add id column to a df using the year of collection and the
  # row_number
  df %>% 
    mutate(
      id = paste(
        year,
        str_pad(as.character(row_number()), width = 5, "left", "0"),
        sep = "_"),
      .before = 1
    )
}

make_regexpr <- function(char_vector) {
  # function takes a vector of multiple patterns and returns one
  # concatenated character string suitable for regex operations
  return(paste0(char_vector, collapse = "|"))
}

pivot_df <- function(
  df, pivot_column_contents, new_col_names = c("measure_type", "value")
) {
  # function to convert df from wide to long format, based on given search
  # values contents common
  
  df %>% 
    pivot_longer(
      where(~any(. %in% pivot_column_contents)),
      names_to = new_col_names[1],
      values_to = new_col_names[2]
    )
}