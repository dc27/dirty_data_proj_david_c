# load in the tidyverse
library(tidyverse)

# read in the dirty data
right_wing_data <- read_csv("data/raw_data/rwa.csv")

make_survey_data_clean <- function(dirty_data) {

  # 1: longer data
  # add an ID column for each survey participant
  # make the data long by giving a row to each question in the survey
  long_dirty_data <- dirty_data %>%
    rowid_to_column("participant_no") %>% 
    pivot_longer(cols = starts_with("Q"),
                names_to = "survey_question",
                values_to = "response")
  
  # 2: calculate the average score
  # 2.1: take a subset with only the necessary cols
  subset_long <- long_dirty_data %>% 
    select(-E1:-E22,
           -TIPI1:-TIPI10,
           -VCL1:-VCL16) %>% 
    select(participant_no, survey_question, response, introelapse:major)
  
  # 2.2 some questions are scored in reverse. Before the average can be
  # calculated they have to be changed to the right form. This is done by a
  # CASE_WHEN expression
  correct_responses_subset_long <- subset_long %>% 
    mutate(survey_question = as.numeric(str_remove(survey_question, "Q"))) %>% 
    mutate(response = case_when(
      survey_question == 4|6|8|9|11|13|15|18|20|21 ~ 10 - response,
      TRUE ~ response
    ))
  
  # 2.3 after filtering out the first two questions because they were warm up
  # questions, the average for each participant can be calculated. The
  # question and response columns are dropped from the selection since they are
  # no longer needed, and duplicate rows are removed.
  # each participant should be left with only one row
  dirty_data_with_avg <- correct_responses_subset_long %>% 
    group_by(participant_no) %>% 
    filter(survey_question > 2) %>% 
    mutate(avg_rwa = mean(response)) %>% 
    select(-survey_question, -response) %>% 
    unique()
  
  # 3. recode values and clean up output
  # some of the values correspond to text values. The brief asks for these to
  # be inserted into the tibble. 
  education_levels <- c("less than high school",
                        "high school",
                        "university degree",
                        "graduate degree")
  
  genders <- c("male", "female", "other")
  
  district_types <- c("rural","suburban","urban")
  
  native_eng_options <- c("yes", "no")
  
  handedness <- c("right", "left", "both")
  
  religions <- c("agnostic", "atheist", "buddhist",
                 "christian (catholic)",
                 "christian (mormon)",
                 "christian (protestant)",
                 "christian (other)" ,
                 "hindu", "jewish","muslim",
                 "sikh", "other")
  
  sex_prefs <- c("heterosexual",
                 "bisexual",
                 "homosexual",
                 "asexual",
                 "other")
  
  races <- c("asian", "arab", "black",
             "white or indigenous australian or native american",
             "other")
  
  vote_options <- c("yes", "no")
  
  marital_statuses <- c("never married",
                        "currently married",
                        "previously married")
  
  recoded_cols <- dirty_data_with_avg %>%
    mutate(gender = case_when(
      gender == 1 ~ genders[1],
      gender == 2 ~ genders[2],
      gender == 3 ~ genders[3]
    ), education = case_when(
      education == 1 ~ education_levels[1],
      education == 2 ~ education_levels[2],
      education == 3 ~ education_levels[3],
      education == 4 ~ education_levels[4],
      education == 5 ~ education_levels[5]
    ), urban = case_when(
      urban == 1 ~ district_types[1],
      urban == 2 ~ district_types[2],
      urban == 3 ~ district_types[3]
    ), engnat = case_when(
      engnat == 1 ~ native_eng_options[1],
      engnat == 2 ~ native_eng_options[2]
    ), hand = case_when(
      hand == 1 ~ handedness[1],
      hand == 2 ~ handedness[2],
      hand == 3 ~ handedness[3]
    ), religion = case_when(
      religion == 1 ~ religions[1],
      religion == 2 ~ religions[2],
      religion == 3 ~ religions[3],
      religion == 4 ~ religions[4],
      religion == 5 ~ religions[5],
      religion == 6 ~ religions[6],
      religion == 7 ~ religions[7],
      religion == 8 ~ religions[8],
      religion == 9 ~ religions[9],
      religion == 10 ~ religions[10],
      religion == 11 ~ religions[11],
      religion == 12 ~ religions[12]
    ), orientation = case_when(
      orientation == 1 ~ sex_prefs[1],
      orientation == 2 ~ sex_prefs[2],
      orientation == 3 ~ sex_prefs[3],
      orientation == 4 ~ sex_prefs[4],
      orientation == 5 ~ sex_prefs[5]
    ), race = case_when(
      race == 1 ~ races[1],
      race == 2 ~ races[2],
      race == 3 ~ races[3],
      race == 4 ~ races[4],
      race == 5 ~ races[5]
    ), voted = case_when(
      voted == 1 ~ vote_options[1],
      voted == 2 ~ vote_options[2]
    ), married = case_when(
      married == 1 ~ marital_statuses[1],
      married == 2 ~ marital_statuses[2],
      married == 3 ~ marital_statuses[3]
    ))
  
  # some of the columns need better names, drop unused columns, and re-order
  # them so that the average is immediately visible
  clean_output_and_average <- recoded_cols %>% 
    select(-screenw, -screenh) %>%
    select(participant_no, avg_rwa, introelapse:major) %>% 
    rename(sexual_preference = orientation,
           education_level = education,
           handedness = hand,
           english_native = engnat,
           urban_classification = urban,
           country_ip = IP_country,
           intro_elapse_t = introelapse,
           test_elapse_t = testelapse,
           survey_elapse_t = surveyelapse,
           family_size = familysize,
           survey_accurate = surveyaccurate) %>% 
    mutate(major = str_to_lower(major),
           survey_accurate = as.logical(survey_accurate)) %>% 
    filter(survey_accurate == TRUE)
  
  clean_output_and_average %>% 
    write_csv("data/clean_data/clean_rwa.csv")
}

make_survey_data_clean(right_wing_data)