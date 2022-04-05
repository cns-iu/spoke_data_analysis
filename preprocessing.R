library(tidyverse)
library(qualtRics)
library(lubridate)

source("qualtrics_auth.R")

# loading the survey data
survey_id = 'SV_6sMimcMUAjk8BEO'

raw <- fetch_survey(surveyID = survey_id, 
                         verbose = TRUE, 
                         save_dir = "data", 
                         force_request = TRUE)

raw %>% view()

# ingest answer key

raw$Q6.14_15 == "Oil palm"

answer_key = read_csv("data/spoke_answer_key.csv")
answer_key

# evaluate answers


# multiple-choice (single-answer)

raw$Q6.2
raw$Q6.6
raw$Q6.18
raw$Q6.22
raw$Q6.30


# multiple-choice (multiple answers)
raw$Q6.10_1
raw$Q6.10_2
raw$Q6.10_3
raw$Q6.10_4
raw$Q6.10_5

raw$Q6.14_1
raw$Q6.14_2
raw$Q6.14_3
raw$Q6.14_4
raw$Q6.14_5
raw$Q6.14_6

raw$Q6.26_1
raw$Q6.26_2
raw$Q6.26_3




raw$Q6.31


# time
as_datetime(raw$Q6.28/1000, tz = "EST")

study = read_csv("data/SPOKE_prestudy_data/bq-results-20220405-154941-1649178520922.csv")
as_datetime(study$user_first_touch_timestamp/1000/1000, tz = "EST")[1]
as_datetime(study$event_timestamp/1000/1000, tz = "EST") %>% unique() %>% sort()
study$event_timestamp/1000000 %>% unique() %>% sort()



as.character(study$user_pseudo_id) ==  "1179371801.1647741692"


study %>% view()
