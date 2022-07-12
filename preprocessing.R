library(tidyverse)
library(dplyr)
library(qualtRics)
library(lubridate)
library(ggstatsplot)
library(rjson)

source("qualtrics_auth.R")

# loading the survey data
survey_id = 'SV_6sMimcMUAjk8BEO'

raw <- fetch_survey(surveyID = survey_id, 
                         verbose = TRUE, 
                         save_dir = "data", 
                         force_request = TRUE)

# load answer key

answer_key = read_csv("data/spoke_answer_key.csv")
answer_key

# delete unneeded columns

fewer_cols = raw %>% 
  select(-c("StartDate", "EndDate","Status", "IPAddress","RecipientFirstName","RecipientLastName","RecipientEmail","ExternalReference","DistributionChannel","UserLanguage","Q_RelevantIDDuplicate","Q_RelevantIDDuplicateScore",
            "LocationLatitude","LocationLongitude","Q_RecaptchaScore","Q_RelevantIDFraudScore","Q_RelevantIDLastStartDate","Q_BallotBoxStuffing"))
fewer_cols

# rename columns

renamed = fewer_cols %>% 
  rename(
    Duration = "Duration (in seconds)"
  )
renamed

# extract relevant users

pilot = renamed %>% 
  filter( 
    !is.na(Q5.5)
  ) %>% 
  filter(
    Progress == 100
  )
pilot


