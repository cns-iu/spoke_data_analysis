library(tidyverse)
library(qualtRics)
library(lubridate)

source("qualtrics_auth.R")

survey_id = 'SV_6sMimcMUAjk8BEO'

raw <- fetch_survey(surveyID = survey_id, 
                         verbose = TRUE, 
                         save_dir = "data", 
                         force_request = TRUE)



