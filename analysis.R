source("preprocessing.r")

# set global vars

total_num_tasks = 8

# RQ1

# Load interaction data

# get overview of times and scores for all pilot user
pilot %>% select(ResponseId, RecordedDate,total_score_norm, Duration)

# investigate timingstimeline for qualtrics-> google cloud
google = read_csv("data/SPOKE_prestudy_data/R_1rp0VM9Y2HLSSWk.csv")
google$event_params


# RQ2
# evaluate answers
# assign scores based on answer key

# single answer
# pilot$Q6.2
# pilot$Q6.6

pilot = pilot %>% mutate(
 Q6.2_score = (as.character(answer_key[1,2]) == as.character(pilot$Q6.2)),
 Q6.6_score = (as.character(answer_key[2,2]) == as.character(pilot$Q6.6))
)

# multiple answers
# pilot$Q6.10_1
# pilot$Q6.10_2
# pilot$Q6.10_3
# pilot$Q6.10_4
# pilot$Q6.10_5
pilot = pilot %>% mutate(
  Q6.10_score = ((gsub("\n", "", as.character(answer_key[3,2]),",", fixed = TRUE) == paste(as.character(pilot$Q6.10_1), as.character(pilot$Q6.10_5), sep=",")))
)

# pilot$Q6.14_1
# pilot$Q6.14_2
# pilot$Q6.14_3
# pilot$Q6.14_4
# pilot$Q6.14_5
# pilot$Q6.14_6

pilot = pilot %>% mutate(
  Q6.14_score = ((gsub("\n", "", as.character(answer_key[4,2]),",", fixed = TRUE) == paste(as.character(pilot$Q6.14_2), as.character(pilot$Q6.14_3), as.character(pilot$Q6.14_5), sep=",")))
)

# pilot$Q6.18
# pilot$Q6.22
pilot = pilot %>% mutate(
  Q6.18_score = (as.character(answer_key[5,2]) == as.character(pilot$Q6.18)),
  Q6.22_score = (as.character(answer_key[6,2]) == as.character(pilot$Q6.22))
)

# pilot$Q6.26_1
# pilot$Q6.26_2
# pilot$Q6.26_3
pilot = pilot %>% mutate(
  Q6.26_score = ((gsub("\n", "", as.character(answer_key[7,2]),",", fixed = TRUE) == paste(as.character(pilot$Q6.26_2), as.character(pilot$Q6.26_3), sep=",")))
)

# pilot$Q6.30

pilot = pilot %>% mutate(
  Q6.30_score = (as.character(answer_key[8,2]) == as.character(pilot$Q6.30))
)

pilot = pilot %>% rowwise() %>% mutate(
  total_score_abs = sum(c_across(Q6.2_score:Q6.30_score)),
  total_score_norm = total_score_abs/total_num_tasks
  )

pilot$total_score_abs
pilot$total_score_abs %>% summary()
pilot$total_score_abs %>% hist()

# general descriptive stats

# analyze durations
ggplot(pilot, aes(x = ResponseId, y = Duration/60, fill=total_score_norm))+
  geom_bar(stat = "identity") +
  geom_hline(yintercept = median(pilot$Duration/60, linetype="fsdf"), color="red")+
  labs(x = "Subject ID", y="Duration [minutes]", fill ="Total score (normalized)")+
  theme(
    legend.position = c(.95, .95),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6)
  )

pilot$Duration %>% summary()/60

# correlation between study duration and score
cor.test(pilot$Duration, pilot$total_score_norm)


# calculate times between for exploration

pilot$`Q5.1_First Click`
pilot$`Q5.1_Last Click`
pilot$`Q5.1_Click Count`
pilot$`Q5.1_Page Submit`
pilot$Q5.3
pilot$Q5.4
pilot$Q5.5

pilot$Q5.4
pilot$`Q5.1_Page Submit` %>% mean()/60

# time spent on documentation part in Qualtrics
pilot$`Q5.1_Page Submit` %>% summary()/60

# load google data
# as_datetime(raw$Q6.28/1000, tz = "EST")
# as_datetime(study$user_first_touch_timestamp/1000/1000, tz = "EST")[1]
# as_datetime(study$event_timestamp/1000/1000, tz = "EST") %>% unique() %>% sort()
# study$event_timestamp/1000000 %>% unique() %>% sort()

study = read_csv("data/SPOKE_prestudy_data/all_events.csv")

# check if subjects R_3nwax0EqOYAGxo4 and R_2rAyEiPIZPyxG4Y were recorded on Google but without responseId

# analyze if we can make sense of google data for two subjects
# filter data for only subjects R_3xYcNPgDwiQAC2d and R_3s1ANePRmPSafL2

g_check = study %>% filter(
  grepl('R_3s1ANePRmPSafL2', page_location, fixed = TRUE)
)
g_check

# replace user pseudo id with response id


