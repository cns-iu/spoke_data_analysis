source("preprocessing.r")

# analyze duration

ggplot(pilot, aes(x = ResponseId,y = Duration, fill = pilot$Q2.5_6))+
  geom_bar(stat = "identity")


# evaluate answers

# assign scores based on answer key

# multiple-choice (single-answer)

# pilot$Q6.2
# pilot$Q6.6

pilot = pilot %>% mutate(
 Q6.2_score = (as.character(answer_key[1,2]) == as.character(pilot$Q6.2)),
 Q6.6_score = (as.character(answer_key[2,2]) == as.character(pilot$Q6.6))
)

# multiple-choice (multiple answers)
# 
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
  total_score = sum(c_across(Q6.2_score:Q6.30_score))
  )

pilot$total_score %>% hist()

# time
raw$Q6.31
as_datetime(raw$Q6.28/1000, tz = "EST")

study = read_csv("data/SPOKE_prestudy_data/bq-results-20220405-154941-1649178520922.csv")
as_datetime(study$user_first_touch_timestamp/1000/1000, tz = "EST")[1]
as_datetime(study$event_timestamp/1000/1000, tz = "EST") %>% unique() %>% sort()
study$event_timestamp/1000000 %>% unique() %>% sort()



as.character(study$user_pseudo_id) ==  "1179371801.1647741692"


study %>% view()
