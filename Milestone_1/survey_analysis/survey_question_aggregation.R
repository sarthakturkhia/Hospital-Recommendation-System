# Project Milestone 1 - "Person 2"

# Look at the survey questions and decide which ones are most valuable for informing care and quality
# Read the documentation to understand the scores as they relate to different things they are associated with. 
# Make a short report about these that can be used in our final analysis page.

library(dplyr)
library(stringr)
library(glue)

# Read in survey data
survey.data <- read.csv("./../../data/raw/hospital_level_data/HOSPITAL_QUARTERLY_QUALITYMEASURE_PCH_HCAHPS_HOSPITAL.csv", stringsAsFactors = FALSE)
survey.data$HCAHPS.Question <- trim(survey.data$HCAHPS.Question)
# Only 11 hospitals giving the survey
hospitals.administering <- unique(survey.data$Hospital.Name)
states <- unique(survey.data$State)

all.questions <- unique(survey.data$HCAHPS.Question)

overall.star.ratings <- all.questions[str_detect(all.questions, "star")]

# Reports about how often room and bathroom were clean
cleanliness.questions <- all.questions[1:5]

# "Patients who reported that their doctors" sometimes/never/always/usually communicated well, 3 qs
doctor.communication.related.questions <- all.questions[str_detect(all.questions, "communicated")
                                                        & str_detect(all.questions, "doctor")]

# "Patients who reported that their nurses" sometimes/never/always/usually communicated well, 3 qs
nurse.communication.related.questions <- all.questions[str_detect(all.questions, "communicated")
                                                       & str_detect(all.questions, "nurses")]

# 5 qs - "Patients who reported that their pain was" always / sometimes or never / usually controlled
pain.management.related.questions <- all.questions[str_detect(all.questions, "pain")
                                                       | str_detect(all.questions, "Pain")]

# 2 qs - dischareg summary data, questionable utility
discharge.related.questions <- all.questions[str_detect(all.questions, "discharge")
                                                   | str_detect(all.questions, "Discharge")]

# 5qs - report Yes definitely, yes/no probably (No definitive "No" option for not recommending, lumped in with "probably")
recommend.hospital.related.questions <- all.questions[str_detect(all.questions, "recommend")
                                                        | str_detect(all.questions, "Recommend")]
# 2qs - Summary data only
staff.responsiveness.related.questions <- all.questions[str_detect(all.questions, "responsiveness")]

# 5qs - whether or not medications were explained to patients before they received them
medicine.communication.related.questions <- all.questions[str_detect(all.questions, "medicines")]

# 2qs - summary data only
care.transition.related.questions <- all.questions[str_detect(all.questions, "transition")]

# 5qs - reported are around room was always / sometimes/never / usually quiet at night
quietness.related.questions <- all.questions[str_detect(all.questions, "quiet") | str_detect(all.questions, "Quietness")]

# Format questions for .Rmd output
format.questions <- function(question.vector) {
  return(collapse(question.vector, sep = ", <br>", last = " and "))
}

# Make summary table for questions broken up by percentages
# Break down into summary stats so we can see if there is a significant range such that
# there 
make.summary.table.percents <- function(question.vector) {
  output.table <- survey.data %>% filter(HCAHPS.Question %in% as.vector(question.vector)) %>%
    group_by(HCAHPS.Question) %>%
    mutate("Max.Percent" = max(HCAHPS.Answer.Percent),
           "Min.Percent" = min(HCAHPS.Answer.Percent),
           "Average.Percent" = mean(HCAHPS.Answer.Percent),
           "Median.Percent" = median(HCAHPS.Answer.Percent)) %>%
    select(HCAHPS.Question, 
           Max.Percent, 
           Min.Percent, 
           Average.Percent, 
           Median.Percent) %>%
    filter(!is.na(Max.Percent)) %>% # Filter out NAs, filtering out linear mean and overall rows
    mutate("Range" = Max.Percent - Min.Percent)
  return(output.table[1:3,])
}


# Conclusions:
# Since only 11 hospitals are given the survey, data may be hard use to recommend hospitals across
# the US


# Populate and output a table with summary stats around each question that can be joined on tables with Provider ID
# Include # of survey responders in table and response rate for error estimation
output.table <- survey.data %>% select(Provider.ID, Survey.Response.Rate.Percent, Number.of.Completed.Surveys) %>% distinct()

# Since only 11 hospitals are administering this survey, the data is not feasible to use.
