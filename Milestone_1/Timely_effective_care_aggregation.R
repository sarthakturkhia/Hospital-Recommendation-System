# timely_effective_care_aggregation.R
# Norry
# 2018/03/01

library(tidyverse)
timely_effective_care <- read.csv("data/raw/hospital_level_data/timely_effective_care.csv",stringsAsFactors = FALSE)

# Create the table decribing the measures
unique.effectivecare.details <- timely_effective_care %>% select(Measure.ID, Measure.Name) %>% distinct()
write_csv(unique.effectivecare.details,"data_to_join/coltable_effectivecare")

# Count how many hospitals are avaiable in the data
number_hospital <- length(unique(timely_effective_care$Provider.ID)) # 4806

# Select the column we need
# The data includes some separate observations with the common hospital names and so we need provider.IDs
# as well as hospital names 

timely_effective_care_summary <- timely_effective_care %>% select(Hospital.Name,Provider.ID,Measure.ID,Score)

# Make the data frame wide

timely_effective_care_summary <- timely_effective_care_summary %>% spread(Measure.ID,Score)

# Save the cleaned data frame
write.csv(timely_effective_care_summary, "data/joined_data/timely_effective_care_summary.csv", row.names = FALSE)
