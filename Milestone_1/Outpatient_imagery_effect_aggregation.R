#outpatient_imagery_effect.csv
#Kazunori kasahara
#2018/03/01

library(tidyverse)
outpatient_imagery_effect <- read.csv("data/raw/hospital_level_data/outpatient_imagery_effect.csv",stringsAsFactors = FALSE)

# Create the table decribing the measures
unique.imagery_effect.details <- outpatient_imagery_effect %>% select(Measure.ID, Measure.Name) %>% distinct()
write_csv(unique.imagery_effect.details,"data_to_join/coltable_imagery")

# Count how many hospitals are avaiable in the data
number_hospital <- length(unique(outpatient_imagery_effect$Provider.ID)) # 4806

# Select the column we need
# The data includes some separate observations with the common hospital names and so we need provider.IDs
# as well as hospital names 

outpatient_imagery_effect_summary <- outpatient_imagery_effect %>% select(Hospital.Name,Provider.ID,Measure.ID,Score)

# Make the data frame wide

outpatient_imagery_effect_summary <- outpatient_imagery_effect_summary %>% spread(Measure.ID,Score)

# Save the cleaned data frame
write.csv(outpatient_imagery_effect_summary, "data/joined_data/outpatient_imagery_effect_summary.csv", row.names = FALSE)
