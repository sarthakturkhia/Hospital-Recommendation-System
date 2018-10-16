## disease_complication_aggregation.R
## AAMILLER 2/26/18
## Prepares data from hospital data sets on complications from common medical procedures
## for the final joined data table with each hospital name. 

library(dplyr)
library(stringr)
library(tidyr)
library(reshape)

complications.data <- read.csv("./../../data/raw/hospital_level_data/complications_deathsHospital.csv", stringsAsFactors = FALSE) %>%
  filter(Higher.Estimate != "Not Available")

# Create tables with measure IDs and name of measure for easy reference
unique.complications.details <- complications.data %>% select(Measure.ID, Measure.Name) %>% distinct()

# Get counts of how many hospitals represented in data with available data by finding number of unique hospital names
complications.hospitals <- unique(complications.data$Provider.ID) # 4357

# Calculate a score for each infection for each hospital that represents how good they are at
# dealing with that type of infection.

# Resource on how the scores are gotten and who is in the study: 
# https://www.medicare.gov/hospitalcompare/Data/Surgical-Complications-Hip-Knee.html
# Standard error calculated using the 95% confidence interval that can be determined from Higher.Estimate and the score, an average
# Since we know the confidence intervals are 95%, we can then use them to calculate the standard error of the measure even though it is not provided.
complications.summary <- complications.data %>% mutate("Standard.error" = (as.numeric(Higher.Estimate) - as.numeric(Score))/1.96) %>% 
  select(Hospital.Name, Provider.ID, Measure.ID, Measure.Name, Score, Standard.error) 

# Categorize into data to support recommendations when suggesting where patients should go based on reason for visit by
# getting all of the complications with key words that identify each measure uniquely to avoid having to select by individual
# measure name or ID.

# Stroke death rate score by hospital
stroke.death.rates <- complications.summary[str_detect(complications.summary$Measure.Name, "stroke") |
                                              str_detect(complications.summary$Measure.Name, "Stroke"),]
# Complication rates for hip/knee replacements
knee.hip.replacement.complication.rates <- complications.summary[str_detect(complications.summary$Measure.Name, "hip/knee replacement"),]

# Coronary artery bypass surgery (CABG) death rates
CABG.complication.rates <- complications.summary[str_detect(complications.summary$Measure.Name, "CABG"),]

# Postoperative Acute Kidney Injury Requiring Dialysis Rate
po.acute.kidney.injury.requiring.dialysis.rates <- complications.summary[str_detect(complications.summary$Measure.Name, "Acute Kidney"),]

# Serious blood clots after surgery
serious.blood.clot.rates <- complications.summary[str_detect(complications.summary$Measure.Name, "blood clots after surgery"),]

# A wound that splits open  after surgery on the abdomen or pelvis - can use for abdominal or pelvis surgery
wound.split.abdomen.pelvis.rates <- complications.summary[str_detect(complications.summary$Measure.Name, "splits open"),]

# Deaths among Patients with Serious Treatable Complications after Surgery
death.serious.treatable.complications.rates <- complications.summary[str_detect(complications.summary$Measure.Name, "Serious Treatable Complications"),]

# Collapsed lung due to medical treatment
collapsed.lung.rates <- complications.summary[str_detect(complications.summary$Measure.Name, "Collapsed lung"),]

# "Serious complications" post-surgery
serious.complications.rate <- complications.summary[str_detect(complications.summary$Measure.Name, "Serious complications"),]

# Change data from long to wide and join it together, converting it so each row has a hospital name
# Make a column in the data set for every measure.ID we want
complication.ids <- c("MORT_30_STK", "COMP_HIP_KNEE", "MORT_30_CABG", "PSI_10_POST_KIDNEY", "PSI_12_POSTOP_PULMEMB_DVT",
                      "PSI_14_POSTOP_DEHIS", "PSI_4_SURG_COMP", "PSI_6_IAT_PTX", "PSI_90_SAFETY")
final.complication.summary.df <- data.frame(complications.hospitals)

# Add column for stanard error and measure score
for(i in complication.ids) {
  final.complication.summary.df[,i] <- NA
  final.complication.summary.df[,paste0(i, "_Standard.Error")] <- NA
}

PopulateTable <- function(data, old.data) {
  for(row in 1:length(data$Provider.ID)) {
    old.data[old.data$complications.hospitals == data$Provider.ID[row], data$Measure.ID[row]] <- data$Score[row]
    old.data[old.data$complications.hospitals == data$Provider.ID[row], paste0(data$Measure.ID[row], "_Standard.Error")] <- data$Standard.error[row]
  }
  return(old.data)
}

# Add each table to the final data frame... ick
final.complication.summary.df <- PopulateTable(stroke.death.rates, final.complication.summary.df)
final.complication.summary.df <- PopulateTable(knee.hip.replacement.complication.rates, final.complication.summary.df)
final.complication.summary.df <- PopulateTable(CABG.complication.rates, final.complication.summary.df)
final.complication.summary.df <- PopulateTable(po.acute.kidney.injury.requiring.dialysis.rates, final.complication.summary.df)
final.complication.summary.df <- PopulateTable(serious.blood.clot.rates, final.complication.summary.df)
final.complication.summary.df <- PopulateTable(wound.split.abdomen.pelvis.rates, final.complication.summary.df)
final.complication.summary.df <- PopulateTable(death.serious.treatable.complications.rates, final.complication.summary.df)
final.complication.summary.df <- PopulateTable(collapsed.lung.rates, final.complication.summary.df)
final.complication.summary.df <- PopulateTable(serious.complications.rate, final.complication.summary.df)

# final.complication.summary.df now contains standard error measure for each complication, complication score, and hospital name
write.csv(final.complication.summary.df, "./../../data_to_join/complication_summary.csv", row.names = FALSE)
write.csv(unique.complications.details, "complication_summary_Measureid_measurename.csv", row.names = FALSE)

