## infections_complication_aggregation.R
## AAMILLER 2/26/18
## Prepares data from hospital data sets on infections from common medical procedures
## for the final joined data table with each hospital name. 

library(dplyr)
library(stringr)
library(tidyr)
library(reshape)

infections.data <- read.csv("./../../data/raw/hospital_level_data/heathcare_associated_infections.csv", stringsAsFactors = FALSE) %>% filter(Score != "Not Available")

measures <- unique(infections.data$Measure.Name)
measure.ids <- unique(infections.data$Measure.ID)

# All central line-associated bloodstream infection (CLABSI) infection data
CLABSI.related <- infections.data[str_detect(infections.data$Measure.Name, "CLABSI"),]

# All surgical site infection data
SSI.related <- infections.data[str_detect(infections.data$Measure.Name, "SSI"),]

# All MRSA infection data
MRSA.related <- infections.data[str_detect(infections.data$Measure.Name, "MRSA"),]


complication.ids <- c("HAI_1_CI_LOWER", "HAI_1_CI_UPPER", "HAI_3_CI_LOWER", "HAI_3_CI_UPPER",
                      "HAI_5_CI_LOWER", "HAI_5_CI_UPPER")

providers <- unique(infections.data$Provider.ID)
final.infection.complication.summary.df <- data.frame(providers)

# Add column for stanard error and measure score
for(i in complication.ids) {
  final.infection.complication.summary.df[,i] <- NA
}

PopulateTable <- function(data, old.data) {
  for(row in 1:length(data$Provider.ID)) {
    old.data[old.data$providers == data$Provider.ID[row], data$Measure.ID[row]] <- data$Score[row]
  }
  return(old.data)
}

final.infection.complication.summary.df <- PopulateTable(CLABSI.related, final.infection.complication.summary.df)
final.infection.complication.summary.df <- PopulateTable(SSI.related, final.infection.complication.summary.df)
final.infection.complication.summary.df <- PopulateTable(MRSA.related, final.infection.complication.summary.df)

final.infection.complication.summary.df <- final.infection.complication.summary.df %>% select(providers, complication.ids)

write.csv(final.infection.complication.summary.df, "./../../data_to_join/infection_summary.csv", row.names = FALSE)

# Make a data table of all of the measures we are using and what they mean
unique.infection.complications.details <- infections.data %>% filter(Measure.ID == complication.ids) %>% select(Measure.ID, Measure.Name) %>% distinct()
write.csv(unique.infection.complications.details, "./../../data_to_join/infection_summary_Measureid_measurename.csv", row.names = FALSE)

