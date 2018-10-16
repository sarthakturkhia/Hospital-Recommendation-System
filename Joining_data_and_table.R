## Merge data to one data frame & Make the one table to explain the column
## Kazunori Kasahara

library(tidyverse)

## Merge data to one data frame
# Import data to join
filelist <- list.files("data_to_join")[c(-1,-2,-3,-5,-7,-10,-12)] # exclude the data about mean and median and the table about column
for (i in seq_along(filelist)){
file <- filelist[i]
path <- paste0("data_to_join/",file)
data <- read.csv(path)
name <- str_sub(file,start = 1, end = -5)
assign(name,data)
remove(data)
}

# Edit the data to prepare for joining

colnames(complication_summary)[1] <- "Provider.ID"
colnames(hvbp)[1] <- "Provider.ID"
ipfqr <- ipfqr[,-1]
colnames(ipfqr)[1] <- "Provider.ID"
joined_payments <- joined_payments[,-1]
outpatient_imagery_effect_summary <- outpatient_imagery_effect_summary[,-1]
timely_effective_care_summary <- timely_effective_care_summary[,-1]

# Join the data
complete_joined_data <- left_join(joined_payments,complication_summary, by = "Provider.ID") %>%
  left_join(hvbp) %>% 
  left_join(ipfqr) %>%
  left_join(outpatient_imagery_effect_summary) %>%
  left_join(timely_effective_care_summary) %>%
  left_join(joined_payments)

write_csv(complete_joined_data,"data/complete_joined_data")


## Merge the tables about columns

rm(list = ls())
effectivecare <- read.csv("data_to_join/coltable_effectivecare")
imagery <- read.csv("data_to_join/coltable_imagery")
complication <- read.csv("data_to_join/complication_summary_Measureid_measurename.csv")
payment <- read.csv("data_to_join/Payment_ids.csv")
payment <- payment[,-1]
payment <- payment %>% select(2,1)
colnames(payment) <- colnames(effectivecare)
medical_code <- read_csv("data_to_join/medical_codes.csv")
colnames(medical_code) <- colnames(effectivecare)

complete_col_table <- rbind(effectivecare,imagery) %>% 
  rbind(complication) %>% 
  rbind(payment) %>%
  rbind(medical_code)

write_csv(complete_col_table,"data/complete_col_table")



