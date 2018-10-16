# Adds lat and long data to the hospital master .csv file. Last run on 06-March-2018 at 5:28PM PST. 
# Unless data has been updated (loaded in new datasets); do not run this code.

library(dplyr)

hospital.data <- read.csv("complete_joined_data.csv", stringsAsFactors = FALSE)
zipcode.data <- read.csv("zip-code_distance/hospital-lat-long-address.csv", stringsAsFactors = FALSE)
zipcode.data <- zipcode.data[, c("Provider.ID", "ZIP.Code")]
hospital.data <- merge(x = hospital.data, y = zipcode.data, by.x="Provider.ID", by.y="Provider.ID")
hospital.data$lat

write.csv(hospital.data, "complete_joined_data.csv")
