library(dplyr)
library(stringr)
library(tidyr)

#Reads in .csv files for IPFQR data and hospital data
ipfqr.df.1 <- read.csv("raw/hospital_level_data/IPFQR_FUH_QualityMeasures_Facility.csv")
ipfqr.df.2 <- read.csv("raw/hospital_level_data/IPFQR_QualityMeasures_Facility.csv")

#Selects columns interested in and using
ipfqr.df.1 <- ipfqr.df.1[,c(1,8,9,10,11,12,13,14,15,16,17,18)]
ipfqr.df.2 <- ipfqr.df.2[,c(c(1), seq(8,70,1), seq(82,93,1))]

#Creates master dataframe with joined together data
hospital.df <- merge(x = ipfqr.df.1, y = ipfqr.df.2, by.x=c("Provider_Number"), by.y=c("Provider_Number"))
hospital.df <- hospital.df[,-grep(pattern="Desc",colnames(hospital.df))]
hospital.df <- hospital.df[,-grep(pattern="Footnote",colnames(hospital.df))]
hospital.df <- hospital.df[,-grep(pattern="Date",colnames(hospital.df))]

#Prints this dataframe
write.csv(hospital.df, "joined_data/ipfqr.csv")


#Analysis data
ipfqr.analysis.df <- hospital.df[,-grep(pattern="ator",colnames(hospital.df))]
ipfqr.analysis.df <- ipfqr.analysis.df[,-grep(pattern="Den",colnames(ipfqr.analysis.df))]
ipfqr.analysis.df <- ipfqr.analysis.df[,-grep(pattern="Response",colnames(ipfqr.analysis.df))]
ipfqr.analysis.df <- ipfqr.analysis.df[,-grep(pattern="Provider",colnames(ipfqr.analysis.df))]

analysis.df <- ipfqr.analysis.df %>% 
  gather(col.name, value) %>% 
  group_by(col.name) %>% 
  summarise()


mean.vtr <- c(
  suppressWarnings(mean(as.numeric(ipfqr.analysis.df$FUH.30_.), na.rm = TRUE))
  ,suppressWarnings(mean(as.numeric(ipfqr.analysis.df$FUH.7_.), na.rm = TRUE))
  ,suppressWarnings(mean(as.numeric(ipfqr.analysis.df$HBIPS.2_Overall_Num), na.rm = TRUE))
  ,suppressWarnings(mean(as.numeric(ipfqr.analysis.df$HBIPS.2_Overall_Rate_Per_1000), na.rm = TRUE))
  ,suppressWarnings(mean(as.numeric(ipfqr.analysis.df$HBIPS.3_Overall_Num), na.rm = TRUE))
  ,suppressWarnings(mean(as.numeric(ipfqr.analysis.df$HBIPS.3_Overall_Rate_Per_1000), na.rm = TRUE))
  ,suppressWarnings(mean(as.numeric(ipfqr.analysis.df$HBIPS.5_Overall_._of_Total), na.rm = TRUE))
  ,suppressWarnings(mean(as.numeric(ipfqr.analysis.df$HBIPS.5_Overall_Num), na.rm = TRUE))
  ,suppressWarnings(mean(as.numeric(ipfqr.analysis.df$HCP_.), na.rm = TRUE))
  ,suppressWarnings(mean(as.numeric(ipfqr.analysis.df$IMM.2_.), na.rm = TRUE))
  ,suppressWarnings(mean(as.numeric(ipfqr.analysis.df$SUB.1_.), na.rm = TRUE))
  ,suppressWarnings(mean(as.numeric(ipfqr.analysis.df$SUB.2_.), na.rm = TRUE))
  ,suppressWarnings(mean(as.numeric(ipfqr.analysis.df$SUB.2a_.), na.rm = TRUE))
  ,suppressWarnings(mean(as.numeric(ipfqr.analysis.df$TOB.1_.), na.rm = TRUE))
  ,suppressWarnings(mean(as.numeric(ipfqr.analysis.df$TOB.2_.), na.rm = TRUE))
  ,suppressWarnings(mean(as.numeric(ipfqr.analysis.df$TOB.2a_.), na.rm = TRUE))
  ,suppressWarnings(mean(as.numeric(ipfqr.analysis.df$TOB.3_.), na.rm = TRUE))
  ,suppressWarnings(mean(as.numeric(ipfqr.analysis.df$TOB.3a_), na.rm = TRUE))
)

analysis.df$mean <- mean.vtr

median.vtr <- c(
  suppressWarnings(median(as.numeric(ipfqr.analysis.df$FUH.30_.), na.rm = TRUE))
  ,suppressWarnings(median(as.numeric(ipfqr.analysis.df$FUH.7_.), na.rm = TRUE))
  ,suppressWarnings(median(as.numeric(ipfqr.analysis.df$HBIPS.2_Overall_Num), na.rm = TRUE))
  ,suppressWarnings(median(as.numeric(ipfqr.analysis.df$HBIPS.2_Overall_Rate_Per_1000), na.rm = TRUE))
  ,suppressWarnings(median(as.numeric(ipfqr.analysis.df$HBIPS.3_Overall_Num), na.rm = TRUE))
  ,suppressWarnings(median(as.numeric(ipfqr.analysis.df$HBIPS.3_Overall_Rate_Per_1000), na.rm = TRUE))
  ,suppressWarnings(median(as.numeric(ipfqr.analysis.df$HBIPS.5_Overall_._of_Total), na.rm = TRUE))
  ,suppressWarnings(median(as.numeric(ipfqr.analysis.df$HBIPS.5_Overall_Num), na.rm = TRUE))
  ,suppressWarnings(median(as.numeric(ipfqr.analysis.df$HCP_.), na.rm = TRUE))
  ,suppressWarnings(median(as.numeric(ipfqr.analysis.df$IMM.2_.), na.rm = TRUE))
  ,suppressWarnings(median(as.numeric(ipfqr.analysis.df$SUB.1_.), na.rm = TRUE))
  ,suppressWarnings(median(as.numeric(ipfqr.analysis.df$SUB.2_.), na.rm = TRUE))
  ,suppressWarnings(median(as.numeric(ipfqr.analysis.df$SUB.2a_.), na.rm = TRUE))
  ,suppressWarnings(median(as.numeric(ipfqr.analysis.df$TOB.1_.), na.rm = TRUE))
  ,suppressWarnings(median(as.numeric(ipfqr.analysis.df$TOB.2_.), na.rm = TRUE))
  ,suppressWarnings(median(as.numeric(ipfqr.analysis.df$TOB.2a_.), na.rm = TRUE))
  ,suppressWarnings(median(as.numeric(ipfqr.analysis.df$TOB.3_.), na.rm = TRUE))
  ,suppressWarnings(median(as.numeric(ipfqr.analysis.df$TOB.3a_), na.rm = TRUE))
)

analysis.df$median <- median.vtr

sd.vtr <- c(
  suppressWarnings(sd(as.numeric(ipfqr.analysis.df$FUH.30_.), na.rm = TRUE))
  ,suppressWarnings(sd(as.numeric(ipfqr.analysis.df$FUH.7_.), na.rm = TRUE))
  ,suppressWarnings(sd(as.numeric(ipfqr.analysis.df$HBIPS.2_Overall_Num), na.rm = TRUE))
  ,suppressWarnings(sd(as.numeric(ipfqr.analysis.df$HBIPS.2_Overall_Rate_Per_1000), na.rm = TRUE))
  ,suppressWarnings(sd(as.numeric(ipfqr.analysis.df$HBIPS.3_Overall_Num), na.rm = TRUE))
  ,suppressWarnings(sd(as.numeric(ipfqr.analysis.df$HBIPS.3_Overall_Rate_Per_1000), na.rm = TRUE))
  ,suppressWarnings(sd(as.numeric(ipfqr.analysis.df$HBIPS.5_Overall_._of_Total), na.rm = TRUE))
  ,suppressWarnings(sd(as.numeric(ipfqr.analysis.df$HBIPS.5_Overall_Num), na.rm = TRUE))
  ,suppressWarnings(sd(as.numeric(ipfqr.analysis.df$HCP_.), na.rm = TRUE))
  ,suppressWarnings(sd(as.numeric(ipfqr.analysis.df$IMM.2_.), na.rm = TRUE))
  ,suppressWarnings(sd(as.numeric(ipfqr.analysis.df$SUB.1_.), na.rm = TRUE))
  ,suppressWarnings(sd(as.numeric(ipfqr.analysis.df$SUB.2_.), na.rm = TRUE))
  ,suppressWarnings(sd(as.numeric(ipfqr.analysis.df$SUB.2a_.), na.rm = TRUE))
  ,suppressWarnings(sd(as.numeric(ipfqr.analysis.df$TOB.1_.), na.rm = TRUE))
  ,suppressWarnings(sd(as.numeric(ipfqr.analysis.df$TOB.2_.), na.rm = TRUE))
  ,suppressWarnings(sd(as.numeric(ipfqr.analysis.df$TOB.2a_.), na.rm = TRUE))
  ,suppressWarnings(sd(as.numeric(ipfqr.analysis.df$TOB.3_.), na.rm = TRUE))
  ,suppressWarnings(sd(as.numeric(ipfqr.analysis.df$TOB.3a_), na.rm = TRUE))
)

analysis.df$sd <- sd.vtr

write.csv(analysis.df, "joined_data/ipfqr_summary_mean_median_sd.csv", row.names=FALSE)

