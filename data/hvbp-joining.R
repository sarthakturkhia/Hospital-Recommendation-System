library(dplyr)
library(tidyverse)

#Reads in .csv files for IPFQR data and hospital data
hvbp.raw.df.01 <- read.csv("raw/hospital_level_data/hvbp_safety_11_07_2017.csv", stringsAsFactors = FALSE)
hvbp.raw.df.02 <- read.csv("raw/hospital_level_data/hvbp_clinical_care_11_07_2017.csv", stringsAsFactors = FALSE)


#Selects columns interested in and using
hvbp.psi.df <- hvbp.raw.df.01[,c(c(1), seq(8,64,1))]
hvbp.mort.df <- hvbp.raw.df.02[,c(c(1), seq(8,28,1))]

#Creates master dataframe with joined together data
hospital.df <- merge(x = hvbp.mort.df, y = hvbp.psi.df, by.x=c("Provider.Number"), by.y=c("Provider.Number"))

#Prints this dataframe
write.csv(hospital.df, "joined_data/hvbp.csv", row.names=FALSE)

#Summary data
hvbp.analysis.df <- hospital.df[,grep(pattern="Baseline",colnames(hospital.df))]

analysis.df <- hvbp.analysis.df %>% 
  gather(col.name, value) %>% 
  group_by(col.name) %>% 
  summarise()

mean.vtr <- c(
 suppressWarnings(mean(as.numeric(hvbp.analysis.df$HAI.1.Baseline.Rate[1:2807]), na.rm = TRUE))
,suppressWarnings(mean(as.numeric(hvbp.analysis.df$HAI.2.Baseline.Rate[1:2807]), na.rm = TRUE))
,suppressWarnings(mean(as.numeric(hvbp.analysis.df$HAI.3.Baseline.Rate[1:2807]), na.rm = TRUE))
,suppressWarnings(mean(as.numeric(hvbp.analysis.df$HAI.4.Baseline.Rate[1:2807]), na.rm = TRUE))
,suppressWarnings(mean(as.numeric(hvbp.analysis.df$HAI.5.Baseline.Rate[1:2807]), na.rm = TRUE))
,suppressWarnings(mean(as.numeric(hvbp.analysis.df$HAI.6.Baseline.Rate[1:2807]), na.rm = TRUE))
,suppressWarnings(mean(as.numeric(hvbp.analysis.df$MORT.30.AMI.Baseline.Rate[1:2807]), na.rm = TRUE))
,suppressWarnings(mean(as.numeric(hvbp.analysis.df$MORT.30.HF.Baseline.Rate[1:2807]), na.rm = TRUE))
,suppressWarnings(mean(as.numeric(hvbp.analysis.df$MORT.30.PN.Baseline.Rate[1:2807]), na.rm = TRUE))
,suppressWarnings(mean(as.numeric(hvbp.analysis.df$PC.01.Baseline.Rate[1:2807]), na.rm = TRUE))
,suppressWarnings(mean(as.numeric(hvbp.analysis.df$PSI.90.Baseline.Rate[1:2807]), na.rm = TRUE))
)

analysis.df$mean <- mean.vtr

median.vtr <- c(
  suppressWarnings(median((hvbp.analysis.df$HAI.1.Baseline.Rate[1:2807]), na.rm = TRUE))
  ,suppressWarnings(median(as.numeric(hvbp.analysis.df$HAI.2.Baseline.Rate[1:2807]), na.rm = TRUE))
  ,suppressWarnings(median(as.numeric(hvbp.analysis.df$HAI.3.Baseline.Rate[1:2807]), na.rm = TRUE))
  ,suppressWarnings(median(as.numeric(hvbp.analysis.df$HAI.4.Baseline.Rate[1:2807]), na.rm = TRUE))
  ,suppressWarnings(median(as.numeric(hvbp.analysis.df$HAI.5.Baseline.Rate[1:2807]), na.rm = TRUE))
  ,suppressWarnings(median(as.numeric(hvbp.analysis.df$HAI.6.Baseline.Rate[1:2807]), na.rm = TRUE))
  ,suppressWarnings(median(as.numeric(hvbp.analysis.df$MORT.30.AMI.Baseline.Rate[1:2807]), na.rm = TRUE))
  ,suppressWarnings(median(as.numeric(hvbp.analysis.df$MORT.30.HF.Baseline.Rate[1:2807]), na.rm = TRUE))
  ,suppressWarnings(median(as.numeric(hvbp.analysis.df$MORT.30.PN.Baseline.Rate[1:2807]), na.rm = TRUE))
  ,suppressWarnings(median(as.numeric(hvbp.analysis.df$PC.01.Baseline.Rate[1:2807]), na.rm = TRUE))
  ,suppressWarnings(median(as.numeric(hvbp.analysis.df$PSI.90.Baseline.Rate[1:2807]), na.rm = TRUE))
)

analysis.df$median <- median.vtr

sd.vtr <- c(
  suppressWarnings(sd(as.numeric(hvbp.analysis.df$HAI.1.Baseline.Rate[1:2807]), na.rm = TRUE))
  ,suppressWarnings(sd(as.numeric(hvbp.analysis.df$HAI.2.Baseline.Rate[1:2807]), na.rm = TRUE))
  ,suppressWarnings(sd(as.numeric(hvbp.analysis.df$HAI.3.Baseline.Rate[1:2807]), na.rm = TRUE))
  ,suppressWarnings(sd(as.numeric(hvbp.analysis.df$HAI.4.Baseline.Rate[1:2807]), na.rm = TRUE))
  ,suppressWarnings(sd(as.numeric(hvbp.analysis.df$HAI.5.Baseline.Rate[1:2807]), na.rm = TRUE))
  ,suppressWarnings(sd(as.numeric(hvbp.analysis.df$HAI.6.Baseline.Rate[1:2807]), na.rm = TRUE))
  ,suppressWarnings(sd(as.numeric(hvbp.analysis.df$MORT.30.AMI.Baseline.Rate[1:2807]), na.rm = TRUE))
  ,suppressWarnings(sd(as.numeric(hvbp.analysis.df$MORT.30.HF.Baseline.Rate[1:2807]), na.rm = TRUE))
  ,suppressWarnings(sd(as.numeric(hvbp.analysis.df$MORT.30.PN.Baseline.Rate[1:2807]), na.rm = TRUE))
  ,suppressWarnings(sd(as.numeric(hvbp.analysis.df$PC.01.Baseline.Rate[1:2807]), na.rm = TRUE))
  ,suppressWarnings(sd(as.numeric(hvbp.analysis.df$PSI.90.Baseline.Rate[1:2807]), na.rm = TRUE))
)

analysis.df$sd <- sd.vtr

write.csv(analysis.df, "joined_data/hvbp_summary_mean_median_sd.csv", row.names=FALSE)


# Alternate code:
#
# Method #1:
# analysis.df[] <- as.numeric(factor(as.matrix(hvbp.analysis.df)))
# 
# Method #2:
# analysis.df <- data.frame(col_name = colnames(hvbp.analysis.df))
# 
# Method #3:
# for(i in c('mean','median','sd'))
# {
#   analysis.df[[i]] <- apply(t(hospital.df),2,eval(i), na.rm=T)
# }
#
# Method #4:
# data.frame(ID=hvbp.analysis.df[,1], Means=rowMeans(hvbp.analysis.df[,-1]))
# 
# Method #5:
# apply(hvbp.analysis.df, 2, mean, na.rm = TRUE)
