## analysis_for_inputs.R
## 3/3/18 AAMILLER
## Considers the different inputs possible from the UI and how the application
## may recommend hospitals based on that. 

library(dplyr)

joined.data <- read.csv("./../../data/complete_joined_data.csv", stringsAsFactors = FALSE)
measure.names.and.ids.data <- read.csv("./../../data/complete_col_table.csv", stringsAsFactors = FALSE)

# First, Arihan filters down provider IDs in the desired range, mocked up here
filtered.data <- joined.data %>% filter(Provider.ID < 10035)

# Clean all payment data columns of their "$" and "," symbols, cast to numeric
RemoveNumberFormatting <- function(vector) {
  output <- gsub('\\D+','', vector)
  return(as.numeric(output))
}
# Usage
filtered.data$PAYM_30_AMI <- RemoveNumberFormatting(filtered.data$PAYM_30_AMI)


## Possible inputs for "Reason for Visit" + the data they are informed by and who did that data work ###

# Stroke - Informed by Adele (complications)
stroke.death.rate <- filtered.data$"MORT_30_STK"

# Heart Failure
heart.failure.death.rate <- filtered.data$"MORT-30-HF"
payment.heart.failure <- filtered.data$PAYM_30_HF

# Heart Attack
heart.attack.payment.data <- filtered.data$PAYM_30_AMI


# Hip/Knee replacement - Adele + Arihan (Serious treatable complications) + Sarthak (Cost)
hip.knee.complication.rate <- filtered.data$COMP_HIP_KNEE
payment.hip.knee <- filtered.data$PAYM_90_HIP_KNEE


# Coronary bypass artery surgery - Adele (complications)
cabg.death.rate <- filtered.data$MORT_30_CABG

  
# Drug issues: Tobacco, Alcohol - Arihan (IPFQR, hvbp)
# Tobacco Use Screening during past 30 days
tobacco.use.screening <- filtered.data$TOB.1_.

# Tobacco Use Treatment (Counseling + Medication) Provided or Offered (during hospitalization, within the first three days after admission) 
tobacco.use.treatment <- filtered.data$TOB.2_.

# Tobacco Use Treatment Provided or Offered at Discharge
tobacco.use.treatment.at.dischage <- filtered.data$TOB.3_.

# Alcohol Use Screening during past 30 days
alcohol.use.screening <- filtered.data$SUB.1_.

# Alcohol Use Treatment (Counseling + Medication) Provided or Offered (during hospitalization, within the first three days after admission) 
alcohol.use.treatment <- filtered.data$SUB.2_.

# Alcohol Use Treatment Provided or Offered at Discharge
alcohol.use.treatment.at.dischage <- filtered.data$SUB.3_.
  
# Other Surgery (inform with complications data - wound splits, "serious complications" blood clots)
abdomen.wound.splits <- filtered.data$PSI_14_POSTOP_DEHIS
serious.complication.rate <- filtered.data$PSI_90_SAFETY
blood.clot <- filtered.data$PSI_12_POSTOP_PULMEMB_DVT
death.from.serious.treatable.complications <- filtered.data$PSI_4_SURG_COMP


# Mental illness (by 30 day and 7 day follow up time, + patients discharged with appropriate meds - Arihan)
# Percent of patients receiving follow-up care within 30 days 
# (FUH-30) or within 7 days (FUH-7) after hospitalization for mental illness
mental.illness.follow.up.30 <- filtered.data$FUH.30_.
mental.illness.follow.up.7 <- filtered.data$FUH.7_.
mental.illness.justified.medication <- filtered.data$HBIPS.5_Overall_._of_Total


# Infections, accidents, and equipment failure
equipment.failure.and.accidents <- filtered.data$PSI.90.Baseline.Rate
infection.bloodstream.bc.centralline <- filtered.data$HAI.1.Baseline.Rate
infection.urinary.bc.catheter <- filtered.data$HAI.2.Baseline.Rate

# The following infections are from the surgical site from the type of surgery conducted (e.g.- colon)
infection.colon.surgery.site <- filtered.data$HAI.3.Baseline.Rate
infection.abdonminal.surgery.site <- filtered.data$HAI.4.Baseline.Rate
# Methicillin-resistant Staphylococcus aureus (or MRSA) blood laboratory-identified events (bloodstream infections)
infection.bloodstream.bc.accidents <- filtered.data$HAI.5.Baseline.Rate

# Other (general hospital quality) (Norry and Arihan?)
 # Wait times
fibrinolysis <- filtered.data$OP_1  # Blood clot medication
pain.meds <- filtered.data$OP_21
time.to.ecg <- filtered.data$OP_5


# Takes in dataframe and a column to assign "ranks" to and returns the data frame
# with the column's values replaced with ranks. The ranks are from (1, length of vector - NA values)
# with lower ones showing the better scores.
# NAs are assigned the average value rank.
CreateRanks <- function(data, column) {
  data.column <- data[, column]
  # Count NAs 
  na.count <- sum(is.na(data.column))
  output <- data %>% arrange_(sym(column))
  output[, column] <- 1:length(data.column)
  average.rank <- mean(1:(length(data.column) - na.count))
  output[(length(data.column) - na.count + 1):length(data.column), column] <- average.rank
  return(output)
}

# Note: Payment data dollar signs and commas need to be cleaned before those columns can be used
ranked <- CreateRanks(filtered.data, "MORT_30_STK")
ranked <- CreateRanks(ranked, "PSI_14_POSTOP_DEHIS")
ranked <- ranked %>% select(MORT_30_STK, PSI_14_POSTOP_DEHIS)

# Weights the ranks of different columns. Needs col length = weight length
# Adds a column to the data table to store weight calculations called "weighted.ranks"
WeightRanks <- function(data, columns, weights) {
  data$weighted.ranks <- 0
  if (length(columns) == length(weights)) {
    for (row in 1:nrow(data)) {
      for (interest.col in 1:length(columns)) {
        data$weighted.ranks[row] <- data$weighted.ranks[row] + (data[row, columns[interest.col]] * weights[interest.col])
      }
    }
    return(data)
  } else {
    print("Weight and column lengths unequal.")
  }
}
weighted.ranks <- WeightRanks(ranked, c("MORT_30_STK", "PSI_14_POSTOP_DEHIS"), c(1, 5))