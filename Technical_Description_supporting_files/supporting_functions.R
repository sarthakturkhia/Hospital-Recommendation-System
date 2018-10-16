# supporting_functions.R
# AAMILLER 3/9/18
# Supporting functions for the display in the technical description.
# Separated from main document in order to make it easy to look at this code
# independent of the server.R file.

library(dplyr)
library(rlang)


# Adds readable descriptions for each measure so that they can be displayed as with a reason + ranks in the recommendation table.
# Ex. "Ranked x in the __ category"
IDS.to.Descriptions <- list("PSI.90.Baseline.Rate" = "lowest hospital accidents and equipment failure rates", 
                            "HAI.1.Baseline.Rate" = "lowest bloodstream infections rates", 
                            "HAI.2.Baseline.Rate" = "lowest urinary tract infections", 
                            "HAI.3.Baseline.Rate" = "lowest surgical site infections from colon surgery", 
                            "HAI.4.Baseline.Rate" = "lowest surgical site infection from abdominal hysterectomy", 
                            "HAI.5.Baseline.Rate" = "lowest rates of bloodstream infections ", 
                            "OP_21" = "median time until pain medication given",
                            "OP_1" = "median time to fibrinolysis (blood clot medication)", 
                            "OP_5" = "median time until electrocardiography (electric impulse measuring)", 
                            "MORT_30_STK" = "death rate for stroke patients", 
                            "MORT.30.HF.Measure.Score" = "death rate for heart failure patients", 
                            "PAYM_30_HF" = "cost for heart failure patients",
                            "PAYM_90_HIP_KNEE" = "cost for hip/knee replacement patients", 
                            "COMP_HIP_KNEE" = "rate of complications for hip/knee replacement patients", 
                            "PAYM_30_AMI" = "cost for heart attack patients", 
                            "MORT_30_CABG" = "death rate for coronary artery bypass surgery", 
                            "TOB.1_." = "Tobacco Use Screening",
                            "TOB.2_." = "Tobacco Use Treatment (during the hospital stay)", 
                            "TOB.3_." = "Tobacco Use Treatment Provided or Offered at Discharge", 
                            "SUB.1_." = "Alcohol Use Screening", 
                            "SUB.2_." = "Alcohol Use Brief Intervention Provided or Offered", 
                            "PSI_14_POSTOP_DEHIS" = "rate of abdomen or pelvis wounds spliting open after surgery",
                            "PSI_90_SAFETY" = "fewest serious complications",
                            "PSI_12_POSTOP_PULMEMB_DVT" = "serious blood clots after surgery",
                            "PSI_4_SURG_COMP" = "death among patients with serious treatable complications after surgery", 
                            "FUH.30_." = "following up with patients within 30 days after hospitalization for mental illness", 
                            "FUH.7_." = "following up with patients within 7 days after hospitalization for mental illness",
                            "HBIPS.5_Overall_._of_Total" = "properly justifying antipsychotic medication to discharged patients",
                            "general.hosp.stats.ranks" = "various metrics used to evaluate hospital safety and quality")


# Values (multipliers) for low, medium and high weights
low <- 1
medium <- 1.5  # Payment, other significant factors
high <- 2      # Death rates


# Takes in dataframe and a column to assign "ranks" to and returns the data frame
# with the column's values replaced with ranks. The ranks are from (1, length of vector - NA values)
# with higher ones showing the better scores.
# The worst (lowest) rank is assigned to hospitals with "NA" values, assuming the worst.
CreateRanks <- function(data, column) {
  # Select relevant column to rank
  data.column <- data[, column]
  
  # Count NAs 
  na.count <- sum(is.na(data.column))
  output <- data %>% arrange_(column) # Ascending order
  
  # Assign ranks 1 through length of column
  output[, column] <- 1:length(data.column)
  
  # Assign NA values to be the worst rank
  output[(length(data.column) - na.count + 1):length(data.column), column] <- length(data.column) - na.count
  return(output)
}

# Takes in a data frame and multiple columns
# to give a rank to. Outputs that data frame the columns
# replaced by their ranks.
RankMultipleCols <- function(data, columns) {
  output <- data
  for (column in columns) {
    output <- CreateRanks(output, column)
  }
  return(output)
}

# Weights the ranks of different columns. Needs col length == weight length
# Adds a column to the data table to store weight calculations called "weighted.ranks"
# which is each rank of the hospital in the input columns * the weight.
# Since lower weights are 
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

# Remove dollar symbols and commas from payment columns
# and cast to numeric. Usage
# data$col <- CleanPaymentData(data$col)
# Payment data is negated so that the largest values will become the smallest, allowing the 
# lower payment values to be preferable.
CleanPaymentData <- function(column) {
  return(-1 * as.numeric(gsub('[$]([0-9]+)[,]([0-9]+)','\\1\\2', column)))
}

# Takes in columns and a string describing what each measure is.
# Outputs a sentence stating what the hospital is best in.
# This serves the function of explaining why certain hospitals are ranked highly
# since we are not showing out weighting system on our GUI.
DetermineHighlightReasons <- function(data) {
  data$Reasons <- ""
  for(colname in colnames(data)) {
    # Check if column is one of the measures
    if (colname %in% names(IDS.to.Descriptions)) {
      for(row in 1:nrow(data)) {
        if (data$Reasons[row] == "" & data[row, colname]) {  # Fenceposting so commas are in the correct location
          data$Reasons[row] <- paste0("Ranked ", (num.hospitals.in.range - data[row, colname] + 1), " in ", IDS.to.Descriptions[[colname]])
        } else {
          data$Reasons[row] <- paste0(data$Reasons[row], ", ranked ", (num.hospitals.in.range - data[row, colname] + 1), " in ", IDS.to.Descriptions[[colname]])
        }
      }
    }
  }
  return(data)
}

# Uses the columns that are not for specific reasons for visits
# to calculate value that can be used to provide a judgment of how good the hospital is
# when weighted together into one rank value.
# Returns the input data frame with a weighted value based on the 
# "general" hospital measures, such as time until medication
# and score for equipment failures.
GetGeneralHospitalStatsValue <- function(filtered.data) {
  filtered.data <- RankMultipleCols(filtered.data, c("PSI.90.Baseline.Rate", "HAI.1.Baseline.Rate", "HAI.2.Baseline.Rate",
                                                     # Infections, accidents, equipment failure -- 
                                                     # Equipment failure and accidents, bloodstream infections from central line, catheter infection
                                                     "HAI.3.Baseline.Rate", "HAI.4.Baseline.Rate", "HAI.5.Baseline.Rate"))
  # Time until medications or screening
  # Blood clot medicine, pain medical, ecg screening
  filtered.data <- WeightRanks(filtered.data, c("PSI.90.Baseline.Rate", "HAI.1.Baseline.Rate", "HAI.2.Baseline.Rate", 
                                                "HAI.3.Baseline.Rate", "HAI.4.Baseline.Rate", "HAI.5.Baseline.Rate"),
                               c(low, medium, low, medium, medium, high))
  filtered.data$general.hosp.stats <- filtered.data$weighted.ranks  # Change name so it can be differentiated from other weights
  ranks <- (CreateRanks(filtered.data, "general.hosp.stats") %>% filter(!is.na(Hospital.name)))
  filtered.data$general.hosp.stats.ranks <- ranks$general.hosp.stats
  return(filtered.data)
}

# Changes the ranks so that the best/optimal values are represented
# as smaller numbers so that it's more intuitive to the user, as we usually expect
# rank 1 to be the best
FlipRanks <- function(data, column) {
  # Select relevant column to rank
  data.column <- data[, column]
  
  output <- data %>% arrange(desc(!!sym(column))) # Descending order
  
  # Assign ranks 1 through length of column
  output[, column] <- 1:length(data.column)
  
  return(output)
}