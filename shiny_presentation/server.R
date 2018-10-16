library(dplyr)
#(gmapsdistance)
library(ggmap)
library("httr")
library(leaflet)
library(tidyverse)
library(shiny)
library(stringr)
library(rlang)

hospital.df <- read.csv("../data/complete_joined_data.csv", stringsAsFactors = FALSE)

zip.api.key <-'PzFgQ1YvdpWmTwKZleDGkFUyJZ66j02QRnDL0lzKU4TH8fxU83RFDxN0zWBKZm0f'

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
                            "HBIPS.5_Overall_._of_Total" = "patients discharged on multiple antipsychotic medications with appropriate justification",
                            "general.hosp.stats.ranks" = "various metrics used to evaluate hospital safety and quality")
# Values (multipliers) for low, medium and high weights
low <- 1
medium <- 1.5  # Payment, other significant factors
high <- 2      # Death rates


server <- function(input, output) {
  
  # Makes an API call and returns data based on 
  # input zipcode, radius from zipcode, and input radius distance unit
  GetZipCodeData <- function(zip, dist, unit) {
    zip.url <-    paste0('https://www.zipcodeapi.com/rest/', zip.api.key, '/radius.csv/', zip, '/', dist, '/', unit)
    
    response <- GET(zip.url)
    zip.df <- read.csv(textConnection(content(response, 'text')))
    
    # Selects a subset of the hospitals dataframe whose zipcodes match those near the user
    p.hospital.df <- subset(hospital.df, ZIP.Code %in% zip.df$zip_code)
    
    # Joins information about distance from API's df to hospital subset df
    p.hospital.df <- merge(x = p.hospital.df, y = zip.df, by.x="ZIP.Code", by.y="zip_code") %>% arrange(distance)
    return(p.hospital.df)
  }
  
  zipcode <- reactive({
    input$zipcode
  })
  distance <- reactive({
    input$distance
  })
  unit <- reactive({
    input$unit
  })
  
  
  # Get call output for current inputs
  filtered.hospital.data <- reactive({
    return(GetZipCodeData(zipcode(), distance(), unit()))
  })

  
  # Creates and Renders map using subsetted hospital dataframe
  output$mymap <- renderLeaflet({
    # Validates user input
    validate(
      need(input$zipcode != "", "Please enter a zipcode"),
      need(grep("[0-9]{5}", input$zipcode), "Please enter a valid 5 digit zipcode"))
    
    leafy <- leaflet() %>% addTiles() %>% addMarkers(data = filtered.hospital.data(), lat = ~ lat, lng = ~ lon, label = ~Hospital.name)
    
    return(leafy)
  })
  
  CreateRanks <- function(data, column) {
    # Select relevant column to rank
    data.column <- data[, column]
    
    # Count NAs 
    na.count <- sum(is.na(data.column))
    output <- data %>% arrange_(column) # Ascending order
    
    # Assign ranks 1 through length of column
    output[, column] <- 1:length(data.column)
    
    # Assign NA values to be the worst rank
    output[(length(data.column) - na.count + 1):length(data.column), column] <- 0
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
  DetermineHighlightReasons <- function(data, rows) {
    data$Reasons <- ""
    for(colname in colnames(data)) {
      # Check if column is one of the measures
      if (colname %in% names(IDS.to.Descriptions)) {
        for(row in 1:nrow(data)) {
          if (data$Reasons[row] == "" & data[row, colname]) {  # Fenceposting so commas are in the correct location
            data$Reasons[row] <- paste0("Ranked ", if(data[row, colname] != 0) (rows - data[row, colname] + 2) else "(No rank, unknown)",
                                        " in ", IDS.to.Descriptions[[colname]])
          } else {
            data$Reasons[row] <- paste0(data$Reasons[row], ", ranked ", 
                                        if(data[row, colname] != 0) (rows - data[row, colname] + 2) else "(No rank, unknown)", " in ", IDS.to.Descriptions[[colname]])
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
    filted.data <- filtered.hospital.data
    filtered.data <- RankMultipleCols(filtered.data, c("PSI.90.Baseline.Rate",
                                                       "OP_1", "OP_21", "OP_5"))
    # Time until medications or screening
    # Blood clot medicine, pain medical, ecg screening
    filtered.data <- WeightRanks(filtered.data, c("PSI.90.Baseline.Rate",
                                                  "OP_1", "OP_21", "OP_5"),
                                 c(low, medium, medium, medium))
    filtered.data$general.hosp.stats <- filtered.data$weighted.ranks  # Change name so it can be differentiated from other weights
    filtered.data <- filtered.data %>% filter(!is.na(Hospital.name))
    ranks <- CreateRanks(filtered.data, "general.hosp.stats") %>% filter(!is.na(Hospital.name))
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
  
  ####################### END OF SUPPORTING FUNCTIONS FOR RECOMMENDATION TABLE ###############################
  
  # Render the hispital recommendations
  output$recommendedHospitals <- DT::renderDataTable({
    reason <- input$reason.for.visit  # User input
    poss.reasons.for.visit <- c("Stroke", "Heart Failure", "Heart Attack", "Hip or Knee Replacement", "Coronary Bypass Artery Surgery",
                                "Tobacco Use", "Alcohol Use", "Other Surgery", "Mental Illness", "Other")
    recommendationTable <- filtered.hospital.data()  # Table filtered by zipcode
    recommendationTable <- GetGeneralHospitalStatsValue(recommendationTable) # Add weighted hospital general stat col
    
    ## If visiting for stroke treatment
    ### Informed by stroke death rate at a high weight, general hospital factors at a low weight.
    ## TODO: GENERAL HOSPITAL RANKING
    if (reason == poss.reasons.for.visit[1]) {
      recommendationTable <- RankMultipleCols(recommendationTable, c("MORT_30_STK"))
      recommendationTable <- WeightRanks(recommendationTable, c("MORT_30_STK"), c(high))
      
      # Select used columns
      recommendationTable <- recommendationTable %>% select(Hospital.name, general.hosp.stats, weighted.ranks, distance,
                                                            general.hosp.stats.ranks, MORT_30_STK)
    }
    
    ## If visiting for heart failure
    ### Informed by death rate at a high weight, payment at a medium weight, and general hospital at a low weight.
    if (reason == poss.reasons.for.visit[2]) { ###
      recommendationTable$PAYM_30_HF <- CleanPaymentData(recommendationTable$PAYM_30_HF)
      recommendationTable <- RankMultipleCols(recommendationTable, c("MORT.30.HF.Measure.Score", "PAYM_30_HF"))
      recommendationTable <- WeightRanks(recommendationTable, c("MORT.30.HF.Measure.Score", "PAYM_30_HF"), c(high, medium))
      
      # Select used columns
      recommendationTable <- recommendationTable %>% select(Hospital.name, general.hosp.stats, weighted.ranks, distance,
                                                            general.hosp.stats.ranks, MORT.30.HF.Measure.Score, PAYM_30_HF)
    }
    
    ## If visiting for heart attack
    ### Informed by payment at a medium weight, and general hospital at a low weight.
    if (reason == poss.reasons.for.visit[3]) {
      recommendationTable$PAYM_30_AMI <- CleanPaymentData(recommendationTable$PAYM_30_AMI)
      recommendationTable <- RankMultipleCols(recommendationTable, c("PAYM_30_AMI"))
      recommendationTable <- WeightRanks(recommendationTable, c("PAYM_30_AMI"), c(medium))
      
      # Select used columns
      recommendationTable <- recommendationTable %>% select(Hospital.name, general.hosp.stats, weighted.ranks,
                                                            distance, general.hosp.stats.ranks, PAYM_30_AMI)
    }
    
    ## If visiting for hip/knee replacement
    ### Informed by serious complication rate at a high weight, payment at a medium weight, general hospital at a low weight.
    if (reason == poss.reasons.for.visit[4]) {
      recommendationTable$PAYM_90_HIP_KNEE <- CleanPaymentData(recommendationTable$PAYM_90_HIP_KNEE)
      recommendationTable <- RankMultipleCols(recommendationTable, c("PAYM_90_HIP_KNEE","COMP_HIP_KNEE"))
      recommendationTable <- WeightRanks(recommendationTable, c("PAYM_90_HIP_KNEE", "COMP_HIP_KNEE"), c(medium, high))
      
      # Select used columns
      recommendationTable <- recommendationTable %>% select(Hospital.name, general.hosp.stats, weighted.ranks, distance,
                                                            general.hosp.stats.ranks, PAYM_90_HIP_KNEE, COMP_HIP_KNEE)
    }
    
    ## If visiting for coronary bypass artery surgery
    ### Informed by death rate and gen hosp
    if (reason == poss.reasons.for.visit[5]) {
      recommendationTable <- RankMultipleCols(recommendationTable, c("MORT_30_CABG"))
      recommendationTable <- WeightRanks(recommendationTable, c("MORT_30_CABG"), c(high))
      # Select used columns
      recommendationTable <- recommendationTable %>% select(Hospital.name, general.hosp.stats,
                                                            weighted.ranks, distance, general.hosp.stats.ranks)
    }
    
    ## If visiting for tobacco use
    ### Tobacco Use Screening during past 30 days
    ### Tobacco Use Treatment (Counseling + Medication) Provided or Offered (during hospitalization,
    ###                                                 within the first three days after admission) 
    ### Tobacco Use Treatment Provided or Offered at Discharge
    
    if (reason == poss.reasons.for.visit[6]) {
      recommendationTable <- RankMultipleCols(recommendationTable, c("TOB.1_.", "TOB.2_.", "TOB.3_."))
      recommendationTable <- WeightRanks(recommendationTable, c("TOB.1_.", "TOB.2_.", "TOB.3_."), c(low, medium, medium))
      
      # Select used columns
      recommendationTable <- recommendationTable %>% select(Hospital.name, general.hosp.stats, weighted.ranks,
                                                            distance, general.hosp.stats.ranks, TOB.1_., TOB.2_., TOB.3_.)
    }
    
    ## If visiting for alcohol use
    ### Alcohol Use Screening during past 30 days
    ### Alcohol Use Treatment (Counseling + Medication) Provided or Offered (during hospitalization, within the first three days after admission) 
    
    if (reason == poss.reasons.for.visit[7]) {
      recommendationTable <- RankMultipleCols(recommendationTable, c("SUB.1_.", "SUB.2_."))
      recommendationTable <- WeightRanks(recommendationTable, c("SUB.1_.", "SUB.2_."), c(low, medium))
      
      # Select used columns
      recommendationTable <- recommendationTable %>% select(Hospital.name, general.hosp.stats, weighted.ranks,
                                                            distance, general.hosp.stats.ranks, SUB.1_., SUB.2_.)
    }
    
    ## If visiting for other surgery
    if (reason == poss.reasons.for.visit[8]) {
      recommendationTable <- RankMultipleCols(recommendationTable, c("PSI_14_POSTOP_DEHIS",       # Abdomen wound splits
                                                                     "PSI_90_SAFETY",             # Aggregate safety measure from data set
                                                                     "PSI_12_POSTOP_PULMEMB_DVT", # Pulmenary embolism
                                                                     "PSI_4_SURG_COMP",           # Post-surgery complications # Surgical site infections
                                                                     "HAI.1.Baseline.Rate", "HAI.2.Baseline.Rate",
                                                                     "HAI.3.Baseline.Rate", "HAI.4.Baseline.Rate", "HAI.5.Baseline.Rate"))
      recommendationTable <- WeightRanks(recommendationTable, c("PSI_14_POSTOP_DEHIS", "PSI_90_SAFETY",
                                                                "PSI_12_POSTOP_PULMEMB_DVT", "PSI_4_SURG_COMP", "HAI.1.Baseline.Rate","HAI.2.Baseline.Rate",
                                                                "HAI.3.Baseline.Rate", "HAI.4.Baseline.Rate", "HAI.5.Baseline.Rate"),
                                         c(high, medium, high, high, high, high, high, high, high))
      # Select used columns
      recommendationTable <- recommendationTable %>% select(Hospital.name, general.hosp.stats, weighted.ranks, distance, general.hosp.stats.ranks, PSI_14_POSTOP_DEHIS,
                                                            PSI_90_SAFETY, PSI_12_POSTOP_PULMEMB_DVT, PSI_4_SURG_COMP,HAI.1.Baseline.Rate, HAI.2.Baseline.Rate,
                                                            HAI.3.Baseline.Rate, HAI.4.Baseline.Rate, HAI.5.Baseline.Rate)
    }
    
    ## If visiting for mental illness
    ### Mental illness (by 30 day and 7 day follow up time, + patients discharged with appropriate meds)
    ### Percent of patients receiving follow-up care within 30 days 
    ### (FUH-30) or within 7 days (FUH-7) after hospitalization for mental illness
    if (reason == poss.reasons.for.visit[9]) {
      recommendationTable <- RankMultipleCols(recommendationTable, c("FUH.30_.", "FUH.7_.", "HBIPS.5_Overall_._of_Total"))
      recommendationTable <- WeightRanks(recommendationTable, c("FUH.30_.", "FUH.7_.", "HBIPS.5_Overall_._of_Total"), c(medium, low, high))
      
      # Select used columns
      recommendationTable <- recommendationTable %>% select(Hospital.name, general.hosp.stats, weighted.ranks, distance, general.hosp.stats.ranks,
                                                            FUH.30_., FUH.7_., HBIPS.5_Overall_._of_Total)
    }
    
    ## If visiting for all other reasons
    if (reason == poss.reasons.for.visit[10]) {
      recommendationTable <- RankMultipleCols(recommendationTable, c("PSI.90.Baseline.Rate",
                                                                     "OP_1", "OP_21", "OP_5"))
      recommendationTable <- WeightRanks(recommendationTable, c("PSI.90.Baseline.Rate",
                                                                "OP_1", "OP_21", "OP_5"), c(low, medium, medium, medium))
      # Select used columns
      recommendationTable <- recommendationTable %>% select(Hospital.name, general.hosp.stats, weighted.ranks, distance, general.hosp.stats.ranks,
                                                            PSI.90.Baseline.Rate, OP_1, OP_21, OP_5)
    }
    
    # Re-ranks based on how the weighted ranks are distributed after weighting
    recommendationTable <- CreateRanks(recommendationTable, "weighted.ranks") %>% filter(!is.na(Hospital.name))
    
    
    # Add formatted reasons for why these hospitals were chosen, programmatically
    recommendationTable <- DetermineHighlightReasons(recommendationTable, nrow(recommendationTable))
    
    # Change the ranks so that low ones are the optimal ones, as it's more intuitive for most viewers
    # when 1 (lower) is better than (4) higher numbers
    recommendationTable <- FlipRanks(recommendationTable, "weighted.ranks")
    
    # Select columns for output
    recommendationTable <- recommendationTable %>% select(Hospital.name, weighted.ranks, distance, Reasons) %>% arrange(weighted.ranks)
    
    # Format column names
    colnames(recommendationTable) <- c("Hospital", "Rank", "Distance from Zipcode", "Reason for Recommendation")
    return(recommendationTable)
  })
  
  # Outputs logo on the top of the page
  output$logo <- renderImage({
    list(src = "Logo.png",
         width = 200, 
         height = 200,
         alt = "Unable to display image")
  }, deleteFile = FALSE)
  
}


