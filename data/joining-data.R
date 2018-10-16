# AAMILLER 2/7/18
# Join mutliple data files, document what each one has so that we don't need to
# open every file to get an idea of what we have

d1 <- read.csv("raw/hospital_level_data/complications_deathsHospital.csv", stringsAsFactors = FALSE)
# Contains measures of mortality for different complications across hospitals, ex. Pneumonia (PN) 30-Day Mortality Rate MORT_30_PN
# Gives each issue a score
# Provides a comparison to the national average (better or worse)

d2 <- read.csv("raw/hospital_level_data/heathcare_associated_infections.csv", stringsAsFactors = FALSE)
# Similar to D1 -- measures and scores, ex. catheter-associated urinary tract infections (CAUTI) in ICUs and select wards HAI_2_SIR

#d3 <- read.csv("raw/hospital_level_data/HOSPITAL_ANNUAL_QUALITYMEASURE_PCH_EBRT_HOSPITAL.csv", stringsAsFactors = FALSE)
# External Beam Radiotherapy for Bone Metastases measure only, probably low utility for us

d4 <- read.csv("raw/hospital_level_data/HOSPITAL_ANNUAL_QUALITYMEASURE_PCH_OCM_HOSPITAL.csv", stringsAsFactors = FALSE)


d5 <- read.csv("raw/hospital_level_data/HOSPITAL_QUARTERLY_HAC_DOMAIN_HOSPITAL.csv", stringsAsFactors = FALSE)
# Different scores for hospitals on different topics, ex. MRSA, and payment reductions -- probably low utility for us

#d6 <- read.csv("raw/hospital_level_data/HOSPITAL_QUARTERLY_QUALITYMEASURE_PCH_HCAHPS_HOSPITAL.csv", stringsAsFactors = FALSE)
# Surveys about hospital - not very usable for our purposes so far


d7 <- read.csv("raw/hospital_level_data/HOSPITAL_QUARTERLY_QUALITYMEASURE_PCH_HOSPITAL.csv", stringsAsFactors = FALSE)
# Very few hospitals, cancer related care. Likely too limited scope for us

d8 <- read.csv("raw/hospital_level_data/medicare_spending_per_patient_hosp.csv", stringsAsFactors = FALSE)
# Medicare hospital spending per patient (Medicare Spending per Beneficiary) - for majroity of hospitals, it seems, with a score from each
# An option to look at if we want to try to reduce medicare spending, perhaps-- see what makes medicare spending per beneficiary higher at each hospital? 
# May be overly complex

d9 <- read.csv("raw/hospital_level_data/outpatient_imagery_effect.csv", stringsAsFactors = FALSE)
# Ex. MRI Lumbar Spine for Low Back Pain, basically imagery data

d10 <- read.csv("raw/hospital_level_data/payment_valofcare_hosp.csv", stringsAsFactors = FALSE)
# What is being paid for, ex "Payment for heart failure patients" 
# Estimates of payments listed, as well as estimates (maybe estimates given to patients before treatment?) 

d11 <- read.csv("raw/hospital_level_data/timely_effective_care.csv", stringsAsFactors = FALSE)
# Condition and where people went because of it, ex "Emergency Department" for "OP_23 Head CT results"

d12 <- read.csv("raw/hospital_level_data/unplanned_hosp_visits.csv", stringsAsFactors = FALSE)
# Measures "Hospital return days for heart failure patients" , "EDAC_30_HF"



# Create visualizations for PM2 1.1 Data available
# Payment histogram
filtered.d10.payments <- d10 %>% filter(Payment != "Not Available")
filtered.d10.payments$Payment <- as.numeric(gsub("[\\$,]", "", df$col))

filtered.d10.payments$Payment <- as.numeric(filtered.d10.payments$Payment)
unique(filtered.d10.payments$Payment.measure.ID) # 4
filtered.d10.payments <- filtered.d10.payments %>% group_by(Payment.measure.ID) %>% summarise("Median.Payment" = median(Payment, na.rm = TRUE))
barplot(filtered.d10.payments$Median.Payment, names.arg = filtered.d10.payments$Payment.measure.ID)


# Complications histogram
unique(d1$Measure.ID) # 17 measures
d1$Score <- as.numeric(d1$Score)
d1.complications <- d1 %>% group_by(Measure.ID) %>% summarise("Mean.score" = mean(Score, na.rm = TRUE), "Median.score" = median(Score, na.rm = TRUE))
bar.plot <- ggplot(d1.complications, aes(x = Measure.ID, y = Median.score)) +  geom_bar(position="dodge", stat="identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
bar.plot