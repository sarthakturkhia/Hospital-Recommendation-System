library(dplyr)
library(gmapsdistance)
library(ggmap)

# Following code queries Google and adds geocache information to each hospital

hospital.df <- read.csv('hospital-master.csv', stringsAsFactors = FALSE)[,seq(1,8,1)]
hospital.df$adress_search <- paste(hospital.df$Hospital.Name, hospital.df$Address, hospital.df$City, hospital.df$State, sep=", ")

register_google(key = "AIzaSyCxV8qlr-RlmIg6PQqjuarnkAvUqe6bY4Q", account_type = "premium", day_limit = 10000)

for(i in 1:nrow(hospital.df))
{
  result <- geocode(hospital.df$adress_search[i], output = "latlona", source = "google")
  hospital.df$lon[i] <- as.numeric(result[1])
  hospital.df$lat[i] <- as.numeric(result[2])
  hospital.df$geoAddress[i] <- as.character(result[3])
}

write.csv(hospital.df, "hospital-lat-long-address.csv")

# WARNING! When run on 02-Mar-2018; NO DATA FOR-
#
# row_number, provider_number, hospital_name
# 194, 40016, UAMS MEDICAL CENTER
# 260, 43300, ARKANSAS CHILDREN'S HOSPITAL
# 2358, 251304, TALLAHATCHIE CRITICAL ACCESS HOSPITAL
# 2528, 271315, P H S INDIAN HOSPITAL-FT BELKNAP AT HARLEM - CAH
# 2802, 320070, DHHS USPHS INDIAN HEALTH SERVICES
# 3669, 400009, SANTA ROSA CLINIC
# 3672, 400012, HOSPITAL ONCOLOGICO DR ISAAC GONZALEZ MARTINEZ
# 3676, 400016, AUXILIO MUTUO HOSPITAL
# 3677, 400018, MENNONITE GENERAL HOSPITAL INC
# 3678, 400019, HOSPITAL PAVIA SANTURCE
# 3684, 400048, HOSPITAL EPISCOPAL SAN LUCAS GUAYAMA INC
# 3686, 400079, HOSP COMUNITARIO BUEN SAMARITANO
# 3691, 400104, HOSPITAL MENONITA CAGUAS INC
# 3693, 400106, METROPOLITAN HOSPITAL
# 3707, 400124, CENTRO CARDIOVASCULAR
# 3710, 400127, ADMIN DE SERVICIOS MEDICOS PUERTO RIC
# 3815, 431311, SANFORD HOSPITAL WEBSTER - CAH
# 3817, 431313, FREEMAN MEDICAL CENTER - CAH
# 3818, 431314, BENNETT COUNTY HOSPITAL AND NURSING HOME - CAH
# 3822, 431318, BOWDLE HOSPITAL - CAH
# 4633, 430081, PHS INDIAN HOSPITAL AT PINE RIDGE
# 4786, 670099, EMIL J FREIREICH CANCER CENTER
#
# Observation- Many hospitals not found/ having issues are from PR

