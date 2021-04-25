#Machine Learning 
#Merrimack College - Autumn 2020


#Invoke the required library packages
# install.packages("data.table")
# install.packages("dplyr")
# install.packages("tidyr")
# install.packages("tidyverse")
# install.packages("scales")
# install.packages("sqldf")
# install.packages("ROCR")
# install.packages("caret")
# install.packages("parallel")
# install.packages("doParallel")
#install.packages("cluster")
library(data.table)
library(dplyr)
library(tidyr)
library(tidyverse)
library(scales)
#library(sqldf)
library(caret)
library(ROCR)
library(parallel)
library(doParallel)
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization
library(ggplot2)
library(purrr)
library(psych)

set.seed(123) 

setwd("D:/Merrimack/DSE6211GA - Machine Learning/Week 6/medical_data/")

rm(datareduced)
data <- read.csv("data.csv") 

rm(d1)
d1 <- data

########## ########## FEATURE ENGINEERING ########## ##########

#Convert all NA entries in race to Other
d1 <- d1 %>% 
  replace_na(list(race ="NA"))
d1$race <- ifelse(d1$race == "NA", "Other", d1$race)

#Remove the three Unknown entries in gender from the data set
d1 <- subset(d1, gender!="Unknown/Invalid")

#Convert Age Range to midpoint values
d1$age <- case_when (
  d1$age =="[0-10)" ~ 5,
  d1$age == "[10-20)" ~ 15,
  d1$age == "[20-30)" ~ 25,
  d1$age == "[30-40)" ~ 35,
  d1$age == "[40-50)" ~ 45,
  d1$age == "[50-60)" ~ 55,
  d1$age == "[60-70)" ~ 65,
  d1$age == "[70-80)" ~ 75,
  d1$age == "[80-90)" ~ 85,
  d1$age == "[90-100)" ~ 95
)

#Regroup Discharge Disposition IDs
d1$admission_type_id <- case_when(
  d1$admission_type_id ==	1	~	1,
  d1$admission_type_id ==	2	~	2,
  d1$admission_type_id ==	3	~	3,
  d1$admission_type_id ==	4	~	4,
  d1$admission_type_id ==	5	~	5,
  d1$admission_type_id ==	6	~	5,
  d1$admission_type_id ==	7	~	6,
  d1$admission_type_id ==	8	~	5
)

#Regroup Discharge Disposition IDs
d1$discharge_disposition_id <- case_when(
  d1$discharge_disposition_id ==	1	~	1,
  d1$discharge_disposition_id ==	3	~	2,
  d1$discharge_disposition_id ==	4	~	3,
  d1$discharge_disposition_id ==	5	~	4,
  d1$discharge_disposition_id ==	6	~	5,
  d1$discharge_disposition_id ==	8	~	5,
  d1$discharge_disposition_id ==	7	~	6,
  d1$discharge_disposition_id ==	9	~	7,
  d1$discharge_disposition_id ==	10	~	8,
  d1$discharge_disposition_id ==	11	~	9,
  d1$discharge_disposition_id ==	19	~	9,
  d1$discharge_disposition_id ==	20	~	9,
  d1$discharge_disposition_id ==	21	~	9,
  d1$discharge_disposition_id ==	12	~	10,
  d1$discharge_disposition_id ==	13	~	11,
  d1$discharge_disposition_id ==	14	~	11,
  d1$discharge_disposition_id ==	15	~	12,
  d1$discharge_disposition_id ==	16	~	12,
  d1$discharge_disposition_id ==	17	~	12,
  d1$discharge_disposition_id ==	18	~	13,
  d1$discharge_disposition_id ==	25	~	13,
  d1$discharge_disposition_id ==	26	~	13,
  d1$discharge_disposition_id ==	2	~	14,
  d1$discharge_disposition_id ==	22	~	14,
  d1$discharge_disposition_id ==	23	~	14,
  d1$discharge_disposition_id ==	24	~	14,
  d1$discharge_disposition_id ==	27	~	14,
  d1$discharge_disposition_id ==	28	~	14,
  d1$discharge_disposition_id ==	29	~	14,
  d1$discharge_disposition_id ==	30	~	14
)

#Remove discharge disposition ids associated with patient death from the data set
d1 <- subset(d1, discharge_disposition_id!=9)

#Regroup Admission Source IDs
d1$admission_source_id <- case_when(
  d1$admission_source_id ==	1	~	1,
  d1$admission_source_id ==	2	~	2,
  d1$admission_source_id ==	3	~	3,
  d1$admission_source_id ==	4	~	4,
  d1$admission_source_id ==	5	~	4,
  d1$admission_source_id ==	6	~	4,
  d1$admission_source_id ==	10	~	4,
  d1$admission_source_id ==	18	~	4,
  d1$admission_source_id ==	7	~	5,
  d1$admission_source_id ==	8	~	6,
  d1$admission_source_id ==	9	~	7,
  d1$admission_source_id ==	15	~	7,
  d1$admission_source_id ==	17	~	7,
  d1$admission_source_id ==	20	~	7,
  d1$admission_source_id ==	21	~	7,
  d1$admission_source_id ==	11	~	8,
  d1$admission_source_id ==	12	~	9,
  d1$admission_source_id ==	13	~	10,
  d1$admission_source_id ==	14	~	11,
  d1$admission_source_id ==	19	~	12,
  d1$admission_source_id ==	22	~	13,
  d1$admission_source_id ==	23	~	14,
  d1$admission_source_id ==	24	~	15,
  d1$admission_source_id ==	25	~	16,
  d1$admission_source_id ==	26	~	17
)

#Convert all NA entries in payer_code to No and all others to Yes
#It's reasonable that patients just won't have an insurance payer and these records should be retained.
d1 <- d1 %>% 
  replace_na(list(payer_code ="NA"))
d1$payer_code <- ifelse(d1$payer_code == "NA", "No", "Yes")

#Convert V* and E* diagnosis codes to numbers
d1$diag_1 <- ifelse(d1$diag_1  %like% "V",1000, d1$diag_1 )
d1$diag_1 <- ifelse(d1$diag_1  %like% "E",2000, d1$diag_1 )

d1$diag_2 <- ifelse(d1$diag_2  %like% "V",1000, d1$diag_2 )
d1$diag_2 <- ifelse(d1$diag_2  %like% "E",2000, d1$diag_2 )

d1$diag_3 <- ifelse(d1$diag_3  %like% "V",1000, d1$diag_3 )
d1$diag_3 <- ifelse(d1$diag_3  %like% "E",2000, d1$diag_3 )

#Convert NAs to 3000
d1$diag_1[is.na(d1$diag_1)]  <-  3000
d1$diag_2[is.na(d1$diag_2)]  <-  3000
d1$diag_3[is.na(d1$diag_3)]  <-  3000

d1$diag_1 <- as.numeric(d1$diag_1)
d1$diag_2 <- as.numeric(d1$diag_2)
d1$diag_3 <- as.numeric(d1$diag_3)


#Remove columns by name
#d1 <- subset(d1, select=-c(diag_11,diag_21, diag_31))

#Convert ICD-9 codes to chapters
d1$D1ch <- case_when(
  d1$diag_1 >= 1 & d1$diag_1 <= 139 ~ "ch1",
  d1$diag_1 >= 140 & d1$diag_1 <= 239 ~ "ch2",
  d1$diag_1 >= 240 & d1$diag_1 <= 279 ~ "ch3",
  d1$diag_1 >= 280 & d1$diag_1 <= 289 ~ "ch4",
  d1$diag_1 >= 290 & d1$diag_1 <= 319 ~ "ch5",
  d1$diag_1 >= 320 & d1$diag_1 <= 389 ~ "ch6",
  d1$diag_1 >= 390 & d1$diag_1 <= 459 ~ "ch7",
  d1$diag_1 >= 460 & d1$diag_1 <= 519 ~ "ch8",
  d1$diag_1 >= 520 & d1$diag_1 <= 579 ~ "ch9",
  d1$diag_1 >= 580 & d1$diag_1 <= 629 ~ "ch10",
  d1$diag_1 >= 630 & d1$diag_1 <= 679 ~ "ch11",
  d1$diag_1 >= 680 & d1$diag_1 <= 709 ~ "ch12",
  d1$diag_1 >= 710 & d1$diag_1 <= 739 ~ "ch13",
  d1$diag_1 >= 740 & d1$diag_1 <= 759 ~ "ch14",
  d1$diag_1 >= 760 & d1$diag_1 <= 779 ~ "ch15",
  d1$diag_1 >= 780 & d1$diag_1 <= 799 ~ "ch16",
  d1$diag_1 >= 800 & d1$diag_1 <= 999 ~ "ch17",
  d1$diag_1 == 1000 ~ "ch18",
  d1$diag_1 == 2000 ~ "ch19",
  d1$diag_1 == 3000 ~ "ch20"
)

d1$D2ch <- case_when(
  d1$diag_2 >= 1 & d1$diag_2 <= 139 ~ "ch1",
  d1$diag_2 >= 140 & d1$diag_2 <= 239 ~ "ch2",
  d1$diag_2 >= 240 & d1$diag_2 <= 279 ~ "ch3",
  d1$diag_2 >= 280 & d1$diag_2 <= 289 ~ "ch4",
  d1$diag_2 >= 290 & d1$diag_2 <= 319 ~ "ch5",
  d1$diag_2 >= 320 & d1$diag_2 <= 389 ~ "ch6",
  d1$diag_2 >= 390 & d1$diag_2 <= 459 ~ "ch7",
  d1$diag_2 >= 460 & d1$diag_2 <= 519 ~ "ch8",
  d1$diag_2 >= 520 & d1$diag_2 <= 579 ~ "ch9",
  d1$diag_2 >= 580 & d1$diag_2 <= 629 ~ "ch10",
  d1$diag_2 >= 630 & d1$diag_2 <= 679 ~ "ch11",
  d1$diag_2 >= 680 & d1$diag_2 <= 709 ~ "ch12",
  d1$diag_2 >= 710 & d1$diag_2 <= 739 ~ "ch13",
  d1$diag_2 >= 740 & d1$diag_2 <= 759 ~ "ch14",
  d1$diag_2 >= 760 & d1$diag_2 <= 779 ~ "ch15",
  d1$diag_2 >= 780 & d1$diag_2 <= 799 ~ "ch16",
  d1$diag_2 >= 800 & d1$diag_2 <= 999 ~ "ch17",
  d1$diag_2 == 1000 ~ "ch18",
  d1$diag_2 == 2000 ~ "ch19",
  d1$diag_2 == 3000 ~ "ch20"
)

d1$D3ch <- case_when(
  d1$diag_3 >= 1 & d1$diag_3 <= 139 ~ "ch1",
  d1$diag_3 >= 140 & d1$diag_3 <= 239 ~ "ch2",
  d1$diag_3 >= 240 & d1$diag_3 <= 279 ~ "ch3",
  d1$diag_3 >= 280 & d1$diag_3 <= 289 ~ "ch4",
  d1$diag_3 >= 290 & d1$diag_3 <= 319 ~ "ch5",
  d1$diag_3 >= 320 & d1$diag_3 <= 389 ~ "ch6",
  d1$diag_3 >= 390 & d1$diag_3 <= 459 ~ "ch7",
  d1$diag_3 >= 460 & d1$diag_3 <= 519 ~ "ch8",
  d1$diag_3 >= 520 & d1$diag_3 <= 579 ~ "ch9",
  d1$diag_3 >= 580 & d1$diag_3 <= 629 ~ "ch10",
  d1$diag_3 >= 630 & d1$diag_3 <= 679 ~ "ch11",
  d1$diag_3 >= 680 & d1$diag_3 <= 709 ~ "ch12",
  d1$diag_3 >= 710 & d1$diag_3 <= 739 ~ "ch13",
  d1$diag_3 >= 740 & d1$diag_3 <= 759 ~ "ch14",
  d1$diag_3 >= 760 & d1$diag_3 <= 779 ~ "ch15",
  d1$diag_3 >= 780 & d1$diag_3 <= 799 ~ "ch16",
  d1$diag_3 >= 800 & d1$diag_3 <= 999 ~ "ch17",
  d1$diag_3 == 1000 ~ "ch18",
  d1$diag_3 == 2000 ~ "ch19",
  d1$diag_3 == 3000 ~ "ch20"
)

#Internet says an A1C result 6.5 or greater indicates diabetes. 
#A score over 7 suggests diabetes that isn't well controlled.
#Since all these patients are supposed to have diabetes, convert this to
d1$A1Cresult <- case_when (
  d1$A1Cresult == ">7" ~ ">7",
  d1$A1Cresult == ">8" ~ ">7",
  d1$A1Cresult == "None" ~ "None",
  d1$A1Cresult == "Norm" ~ "Norm"
)

#Clean up the medical specialty field and enter it as a new column in the data frame
d1$medical_specialty <- case_when(
  d1$medical_specialty ==	"AllergyandImmunology"	~	"Misc",
  d1$medical_specialty ==	"Anesthesiology"	~	"Misc",
  d1$medical_specialty ==	"Anesthesiology-Pediatric"	~	"Pediatrics",
  d1$medical_specialty ==	"Cardiology"	~	"Cardiology",
  d1$medical_specialty ==	"Cardiology-Pediatric"	~	"Pediatrics",
  d1$medical_specialty ==	"DCPTEAM"	~	"Misc",
  d1$medical_specialty ==	"Dentistry"	~	"Misc",
  d1$medical_specialty ==	"Dermatology"	~	"Misc",
  d1$medical_specialty ==	"Emergency/Trauma"	~	"Emergency/Trauma",
  d1$medical_specialty ==	"Endocrinology"	~	"Endocrinology",
  d1$medical_specialty ==	"Endocrinology-Metabolism"	~	"Endocrinology",
  d1$medical_specialty ==	"Family/GeneralPractice"	~	"GP",
  d1$medical_specialty ==	"Gastroenterology"	~	"Gastroenterology",
  d1$medical_specialty ==	"Gynecology"	~	"OBGYN",
  d1$medical_specialty ==	"Hematology"	~	"Hematology",
  d1$medical_specialty ==	"Hematology/Oncology"	~	"Hematology",
  d1$medical_specialty ==	"Hospitalist"	~	"GP",
  d1$medical_specialty ==	"InfectiousDiseases"	~	"Misc",
  d1$medical_specialty ==	"InternalMedicine"	~	"GP",
  d1$medical_specialty ==	"NA"	~	"None",
  d1$medical_specialty ==	"Nephrology"	~	"Nephrology",
  d1$medical_specialty ==	"Neurology"	~	"Neurology",
  d1$medical_specialty ==	"Neurophysiology"	~	"Misc",
  d1$medical_specialty ==	"Obsterics&Gynecology-GynecologicOnco"	~	"OBGYN",
  d1$medical_specialty ==	"Obstetrics"	~	"OBGYN",
  d1$medical_specialty ==	"ObstetricsandGynecology"	~	"OBGYN",
  d1$medical_specialty ==	"Oncology"	~	"Oncology",
  d1$medical_specialty ==	"Ophthalmology"	~	"Misc",
  d1$medical_specialty ==	"Orthopedics"	~	"Orthopedics",
  d1$medical_specialty ==	"Orthopedics-Reconstructive"	~	"Orthopedics",
  d1$medical_specialty ==	"Osteopath"	~	"Misc",
  d1$medical_specialty ==	"Otolaryngology"	~	"Misc",
  d1$medical_specialty ==	"OutreachServices"	~	"Misc",
  d1$medical_specialty ==	"Pathology"	~	"Pathology",
  d1$medical_specialty ==	"Pediatrics"	~	"Pediatrics",
  d1$medical_specialty ==	"Pediatrics-CriticalCare"	~	"Pediatrics",
  d1$medical_specialty ==	"Pediatrics-EmergencyMedicine"	~	"Pediatrics",
  d1$medical_specialty ==	"Pediatrics-Endocrinology"	~	"Pediatrics",
  d1$medical_specialty ==	"Pediatrics-Hematology-Oncology"	~	"Pediatrics",
  d1$medical_specialty ==	"Pediatrics-Neurology"	~	"Pediatrics",
  d1$medical_specialty ==	"Pediatrics-Pulmonology"	~	"Pediatrics",
  d1$medical_specialty ==	"Perinatology"	~	"OBGYN",
  d1$medical_specialty ==	"PhysicalMedicineandRehabilitation"	~	"Misc",
  d1$medical_specialty ==	"PhysicianNotFound"	~	"None",
  d1$medical_specialty ==	"Podiatry"	~	"Podiatry",
  d1$medical_specialty ==	"Proctology"	~	"Proctology",
  d1$medical_specialty ==	"Psychiatry"	~	"Psych",
  d1$medical_specialty ==	"Psychiatry-Addictive"	~	"Psych",
  d1$medical_specialty ==	"Psychiatry-Child/Adolescent"	~	"Pediatrics",
  d1$medical_specialty ==	"Psychology"	~	"Psych",
  d1$medical_specialty ==	"Pulmonology"	~	"Misc",
  d1$medical_specialty ==	"Radiologist"	~	"Misc",
  d1$medical_specialty ==	"Radiology"	~	"Misc",
  d1$medical_specialty ==	"Resident"	~	"GP",
  d1$medical_specialty ==	"Rheumatology"	~	"Misc",
  d1$medical_specialty ==	"Speech"	~	"Misc",
  d1$medical_specialty ==	"SportsMedicine"	~	"Misc",
  d1$medical_specialty ==	"Surgeon"	~	"Surgery",
  d1$medical_specialty ==	"Surgery-Cardiovascular"	~	"Surgery",
  d1$medical_specialty ==	"Surgery-Cardiovascular/Thoracic"	~	"Surgery",
  d1$medical_specialty ==	"Surgery-Colon&Rectal"	~	"Surgery",
  d1$medical_specialty ==	"Surgery-General"	~	"Surgery",
  d1$medical_specialty ==	"Surgery-Maxillofacial"	~	"Surgery",
  d1$medical_specialty ==	"Surgery-Neuro"	~	"Surgery",
  d1$medical_specialty ==	"Surgery-Pediatric"	~	"Surgery",
  d1$medical_specialty ==	"Surgery-Plastic"	~	"Surgery",
  d1$medical_specialty ==	"Surgery-PlasticwithinHeadandNeck"	~	"Surgery",
  d1$medical_specialty ==	"Surgery-Thoracic"	~	"Surgery",
  d1$medical_specialty ==	"Surgery-Vascular"	~	"Surgery",
  d1$medical_specialty ==	"SurgicalSpecialty"	~	"Surgery",
  d1$medical_specialty ==	"Urology"	~	"Misc"
)

d1 <- d1 %>% 
  replace_na(list(medical_specialty ="None"))

#Convert the >30 and <30 rows to Yes
d1$readmitted <- case_when (
  d1$readmitted == ">30" ~ 1,
  d1$readmitted == "<30" ~ 1,
  d1$readmitted == "NO" ~ 0
)

#Count the instances of "Steady" by row across the columns listed below.
d1$SteadyCount <- rowSums(d1[,c("metformin",
                                "repaglinide",
                                "nateglinide",
                                "chlorpropamide",
                                "glimepiride",
                                "acetohexamide",
                                "glipizide",
                                "glyburide",
                                "tolbutamide",
                                "pioglitazone",
                                "rosiglitazone",
                                "acarbose",
                                "miglitol",
                                "troglitazone",
                                "tolazamide",
                                #"examide",
                                #"citoglipton",
                                "insulin",
                                "glyburide.metformin",
                                "glipizide.metformin",
                                #"glimepiride.pioglitazone",
                                "metformin.rosiglitazone",
                                "metformin.pioglitazone"
)] == "Steady")

#Count the instances of "Down" by row across the columns listed below.
d1$DownCount <- rowSums(d1[,c("metformin",
                                "repaglinide",
                                "nateglinide",
                                "chlorpropamide",
                                "glimepiride",
                                "acetohexamide",
                                "glipizide",
                                "glyburide",
                                "tolbutamide",
                                "pioglitazone",
                                "rosiglitazone",
                                "acarbose",
                                "miglitol",
                                "troglitazone",
                                "tolazamide",
                                #"examide",
                                #"citoglipton",
                                "insulin",
                                "glyburide.metformin",
                                "glipizide.metformin",
                                #"glimepiride.pioglitazone",
                                "metformin.rosiglitazone",
                                "metformin.pioglitazone"
)] == "Down")

#Count the instances of "Up" by row across the columns listed below.
d1$UpCount <- rowSums(d1[,c("metformin",
                              "repaglinide",
                              "nateglinide",
                              "chlorpropamide",
                              "glimepiride",
                              "acetohexamide",
                              "glipizide",
                              "glyburide",
                              "tolbutamide",
                              "pioglitazone",
                              "rosiglitazone",
                              "acarbose",
                              "miglitol",
                              "troglitazone",
                              "tolazamide",
                              #"examide",
                              #"citoglipton",
                              "insulin",
                              "glyburide.metformin",
                              "glipizide.metformin",
                              #"glimepiride.pioglitazone",
                              "metformin.rosiglitazone",
                              "metformin.pioglitazone"
)] == "Up")

#Count the instances of "No" by row across the columns listed below.
d1$NoCount <- rowSums(d1[,c("metformin",
                            "repaglinide",
                            "nateglinide",
                            "chlorpropamide",
                            "glimepiride",
                            "acetohexamide",
                            "glipizide",
                            "glyburide",
                            "tolbutamide",
                            "pioglitazone",
                            "rosiglitazone",
                            "acarbose",
                            "miglitol",
                            "troglitazone",
                            "tolazamide",
                            #"examide",
                            #"citoglipton",
                            "insulin",
                            "glyburide.metformin",
                            "glipizide.metformin",
                            #"glimepiride.pioglitazone",
                            "metformin.rosiglitazone",
                            "metformin.pioglitazone"
)] == "No")

#Remove variables we don't want to use as features
drops <- c("encounter_id"
           ,"patient_nbr"
           ,"weight"
           ,"diag_1"
           ,"diag_2"
           ,"diag_3"
           #Dropping to reduce features
           #,"medical_specialty"
           #,"admission_source_id"
           #Dropping max_glu_serum since blood sugar readings depend on
           #many outside factors for accuracy
           #,"max_glu_serum"
           #Dropping as unimportant after 3 runs
           ,"diabetesMed"
           ,"change"
           #Individual blood tests
           ,"metformin"
           ,"repaglinide"
           ,"nateglinide"
           ,"chlorpropamide"
           ,"glimepiride"
           ,"acetohexamide"
           ,"glipizide"
           ,"glyburide"
           ,"tolbutamide"
           ,"pioglitazone"
           ,"rosiglitazone"
           ,"acarbose"
           ,"miglitol"
           ,"troglitazone"
           ,"tolazamide"
           ,"examide"
           ,"citoglipton"
           ,"insulin"
           ,"glyburide.metformin"
           ,"glipizide.metformin"
           ,"glimepiride.pioglitazone"
           ,"metformin.rosiglitazone"
           ,"metformin.pioglitazone"
)
d1 <- d1[ , !(names(d1) %in% drops)]

#Factor Categorical Variables
d1$race<-factor(d1$race)
d1$gender<-factor(d1$gender)
d1$admission_type_id<-factor(d1$admission_type_id)
d1$discharge_disposition_id<-factor(d1$discharge_disposition_id)
d1$admission_source_id<-factor(d1$admission_source_id)
d1$payer_code<-factor(d1$payer_code)
d1$medical_specialty<-factor(d1$medical_specialty)
d1$D1ch <- factor(d1$D1ch)
d1$D2ch <- factor(d1$D2ch)
d1$D3ch <- factor(d1$D3ch)
d1$max_glu_serum<-factor(d1$max_glu_serum)
d1$A1Cresult<-factor(d1$A1Cresult)
#d1$change<-factor(d1$change)
#d1$diabetesMed<-factor(d1$diabetesMed)
d1$readmitted<-factor(d1$readmitted)

d1 <- d1[,c(3,7,10:16,23:26,1,2,4,5,6,8,9,17,18,20:22,19)]

#Numericals are 1:10
#Factors are 11:23

########## CREATE TRAINING AND TEST SETS ###############

#rm(training_ind, training_set, test_set)
training_ind <- createDataPartition(d1$readmitted, p = 0.70, list = FALSE, times = 1) 
training_set <- d1[training_ind, ] 
test_set <- d1[-training_ind, ]

########## ########## TARGET ENCODING ########## ##########

threshold <- 250

target_enc_train <- function(variable, level) {
  
  training_set$readmitted <- as.numeric(as.vector(training_set$readmitted))
  
  train_avg_target <- mean(training_set[, "readmitted"])
  if (nrow(training_set[training_set[, variable]==level, ])==0) {
    return(train_avg_target)
  } else {
    level_num_obs <- nrow(training_set[training_set[, variable]==level, ])
    level_avg_target <- mean(training_set[training_set[, variable]==level, "readmitted"])
    return((level_num_obs*level_avg_target+threshold*train_avg_target)/ (level_num_obs+threshold))
  }
}

#Create Target Variable Versions
#Race
race_target <- mapply(target_enc_train, variable = "race", 
                      level = levels(training_set$race), USE.NAMES = TRUE) 
names(race_target) <- levels(training_set$race)

#Gender
gender_target <- mapply(target_enc_train, variable = "gender", 
                        level = levels(training_set$gender), 
                        USE.NAMES = FALSE) 

names(gender_target) <- levels(training_set$gender)

#admission_type_id
admission_type_id_target <- mapply(target_enc_train, variable = "admission_type_id", 
                                   level = levels(training_set$admission_type_id), 
                                   USE.NAMES = FALSE) 
names(admission_type_id_target) <- levels(training_set$admission_type_id)

#discharge_disposition_id
discharge_disposition_id_target <- mapply(target_enc_train, variable = "discharge_disposition_id", 
                                          level = levels(training_set$discharge_disposition_id), 
                                          USE.NAMES = FALSE) 
names(discharge_disposition_id_target) <- levels(training_set$discharge_disposition_id)

#admission_source_id
admission_source_id_target <- mapply(target_enc_train, variable = "admission_source_id", 
                                     level = levels(training_set$admission_source_id), 
                                     USE.NAMES = FALSE) 
names(admission_source_id_target) <- levels(training_set$admission_source_id)

#payer_code
payer_code_target <- mapply(target_enc_train, variable = "payer_code", 
                            level = levels(training_set$payer_code), 
                            USE.NAMES = FALSE) 
names(payer_code_target) <- levels(training_set$payer_code)

#medical_specialty
medical_specialty_target <- mapply(target_enc_train, variable = "medical_specialty", 
                                   level = levels(training_set$medical_specialty), 
                                   USE.NAMES = FALSE) 
names(medical_specialty_target) <- levels(training_set$medical_specialty)

#max_glu_serum
max_glu_serum_target <- mapply(target_enc_train, variable = "max_glu_serum", 
                               level = levels(training_set$max_glu_serum), 
                               USE.NAMES = FALSE) 
names(max_glu_serum_target) <- levels(training_set$max_glu_serum)

#A1Cresult
A1Cresult_target <- mapply(target_enc_train, variable = "A1Cresult", 
                           level = levels(training_set$A1Cresult), 
                           USE.NAMES = FALSE) 
names(A1Cresult_target) <- levels(training_set$A1Cresult)

#D1ch
D1ch_target <- mapply(target_enc_train, variable = "D1ch", 
                      level = levels(training_set$D1ch), 
                      USE.NAMES = FALSE) 
names(D1ch_target) <- levels(training_set$D1ch)

#D2ch
D2ch_target <- mapply(target_enc_train, variable = "D2ch", 
                      level = levels(training_set$D2ch), 
                      USE.NAMES = FALSE) 
names(D2ch_target) <- levels(training_set$D2ch)

#D3ch
D3ch_target <- mapply(target_enc_train, variable = "D3ch", 
                      level = levels(training_set$D3ch), 
                      USE.NAMES = FALSE) 
names(D3ch_target) <- levels(training_set$D3ch)



#Target Training

#Target Training - race
training_set$race_target <- 0
for (level in levels(training_set$race)) {
  training_set[training_set[, "race"]==level, "race_target"] <-
    race_target[level] }

#Target Training - gender
training_set$gender_target <- 0 
for (level in levels(training_set$gender)) {
  training_set[training_set[, "gender"]==level, "gender_target"] <- 
    gender_target[level] }

#Target Training - admission_type_id
training_set$admission_type_id_target <- 0 
for (level in levels(training_set$admission_type_id)) {
  training_set[training_set[, "admission_type_id"]==level, "admission_type_id_target"] <- 
    admission_type_id_target[level] }

#Target Training - discharge_disposition_id
training_set$discharge_disposition_id_target <- 0 
for (level in levels(training_set$discharge_disposition_id)) {
  training_set[training_set[, "discharge_disposition_id"]==level, "discharge_disposition_id_target"] <- 
    discharge_disposition_id_target[level] }

#Target Training - admission_source_id
training_set$admission_source_id_target <- 0 
for (level in levels(training_set$admission_source_id)) {
  training_set[training_set[, "admission_source_id"]==level, "admission_source_id_target"] <- 
    admission_source_id_target[level] }

#Target Training - payer_code
training_set$payer_code_target <- 0 
for (level in levels(training_set$payer_code)) {
  training_set[training_set[, "payer_code"]==level, "payer_code_target"] <- 
    payer_code_target[level] }

#Target Training - medical_specialty
training_set$medical_specialty_target <- 0 
for (level in levels(training_set$medical_specialty)) {
  training_set[training_set[, "medical_specialty"]==level, "medical_specialty_target"] <- 
    medical_specialty_target[level] }

#Target Training - max_glu_serum
training_set$max_glu_serum_target <- 0 
for (level in levels(training_set$max_glu_serum)) {
  training_set[training_set[, "max_glu_serum"]==level, "max_glu_serum_target"] <- 
    max_glu_serum_target[level] }

#Target Training - A1Cresult
training_set$A1Cresult_target <- 0 
for (level in levels(training_set$A1Cresult)) {
  training_set[training_set[, "A1Cresult"]==level, "A1Cresult_target"] <- 
    A1Cresult_target[level] }

#Target Training - D1ch
training_set$D1ch_target <- 0 
for (level in levels(training_set$D1ch)) {
  training_set[training_set[, "D1ch"]==level, "D1ch_target"] <- 
    D1ch_target[level] }

#Target Training - D2ch
training_set$D2ch_target <- 0 
for (level in levels(training_set$D2ch)) {
  training_set[training_set[, "D2ch"]==level, "D2ch_target"] <- 
    D2ch_target[level] }

#Target Training - D3ch
training_set$D3ch_target <- 0 
for (level in levels(training_set$D3ch)) {
  training_set[training_set[, "D3ch"]==level, "D3ch_target"] <- 
    D3ch_target[level] }


#Target Test

#Target Test - race
test_set$race_target <- 0
for (level in levels(training_set$race)) {
  test_set[test_set[, "race"]==level, "race_target"] <-
    race_target[level] }

#Target Test - gender
test_set$gender_target <- 0 
for (level in levels(training_set$gender)) {
  test_set[test_set[, "gender"]==level, "gender_target"] <- 
    gender_target[level] }

#Target Test - admission_type_id
test_set$admission_type_id_target <- 0 
for (level in levels(training_set$admission_type_id)) {
  test_set[test_set[, "admission_type_id"]==level, "admission_type_id_target"] <- 
    admission_type_id_target[level] }

#Target Test - discharge_disposition_id
test_set$discharge_disposition_id_target <- 0 
for (level in levels(training_set$discharge_disposition_id)) {
  test_set[test_set[, "discharge_disposition_id"]==level, "discharge_disposition_id_target"] <- 
    discharge_disposition_id_target[level] }

#Target Test - admission_source_id
test_set$admission_source_id_target <- 0 
for (level in levels(training_set$admission_source_id)) {
  test_set[test_set[, "admission_source_id"]==level, "admission_source_id_target"] <- 
    admission_source_id_target[level] }

#Target Test - payer_code
test_set$payer_code_target <- 0 
for (level in levels(training_set$payer_code)) {
  test_set[test_set[, "payer_code"]==level, "payer_code_target"] <- 
    payer_code_target[level] }

#Target Test - medical_specialty
test_set$medical_specialty_target <- 0 
for (level in levels(training_set$medical_specialty)) {
  test_set[test_set[, "medical_specialty"]==level, "medical_specialty_target"] <- 
    medical_specialty_target[level] }

#Target Test - max_glu_serum
test_set$max_glu_serum_target <- 0 
for (level in levels(training_set$max_glu_serum)) {
  test_set[test_set[, "max_glu_serum"]==level, "max_glu_serum_target"] <- 
    max_glu_serum_target[level] }

#Target Test - A1Cresult
test_set$A1Cresult_target <- 0 
for (level in levels(training_set$A1Cresult)) {
  test_set[test_set[, "A1Cresult"]==level, "A1Cresult_target"] <- 
    A1Cresult_target[level] }

#Target Test - D1ch
test_set$D1ch_target <- 0 
for (level in levels(training_set$D1ch)) {
  test_set[test_set[, "D1ch"]==level, "D1ch_target"] <- 
    D1ch_target[level] }

#Target Test - D2ch
test_set$D2ch_target <- 0 
for (level in levels(training_set$D2ch)) {
  test_set[test_set[, "D2ch"]==level, "D2ch_target"] <- 
    D2ch_target[level] }

#Target Test - D3ch
test_set$D3ch_target <- 0 
for (level in levels(training_set$D3ch)) {
  test_set[test_set[, "D3ch"]==level, "D3ch_target"] <- 
    D3ch_target[level] }

##########Create K-Means Dataset ##########

#Create data set specifically for K-Means by combining target encoded training and test sets
#I've re-joined the target encoded sets to avoid having to re-code everything for the k-means set
#All that needs to be done now is target-encode the readmitted variable since we definitely want
#our target to be included in the clustering.

kmeansdata <- bind_rows(training_set, test_set)

#Target encode the readmitted variable so it can be included in the k-means
readmitted_target <- mapply(target_enc_train, variable = "readmitted", 
                            level = levels(kmeansdata$readmitted), 
                            USE.NAMES = FALSE) 
names(readmitted_target) <- levels(kmeansdata$readmitted)

kmeansdata$readmitted_target <- 0 
for (level in levels(kmeansdata$readmitted)) {
  kmeansdata[kmeansdata[, "readmitted"]==level, "readmitted_target"] <- readmitted_target[level] }

########### K-Means Analysis ######################
#Create final data set eligible for clustering
cd_seg <- kmeansdata[, -1*c(14:26)]

cd_seg <- subset(kmeansdata, select = c(
  "time_in_hospital",
  "num_lab_procedures",
  "num_procedures",
  "num_medications",
  "number_diagnoses",
  "SteadyCount",
  "NoCount",
  "D2ch_target",
  "discharge_disposition_id_target",
  "admission_source_id_target",
  "readmitted_target"
))

#Rescale the data before input into kmeans for creating clusters
cd_seg_scaled <- as.data.frame(scale(cd_seg))

set.seed(548)
cd_kmeans <- kmeans(cd_seg_scaled, centers = 4, nstart = 25)
#cd_kmeans$betweenss/cd_kmeans$totss
#cd_kmeans

#Silhouette width = Si

#For each observation i, calculate the average dissimilarity ai between i
#and all other points of the cluster to which i belongs.

#For all other clusters C, to which i does not belong, calculate the average 
#dissimilarity d(i,C) of i to all observations of C. The smallest of these 
#d(i,C) is defined as bi=minCd(i,C). 
#The value of bi can be seen as the dissimilarity between i and its "neighbor" 
#cluster, i.e., the nearest one to which it does not belong.
#Finally the silhouette width of the observation i
#is defined by the formula: Si=(bi???ai)/max(ai,bi). 

#Si > 0 means that the observation is well clustered. The closest it is to 1, the best it is clustered.
#Si < 0 means that the observation was placed in the wrong cluster.
#Si = 0 means that the observation is between two clusters.

#Run for Cluster Map - R doesn't like to do it - Run as needed
#sol_test <-fviz_cluster(cd_kmeans, cd_seg_scaled)
#sol_test
#sil <- silhouette(cd_kmeans$cluster, dist(cd_seg_scaled))

########## K-Means Extract Clusters for use in Random Forest ###########

##Apply cluster assignments to cd_seg
ReceiveCluster <- cd_seg
ReceiveCluster$cluster <- cd_kmeans$cluster

##All Cluster data back to the k-means data set
AllFieldsbyCluster <- merge(x = kmeansdata,y = ReceiveCluster,sort = FALSE, no.dups = TRUE, all.x=TRUE, all.y=TRUE)

## This merge results in ~250 duplicate rows since there isn't a proper key used, 
# but considering the size of the data set it's fine. Just remove them with 
# the distinct() function.
AllFieldsbyCluster <- merge(x = kmeansdata ,y = ReceiveCluster,sort = FALSE, no.dups = TRUE, all.x=TRUE, all.y=TRUE)
AllFieldsbyCluster <- AllFieldsbyCluster %>% distinct()

#Here I'm just removing all the non-target encoded variables to make the column selection
#more straight-forward when I run this data set through the random forest.
cluster_set <- AllFieldsbyCluster[c("age", 
                                    "time_in_hospital", 
                                    "num_lab_procedures",
                                    "num_procedures", 
                                    "num_medications",
                                    "number_outpatient",
                                    "number_emergency",
                                    "number_inpatient",
                                    "number_diagnoses",
                                    "SteadyCount",
                                    "DownCount",
                                    "UpCount",
                                    "NoCount",
                                    "race_target",
                                    "gender_target",
                                    "admission_type_id_target",
                                    "discharge_disposition_id_target",
                                    "admission_source_id_target",
                                    "payer_code_target",
                                    "medical_specialty_target",
                                    "max_glu_serum_target",
                                    "A1Cresult_target",
                                    "D1ch_target",
                                    "D2ch_target",
                                    "D3ch_target",
                                    "cluster",
                                    "readmitted")]




########## SCALE TRAINING AND TEST SETS ###############

###---- Scale Random Forest data sets -----###
#we need to manually set the center and scale to the values calculated 
#from the training set.
test_set[, 1:13] <- scale(test_set[, 1:13],
                          center = apply(training_set[, 1:13], 2, mean),
                          scale = apply(training_set[, 1:13], 2, sd)
)

training_set[, 1:13] <- scale(training_set[, 1:13])

#Scale target encoded categorical variables now that they're numbers
#fields 24:35 are the _target fields we want to scale
test_set[, 27:38] <- scale(test_set[, 27:38],
                           center = apply(training_set[, 27:38], 2, mean),
                           scale = apply(training_set[, 27:38], 2, sd)
)
training_set[, 27:38] <- scale(training_set[, 27:38])

###--- Scale for K-means ---###
ktraining_ind <- createDataPartition(cluster_set$readmitted, p = 0.70, list = FALSE, times = 1) 
ktraining_set <- cluster_set[ktraining_ind, ] 
ktest_set <- cluster_set[-ktraining_ind, ]

ktest_set[, c(1:13,26)] <- scale(ktest_set[, c(1:13,26)],
                          center = apply(ktraining_set[, c(1:13,26)], 2, mean),
                          scale = apply(ktraining_set[, c(1:13,26)], 2, sd)
)
ktraining_set[, 1:26] <- scale(ktraining_set[, 1:26])

# rename the levels of the target, so they can be used as valid R variable names 
# by the predict function in caret 
levels(training_set$readmitted)[levels(training_set$readmitted)==0] <- "negative" 
levels(training_set$readmitted)[levels(training_set$readmitted)==1] <- "positive"

levels(ktraining_set$readmitted)[levels(ktraining_set$readmitted)==0] <- "negative" 
levels(ktraining_set$readmitted)[levels(ktraining_set$readmitted)==1] <- "positive"

########## Start Random Forest ##########

#modelLookup("rf")

#3 mtry values centered around sqrt(p) where p is the total number of features.
#In this project, we have 178 features. sprt(178) ~ 13.3 and 13.3/3 is about 4
#This range SHOULD include the target
#We're removing the non-target variables
recommended_mtry <- floor(sqrt(ncol(training_set[, -1*c(14:26)])))

rfGrid <- expand.grid(mtry = c(recommended_mtry-2, recommended_mtry, recommended_mtry+2)) 

#Use the Out-of-the-Bag (oob) method to train the model
rfControl <- trainControl(
                          method = "oob", 
                         classProbs = TRUE,
                         allowParallel = TRUE
                         )
#stopCluster(cluster)
#Used this to see what is actually being produced here
#Remove original factored variables before running the model
#make
# trainx <- training_set[, -1*c(11:21)]

#Set y to our target variable. For the project it should be whatever colnum readmitted is
#You can use the col num reference or the name of the target in double quotes
# trainy <- training_set[, "readmitted"]

rf_oob_te_model <- train(x = training_set[, -1*c(14:26)], y = training_set[, 26], 
                         method = "rf", 
                         tuneGrid = rfGrid, 
                         trControl = rfControl, 
                         importance = TRUE, 
                         trace = FALSE) 

#Using the trained random forest model, we get the predictions of class membership for each 
#observation in the (held-out) test set. 

test_set$prediction_target <- predict(rf_oob_te_model, newdata = test_set[, -1*c(14:26)]) 

#We also get the predicted probabilities of target class membership for each observation in the (held-out) test set.

class_probabilities <- predict(rf_oob_te_model, newdata = test_set[, -1*c(14:26)], type = "prob") 

test_set$class_probabilities_target <- class_probabilities$positive 


#To evaluate the predicted probabilities, we use an ROC curve, AUC, and a calibration curve. 

rocr_pred <- prediction(test_set$class_probabilities_target, test_set$readmitted) 
rocr_roc <- performance(rocr_pred, measure = "tpr", x.measure = "fpr") 
plot(rocr_roc, 
     colorize = TRUE, 
     print.cutoffs.at = seq(0, 1, by = 0.1), 
     text.adj = c(-0.5, 1), 
     lwd = 2) 
abline(a = 0, b = 1)

rocr_auc <- performance(rocr_pred, measure = "auc") 
auc <- rocr_auc@y.values[[1]] 
auc

calibration_curve <- calibration(readmitted ~ class_probabilities_target, 
                                 data = test_set, 
                                 class = 1) 
plot(calibration_curve)

rf_varImp <- varImp(rf_oob_te_model, type = 2) 
plot(rf_varImp)
  
##########PLATT SCALING ##########

t1_ind <- createDataPartition(training_set$readmitted, p = 0.7, list = FALSE, times = 1) 
t1 <- training_set[t1_ind, ] 
t2 <- training_set[-t1_ind, ] 


rf_te_t1 <- train(x = t1[, -1*c(14:26)], y = t1[, 26], 
                      method = "rf", 
                      tuneGrid = rfGrid, 
                      trControl = rfControl, 
                      trace = FALSE) 

rf_te_t1_t2_p <- predict(rf_te_t1, 
                             newdata = t2[, -1*c(14:26)], type = "prob") 

rf_te_platt_model <- train(y ~ x, data = data.frame(x=c(rf_te_t1_t2_p[, "positive"]), 
                                                        y=t2$readmitted), 
                               method="glm", 
                               family="binomial", 
                               trControl = trainControl(classProbs = TRUE), 
                               trace = FALSE) 

rf_onehot_t1_test_p <- predict(rf_te_t1, 
                               newdata = test_set[, -1*c(14:26)], 
                               type = "prob")

class_probabilities_ps <- predict(rf_te_platt_model, 
                               newdata = data.frame(x=c(rf_onehot_t1_test_p[,"positive"])), 
                               type = "prob") 

test_set$class_probs_te_platt <- class_probabilities_ps$positive

preds_list <- list(test_set$class_probabilities_target, test_set$class_probs_te_platt) 

actuals_list <- list(test_set$readmitted, test_set$readmitted) 

m <- length(actuals_list)


rocr_pred_ps <- prediction(preds_list, actuals_list) 
rocr_roc_ps <- performance(rocr_pred_ps, measure = "tpr", x.measure = "fpr")

plot(rocr_roc_ps, col = as.list(1:m), lwd = 2) 
legend(x = "bottomright", 
       legend = c("One-hot", "One-hot w/ Platt"), 
       fill = 1:m) 
abline(a = 0, b = 1)

rocr_auc_ps <- performance(rocr_pred_ps, measure = "auc") 
auc_ps <- rocr_auc_ps@y.values 
auc_ps 

calibration_curve_ps <- calibration(readmitted ~ class_probabilities_target + class_probs_te_platt, 
                                    data = test_set, class = 1) 
plot(calibration_curve_ps, type = "l", auto.key = list(columns = 2, lines = TRUE, points = FALSE))


calibration_curve_ODUP <- calibration(readmitted ~ class_probabilities_target + class_probs_te_platt + class_probs_target_dt + class_probs_target_upt, 
                                    data = test_set, class = 1) 
plot(calibration_curve_ODUP, type = "l", auto.key = list(columns = 4, lines = TRUE, points = FALSE))

########## Down SAMPLING ##########

set.seed(9560)
rm(down_train)
down_train <- downSample(x = training_set[, -1*c(14:26)],
                     y = training_set$readmitted)                         
#table(down_train$Class) 

rm(dtControl)
DTControl <- trainControl(method = "oob",
                          #method = "repeatedcv", 
                          #repeats = 5,
                          classProbs = TRUE,
                          #summaryFunction = twoClassSummary
                          )

rm(down_outside)
set.seed(123)
down_outside <- train(x = down_train[,(1:25)], y = down_train[, 26], 
                    method = "rf",
                    tuneGrid = rfGrid, 
                    importance = TRUE, 
                    trace = FALSE,
                    trControl = DTControl)

test_set$pred_target_dt <- predict(down_outside, newdata = test_set[, -1*c(14:26)]) 
class_probabilities_dt <- predict(down_outside, newdata = test_set[, -1*c(14:26)], type = "prob") 
test_set$class_probs_target_dt <- class_probabilities_dt$positive 

rocr_pred_dt <- prediction(test_set$class_probs_target_dt, test_set$readmitted) 
rocr_roc_dt <- performance(rocr_pred_dt, measure = "tpr", x.measure = "fpr") 
plot(rocr_roc_dt, 
     colorize = TRUE, 
     print.cutoffs.at = seq(0, 1, by = 0.1), 
     text.adj = c(-0.5, 1), 
     lwd = 2) 
abline(a = 0, b = 1)

rocr_auc_dt <- performance(rocr_pred_dt, measure = "auc") 
auc_dt <- rocr_auc_dt@y.values[[1]] 
auc_dt

calibration_curve_dt <- calibration(readmitted ~ class_probs_target_dt, 
                                     data = test_set, 
                                     class = 1) 
plot(calibration_curve_dt)

calibration_curve_origdt <- calibration(readmitted ~ class_probabilities_target + class_probs_target_dt, 
                                    data = test_set, class = 1) 
plot(calibration_curve_origdt, type = "l", auto.key = list(columns = 2, lines = TRUE, points = FALSE))

rf_dt_varImp <- varImp(down_outside, type = 2) 
plot(rf_dt_varImp)

########## UP SAMPLING ###########################################################
#rm(up_train, US1Control, up_outside)
set.seed(5468)
rm(up_train)
up_train <- upSample(x = training_set[, -1*c(14:26)],
                     y = training_set$readmitted)                         
#table(up_train$Class) 

rm(UTControl)
UTControl <- trainControl(method = "oob",
                           # method = "repeatedcv", 
                           # repeats = 5,
                           classProbs = TRUE,
                           #summaryFunction = twoClassSummary
                          )

#rm(up_outside)
set.seed(123)
up_outside <- train(x = up_train[,(1:25)], y = up_train[, 26], 
                    method = "rf",
                    tuneGrid = rfGrid, 
                    importance = TRUE, 
                    trace = FALSE,
                    trControl = UTControl)
                    
test_set$pred_target_upt <- predict(up_outside, newdata = test_set[, -1*c(14:26)]) 
class_probabilities_upt <- predict(up_outside, newdata = test_set[, -1*c(14:26)], type = "prob") 
test_set$class_probs_target_upt <- class_probabilities_upt$positive 

rocr_pred_upt <- prediction(test_set$class_probs_target_upt, test_set$readmitted) 
rocr_roc_upt <- performance(rocr_pred, measure = "tpr", x.measure = "fpr") 
plot(rocr_roc_upt, 
     colorize = TRUE, 
     print.cutoffs.at = seq(0, 1, by = 0.1), 
     text.adj = c(-0.5, 1), 
     lwd = 2) 
abline(a = 0, b = 1)

rocr_auc_upt <- performance(rocr_pred_upt, measure = "auc") 
auc_upt <- rocr_auc_upt@y.values[[1]] 
auc_upt

calibration_curve_upt <- calibration(readmitted ~ class_probs_target_upt, 
                                 data = test_set, 
                                 class = 1) 
plot(calibration_curve_upt)

calibration_curve_origdtupt <- calibration(readmitted ~ class_probabilities_target + class_probs_target_dt + class_probs_target_upt, 
                                        data = test_set, class = 1) 
plot(calibration_curve_origdtupt, type = "l", auto.key = list(columns = 3, lines = TRUE, points = FALSE))

rf_upt_varImp <- varImp(up_outside, type = 2) 
plot(rf_upt_varImp)

########## SMOTE SAMPLING ##########

library(DMwR)

set.seed(9560)
rm(smote_train)

trainprep <- training_set[, -1*c(14:25)]
trainprep <- trainprep[,c(1:13,15:26,14)]

trainprep <- rename(trainprep, Class = readmitted)

smote_train <- SMOTE(Class ~ ., data  = trainprep)                         
#table(smote_train$Class)
smoteControl <- trainControl(method = "oob",
                            #method = "repeatedcv", 
                            #repeats = 5,
                            classProbs = TRUE
                            #summaryFunction = twoClassSummary
                          )

set.seed(5627)
smote_outside <- train(Class ~ ., data = smote_train, 
                       method = "rf",
                       # method = "treebag",
                       # nbagg = 50,
                       # metric = "ROC",
                       importance = TRUE, 
                       trace = FALSE,
                       trControl = smoteControl)


test_set$prediction_target_smote <- predict(smote_outside, newdata = test_set[, -1*c(14:26)]) 
class_probabilities_smote <- predict(smote_outside, newdata = test_set[, -1*c(14:26)], type = "prob") 
test_set$class_probs_target_smote <- class_probabilities_smote$positive 
rocr_pred_smote <- prediction(test_set$class_probs_target_smote, test_set$readmitted) 
rocr_roc_smote <- performance(rocr_pred_smote, measure = "tpr", x.measure = "fpr") 
plot(rocr_roc_smote, 
     colorize = TRUE, 
     print.cutoffs.at = seq(0, 1, by = 0.1), 
     text.adj = c(-0.5, 1), 
     lwd = 2) 
abline(a = 0, b = 1)

rocr_auc_smote <- performance(rocr_pred_smote, measure = "auc") 
auc_smote <- rocr_auc_smote@y.values[[1]] 
auc_smote

calibration_curve_smote <- calibration(readmitted ~ class_probs_target_smote, 
                                     data = test_set, 
                                     class = 1) 
plot(calibration_curve_smote)

########## Run Random Forest with K-MEANS TRAINING AND TEST SETS ###############

krecommended_mtry <- floor(sqrt(ncol(ktraining_set[, 1:26])))

krfGrid <- expand.grid(mtry = c(krecommended_mtry-2, krecommended_mtry, krecommended_mtry+2)) 


#stopCluster(cluster)
#Used this to see what is actually being produced here
#Remove original factored variables before running the model
#make
# trainx <- training_set[, -1*c(11:21)]

#Set y to our target variable. For the project it should be whatever colnum readmitted is
#You can use the col num reference or the name of the target in double quotes
# trainy <- training_set[, "readmitted"]

krf_oob_te_model <- train(x = ktraining_set[, 1:26], y = ktraining_set[, 27], 
                         method = "rf", 
                         tuneGrid = krfGrid, 
                         trControl = rfControl, 
                         importance = TRUE, 
                         trace = FALSE) 

#Using the trained random forest model, we get the predictions of class membership for each 
#observation in the (held-out) test set. 

ktest_set$prediction_target <- predict(krf_oob_te_model, newdata = ktest_set[, 1:26]) 

#We also get the predicted probabilities of target class membership for each observation in the (held-out) test set.

kclass_probabilities <- predict(krf_oob_te_model, newdata = ktest_set[, 1:26], type = "prob") 

ktest_set$kclass_probabilities_target <- kclass_probabilities$positive 


#To evaluate the predicted probabilities, we use an ROC curve, AUC, and a calibration curve. 

krocr_pred <- prediction(ktest_set$kclass_probabilities_target, ktest_set$readmitted) 
krocr_roc <- performance(krocr_pred, measure = "tpr", x.measure = "fpr") 
plot(krocr_roc, 
     colorize = TRUE, 
     print.cutoffs.at = seq(0, 1, by = 0.1), 
     text.adj = c(-0.5, 1), 
     lwd = 2) 
abline(a = 0, b = 1)

krocr_auc <- performance(krocr_pred, measure = "auc") 
kauc <- krocr_auc@y.values[[1]] 
kauc

kcalibration_curve <- calibration(readmitted ~ kclass_probabilities_target, 
                                 data = ktest_set, 
                                 class = 1) 
plot(kcalibration_curve)

krf_varImp <- varImp(krf_oob_te_model, type = 2) 
plot(krf_varImp)

########## K-MEANS RANDOM FOREST PLATT SCALING ##########

kt1_ind <- createDataPartition(ktraining_set$readmitted, p = 0.7, list = FALSE, times = 1) 
kt1 <- ktraining_set[kt1_ind, ] 
kt2 <- ktraining_set[-kt1_ind, ] 


krf_te_t1 <- train(x = kt1[, 1:26], y = kt1[, 27], 
                  method = "rf", 
                  tuneGrid = krfGrid, 
                  trControl = rfControl, 
                  trace = FALSE) 

krf_te_t1_t2_p <- predict(krf_te_t1, 
                         newdata = kt2[, 1:26], type = "prob") 

krf_te_platt_model <- train(y ~ x, data = data.frame(x=c(krf_te_t1_t2_p[, "positive"]), 
                                                    y=kt2$readmitted), 
                           method="glm", 
                           family="binomial", 
                           trControl = trainControl(classProbs = TRUE), 
                           trace = FALSE) 

krf_onehot_t1_test_p <- predict(krf_te_t1, 
                               newdata = ktest_set[, 1:26], 
                               type = "prob")

kclass_probabilities_ps <- predict(krf_te_platt_model, 
                                  newdata = data.frame(x=c(krf_onehot_t1_test_p[,"positive"])), 
                                  type = "prob") 

ktest_set$kclass_probs_te_platt <- kclass_probabilities_ps$positive

kpreds_list <- list(ktest_set$kclass_probabilities_target, ktest_set$kclass_probs_te_platt) 

kactuals_list <- list(ktest_set$readmitted, ktest_set$readmitted) 

km <- length(kactuals_list)


krocr_pred_ps <- prediction(kpreds_list, kactuals_list) 
krocr_roc_ps <- performance(krocr_pred_ps, measure = "tpr", x.measure = "fpr")

plot(krocr_roc_ps, col = as.list(1:km), lwd = 2) 
legend(x = "bottomright", 
       legend = c("One-hot", "One-hot w/ Platt"), 
       fill = 1:m) 
abline(a = 0, b = 1)

krocr_auc_ps <- performance(krocr_pred_ps, measure = "auc") 
kauc_ps <- krocr_auc_ps@y.values 
kauc_ps 

kcalibration_curve_ps <- calibration(readmitted ~ kclass_probabilities_target + kclass_probs_te_platt, 
                                    data = ktest_set, class = 1) 
plot(kcalibration_curve_ps, type = "l", auto.key = list(columns = 2, lines = TRUE, points = FALSE))


calibration_curve_ODUP <- calibration(readmitted ~ class_probabilities_target + class_probs_te_platt + class_probs_target_dt + class_probs_target_upt, 
                                      data = test_set, class = 1) 
plot(calibration_curve_ODUP, type = "l", auto.key = list(columns = 4, lines = TRUE, points = FALSE))



########## Start Neural Network #################

nnGrid <- expand.grid(size = 8:10, decay = 0.2)

nnControl <- trainControl(method = "repeatedcv",
                          repeats = 5,
                          classProbs = TRUE)

nn_te_model <- train(x = training_set[, -1*c(14:26)], y = training_set[, 26], 
                     method = "nnet", 
                     tuneGrid = nnGrid, 
                     trControl = nnControl, 
                     trace = FALSE)

test_set$nn_prediction_target <- predict(nn_te_model, newdata = test_set[, -1*c(14:26)])

nn_class_probs <- predict(nn_te_model, newdata = test_set[, -1*c(14:26)], type = "prob") 

test_set$nn_class_probs_target <- nn_class_probs$positive 

nn_rocr_pred <- prediction(test_set$nn_class_probs_target, test_set$readmitted) 
nn_rocr_roc <- performance(nn_rocr_pred, measure = "tpr", x.measure = "fpr") 
plot(nn_rocr_roc, 
     colorize = TRUE, 
     print.cutoffs.at = seq(0, 1, by = 0.1), 
     text.adj = c(-0.5, 1), 
     lwd = 2) 
abline(a = 0, b = 1)


nn_rocr_auc <- performance(nn_rocr_pred, measure = "auc") 
nn_auc <- nn_rocr_auc@y.values[[1]] 
nn_auc 

nn_calib_curve <- calibration(readmitted ~ nn_class_probs_target, 
                              data = test_set, 
                              class = 1) 
plot(nn_calib_curve)

nn_varImp <- varImp(nn_te_model) 
plot(nn_varImp, top = 22)


setwd("D:/Merrimack/DSE6211GA - Machine Learning/Week 6/Result Data/AUC62469")
write.csv(test_set,'test_set.csv')
write.csv(class_probabilities,'class_probabilities.csv')


########## Start K-means Neural Network #################

nnGrid <- expand.grid(size = 8:10, decay = 0.2)

nnControl <- trainControl(method = "repeatedcv",
                          repeats = 5,
                          classProbs = TRUE)

knn_te_model <- train(x = ktraining_set[, 1:26], y = ktraining_set[, 27], 
                     method = "nnet", 
                     tuneGrid = nnGrid, 
                     trControl = nnControl, 
                     trace = FALSE)

ktest_set$nn_prediction_target <- predict(knn_te_model, newdata = ktest_set[, 1:26])

knn_class_probs <- predict(knn_te_model, newdata = ktest_set[, 1:26], type = "prob") 

ktest_set$knn_class_probs_target <- knn_class_probs$positive 

knn_rocr_pred <- prediction(ktest_set$knn_class_probs_target, ktest_set$readmitted) 
knn_rocr_roc <- performance(knn_rocr_pred, measure = "tpr", x.measure = "fpr") 
plot(knn_rocr_roc, 
     colorize = TRUE, 
     print.cutoffs.at = seq(0, 1, by = 0.1), 
     text.adj = c(-0.5, 1), 
     lwd = 2) 
abline(a = 0, b = 1)


knn_rocr_auc <- performance(knn_rocr_pred, measure = "auc") 
knn_auc <- knn_rocr_auc@y.values[[1]] 
knn_auc 

knn_calib_curve <- calibration(readmitted ~ knn_class_probs_target, 
                              data = ktest_set, 
                              class = 1) 
plot(knn_calib_curve)

knn_varImp <- varImp(knn_te_model) 
plot(knn_varImp, top = 22)

########## K-MEANS NEURAL NETWORK PLATT SCALING ####################################

knn_te_t1 <- train(x = kt1[, 1:26], y = kt1[, 27], 
                   method = "rf", 
                   tuneGrid = krfGrid, 
                   trControl = rfControl, 
                   trace = FALSE) 

knn_te_t1_t2_p <- predict(knn_te_t1, 
                          newdata = kt2[, 1:26], type = "prob") 

knn_te_platt_model <- train(y ~ x, data = data.frame(x=c(knn_te_t1_t2_p[, "positive"]), 
                                                     y=kt2$readmitted), 
                            method="glm", 
                            family="binomial", 
                            trControl = trainControl(classProbs = TRUE), 
                            trace = FALSE) 

knn_onehot_t1_test_p <- predict(knn_te_t1, 
                                newdata = ktest_set[, 1:26], 
                                type = "prob")

knnclass_probabilities_ps <- predict(knn_te_platt_model, 
                                   newdata = data.frame(x=c(knn_onehot_t1_test_p[,"positive"])), 
                                   type = "prob") 

ktest_set$knnclass_probs_te_platt <- knnclass_probabilities_ps$positive

knnpreds_list <- list(ktest_set$knnclass_probs, ktest_set$knnclass_probs_te_platt) 

knnactuals_list <- list(ktest_set$readmitted, ktest_set$readmitted) 

knnm <- length(knnactuals_list)


knnrocr_pred_ps <- prediction(knnpreds_list, knnactuals_list) 
knnrocr_roc_ps <- performance(knnrocr_pred_ps, measure = "tpr", x.measure = "fpr")

plot(knnrocr_roc_ps, col = as.list(1:knnm), lwd = 2) 
legend(x = "bottomright", 
       legend = c("One-hot", "One-hot w/ Platt"), 
       fill = 1:m) 
abline(a = 0, b = 1)

knnrocr_auc_ps <- performance(knnrocr_pred_ps, measure = "auc") 
knnauc_ps <- knnrocr_auc_ps@y.values 
knnauc_ps 

kcalibration_curve_ps <- calibration(readmitted ~ knn_class_probs_target + knnclass_probs_te_platt, 
                                     data = ktest_set, class = 1) 
plot(kcalibration_curve_ps, type = "l", auto.key = list(columns = 2, lines = TRUE, points = FALSE))


########## Additional Metrics ###########

rm(d2)
d2<-kmeansdata
hist(d2$age, main="Patient Age Distribution", xlab = "Age Group", ylab="Customer Count", col="darkgreen",)

df_gender <- d2 %>% 
  group_by(gender) %>%
  count()

ggplot(df_gender, aes(gender,n)) +
  geom_col(fill = "darkblue")+
  labs(title="Patient Gender Distribution", y = "Patient Count") +
  theme(plot.title = element_text(hjust = 0.5))

df_race <- d2 %>% 
  group_by(race) %>%
  count()

ggplot(df_race, aes(race, n)) +
  geom_col(fill = "purple") +
  labs(title="Patient Race Distribution", y = "Patient Count") +
  theme(plot.title = element_text(hjust = 0.5))

df_payer <- d1 %>%
  group_by(payer_code) %>%
  count()


ggplot(df_payer, aes(payer_code, n)) +
  geom_col(fill = "darkred") +
  labs(title="Patient Payer Distribution", x= "Has Payer Code", y = "Patient Count") +
  theme(plot.title = element_text(hjust = 0.5))

########### K-Means Cluster Analysis ##########

##Create a  dataframe for each cluster
#We don't really need this right now
# CL1 <- ReceiveCluster %>% filter(ReceiveCluster$cluster == 1)
# CL2 <- ReceiveCluster %>% filter(ReceiveCluster$cluster == 2)
# CL3 <- ReceiveCluster %>% filter(ReceiveCluster$cluster == 3)
# CL4 <- ReceiveCluster %>% filter(ReceiveCluster$cluster == 4)
# 
# summary(CL1)
# summary(CL2)
# summary(CL3)
# summary(CL4)

readmitCounts <- cluster_set %>%
  group_by(cluster, readmitted) %>%
  count()

#Count Readmits by cluster
readmitCounts$readmitted <- case_when (
  readmitCounts$readmitted == 0 ~ "No",
  readmitCounts$readmitted == 1 ~ "Yes"
)

#Plot readmits by cluster
ggplot(readmitCounts, mapping = aes(x = cluster,weight=n)) +
  geom_bar(aes(fill = readmitted),position = "dodge") +
  labs(title="Cluster Readmission Distribution", x = "Cluster Assignment", y = "Patient Record Count") +
  theme(plot.title = element_text(hjust = 0.5))

