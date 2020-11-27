#load necessary packages
library(tidyverse)
library(plyr)
library(reshape2)
library(scales)
library(readxl)
library(unpivotr)
library(Rmisc)
library(xlsx)
library(extrafont)
library(ggthemes)
library(likert)

# 1. Merge the datasets from two different rounds to create one final dataset for analysis
#upload data
rti_data<-read.csv("KCDMS_agri-nutrition_survey_29 May 2020.csv")
rti_data_child<-read.csv("KCDMS_child_survey_data.csv")

#merge datasets
rti_data$unique<-paste(rti_data$respname, rti_data$phonenum, sep= ",")
rti_data_child$unique<-paste(rti_data_child$pull_name, rti_data_child$pull_phonenum, sep= ",")
rti_complete <- merge(rti_data, rti_data_child, by.x = c("unique"), by.y = c("unique"), all.x = TRUE)

#Combine variables about number of children <2yrs from the two surveys
write.csv(rti_complete, "rti_complete.csv")
#Copy/paste variables about number of children <6 and betw. 6 and 23 months in Excel, replace NA's with 0s and save as xls
rti_complete<-read_excel("rti_complete_c.xlsx")
#Convert several variables into numerics
cols<-c(50:73, 150:151, 297:496, 555:681)
rti_complete[,cols] <- lapply(rti_complete[,cols], as.numeric)

# 2. Create Indices and Dummys
#Create Index for women's DDS 
rti_complete$DDS_resp <- (as.numeric(rti_complete$secea_totalscore_24hr) + as.numeric(rti_complete$secea_totalscore_72hr)) / 2
#Use 24hr recall score if it's bigger than 72hr recall score
rti_complete <- rti_complete %>% 
  mutate(DDS_resp = ifelse(secea_totalscore_24hr > secea_totalscore_72hr, secea_totalscore_24hr, DDS_resp))
#Create dummy for women's DDS (0=below acceptable score)
rti_complete$DDS_resp_dummy  <- as.numeric(rti_complete$DDS_resp >= 5)
rti_complete$DDS_resp_dummy  <- mapvalues(rti_complete$DDS_resp_dummy, from = c("0", "1"), to = c("Non acceptable", "Minimum acceptable"))

#Create Index for children's DDS
#Convert character variables in to numeric - not complete yet
rti_complete$score_male_1_24 <- rowSums(rti_complete[, c("grain_food_f05__1.x", "beans_food_f08__1.x", "dairy_food_f11__1.x", "meatfi_food_f14__1.x", "eggs_food_f17__1.x", "fruits_food_f20__1.x", "other_fruits_food_f23__1.x")], na.rm=FALSE) 
rti_complete$score_male_1_72 <- rowSums(rti_complete[,c("grain_food_f06__1.x", "beans_food_f09__1.x", "dairy_food_f12__1.x", "meatfi_food_f15__1.x", "eggs_food_f18__1.x", "fruits_food_f21__1.x", "other_fruits_food_f24__1.x")], na.rm=FALSE) 
rti_complete$C_DDS_male_1 <- rowSums(rti_complete[,c("score_male_1_24", "score_male_1_72")], na.rm=FALSE)  / 2
rti_complete <- rti_complete %>% 
  mutate(C_DDS_male_1 = ifelse(score_male_1_24 > score_male_1_72, score_male_1_24, C_DDS_male_1))

rti_complete$score_male_2_24 <- rowSums(rti_complete[, c("grain_food_f05__2.x", "beans_food_f08__2.x", "dairy_food_f11__2.x", "meatfi_food_f14__2.x", "eggs_food_f17__2.x", "fruits_food_f20__2.x", "other_fruits_food_f23__2.x")], na.rm=FALSE) 
rti_complete$score_male_2_72 <- rowSums(rti_complete[,c("grain_food_f06__2.x", "beans_food_f09__2.x", "dairy_food_f12__2.x", "meatfi_food_f15__2.x", "eggs_food_f18__2.x", "fruits_food_f21__2.x", "other_fruits_food_f24__2.x")], na.rm=FALSE) 
rti_complete$C_DDS_male_2 <- rowSums(rti_complete[,c("score_male_2_24", "score_male_2_72")], na.rm=FALSE)  / 2
rti_complete <- rti_complete %>% 
  mutate(C_DDS_male_2 = ifelse(score_male_2_24 > score_male_2_72, score_male_2_24, C_DDS_male_2))

rti_complete$score_male_3_24 <- rowSums(rti_complete[,c("grain_food_f05__1.y", "beans_food_f08__1.y", "dairy_food_f11__1.y", "meatfi_food_f14__1.y", "eggs_food_f17__1.y", "fruits_food_f20__1.y", "other_fruits_food_f23__1.y")], na.rm=FALSE)
rti_complete$score_male_3_72 <- rowSums(rti_complete[,c("grain_food_f06__1.y", "beans_food_f09__1.y", "dairy_food_f12__1.y", "meatfi_food_f15__1.y", "eggs_food_f18__1.y", "fruits_food_f21__1.y", "other_fruits_food_f24__1.y")], na.rm=FALSE) 
rti_complete$C_DDS_male_3 <- rowSums(rti_complete[,c("score_male_3_24", "score_male_3_72")], na.rm=FALSE)  / 2
rti_complete <- rti_complete %>% 
  mutate(C_DDS_male_3 = ifelse(score_male_3_24 > score_male_3_72, score_male_3_24, C_DDS_male_3))

rti_complete$score_male_4_24 <- rowSums(rti_complete[,c("grain_food_f05__2.y", "beans_food_f08__2.y", "dairy_food_f11__2.y", "meatfi_food_f14__2.y", "eggs_food_f17__2.y", "fruits_food_f20__2.y", "other_fruits_food_f23__2.y")], na.rm=FALSE)
rti_complete$score_male_4_72 <- rowSums(rti_complete[,c("grain_food_f06__2.y", "beans_food_f09__2.y", "dairy_food_f12__2.y", "meatfi_food_f15__2.y", "eggs_food_f18__2.y", "fruits_food_f21__2.y", "other_fruits_food_f24__2.y")], na.rm=FALSE) 
rti_complete$C_DDS_male_4 <- rowSums(rti_complete[,c("score_male_4_24", "score_male_4_72")], na.rm=FALSE)  / 2
rti_complete <- rti_complete %>% 
  mutate(C_DDS_male_4 = ifelse(score_male_4_24 > score_male_4_72, score_male_4_24, C_DDS_male_4))

rti_complete$score_male_5_24 <- rowSums(rti_complete[,c("grain_food_f05__3", "beans_food_f08__3", "dairy_food_f11__3", "meatfi_food_f14__3", "eggs_food_f17__3", "fruits_food_f20__3", "other_fruits_food_f23__3")], na.rm=FALSE)
rti_complete$score_male_5_72 <- rowSums(rti_complete[,c("grain_food_f06__3", "beans_food_f09__3", "dairy_food_f12__3", "meatfi_food_f15__3", "eggs_food_f18__3", "fruits_food_f21__3", "other_fruits_food_f24__3")], na.rm=FALSE) 
rti_complete$C_DDS_male_5 <- rowSums(rti_complete[,c("score_male_2_24", "score_male_2_72")], na.rm=FALSE)  / 2
rti_complete <- rti_complete %>% 
  mutate(C_DDS_male_5 = ifelse(score_male_5_24 > score_male_5_72, score_male_5_24, C_DDS_male_5))

rti_complete$score_female_1_24 <- rowSums(rti_complete[, c("grain_food_f_f05_f__1.x", "beans_food_f_f08_f__1.x", "dairy_food_f_f11_f__1.x", "meatfi_food_f_f14_f__1.x", "eggs_food_f_f17_f__1.x", "fruits_food_f_f20_f__1.x", "other_fruits_food_f_f23_f__1.x")], na.rm=FALSE) 
rti_complete$score_female_1_72 <- rowSums(rti_complete[,c("grain_food_f_f06_f__1.x", "beans_food_f_f09_f__1.x", "dairy_food_f_f12_f__1.x", "meatfi_food_f_f15_f__1.x", "eggs_food_f_f18_f__1.x", "fruits_food_f_f21_f__1.x", "other_fruits_food_f_f24_f__1.x")], na.rm=FALSE) 
rti_complete$C_DDS_female_1 <- rowSums(rti_complete[,c("score_female_1_24", "score_female_1_72")], na.rm=FALSE)  / 2
rti_complete <- rti_complete %>% 
  mutate(C_DDS_female_1 = ifelse(score_female_1_24 > score_female_1_72, score_female_1_24, C_DDS_female_1))

rti_complete$score_female_2_24 <- rowSums(rti_complete[, c("grain_food_f_f05_f__2.x", "beans_food_f_f08_f__2.x", "dairy_food_f_f11_f__2.x", "meatfi_food_f_f14_f__2.x", "eggs_food_f_f17_f__2.x", "fruits_food_f_f20_f__2.x", "other_fruits_food_f_f23_f__2.x")], na.rm=FALSE) 
rti_complete$score_female_2_72 <- rowSums(rti_complete[,c("grain_food_f_f06_f__2.x", "beans_food_f_f09_f__2.x", "dairy_food_f_f12_f__2.x", "meatfi_food_f_f15_f__2.x", "eggs_food_f_f18_f__2.x", "fruits_food_f_f21_f__2.x", "other_fruits_food_f_f24_f__2.x")], na.rm=FALSE) 
rti_complete$C_DDS_female_2 <- rowSums(rti_complete[,c("score_female_2_24", "score_female_2_72")], na.rm=FALSE)  / 2
rti_complete <- rti_complete %>% 
  mutate(C_DDS_female_2 = ifelse(score_female_2_24 > score_female_2_72, score_female_2_24, C_DDS_female_2))

rti_complete$score_female_3_24 <- rowSums(rti_complete[, c("grain_food_f_f05_f__3.x", "beans_food_f_f08_f__3.x", "dairy_food_f_f11_f__3.x", "meatfi_food_f_f14_f__3.x", "eggs_food_f_f17_f__3.x", "fruits_food_f_f20_f__3.x", "other_fruits_food_f_f23_f__3.x")], na.rm=FALSE) 
rti_complete$score_female_3_72 <- rowSums(rti_complete[,c("grain_food_f_f06_f__3.x", "beans_food_f_f09_f__3.x", "dairy_food_f_f12_f__3.x", "meatfi_food_f_f15_f__3.x", "eggs_food_f_f18_f__3.x", "fruits_food_f_f21_f__3.x", "other_fruits_food_f_f24_f__3.x")], na.rm=FALSE) 
rti_complete$C_DDS_female_3 <- rowSums(rti_complete[,c("score_female_3_24", "score_female_3_72")], na.rm=FALSE)  / 2
rti_complete <- rti_complete %>% 
  mutate(C_DDS_female_3 = ifelse(score_female_3_24 > score_female_3_72, score_female_3_24, C_DDS_female_3))

rti_complete$score_female_4_24 <- rowSums(rti_complete[, c("grain_food_f_f05_f__1.y", "beans_food_f_f08_f__1.y", "dairy_food_f_f11_f__1.y", "meatfi_food_f_f14_f__1.y", "eggs_food_f_f17_f__1.y", "fruits_food_f_f20_f__1.y", "other_fruits_food_f_f23_f__1.y")], na.rm=FALSE) 
rti_complete$score_female_4_72 <- rowSums(rti_complete[,c("grain_food_f_f06_f__1.y", "beans_food_f_f09_f__1.y", "dairy_food_f_f12_f__1.y", "meatfi_food_f_f15_f__1.y", "eggs_food_f_f18_f__1.y", "fruits_food_f_f21_f__1.y", "other_fruits_food_f_f24_f__1.y")], na.rm=FALSE) 
rti_complete$C_DDS_female_4 <- rowSums(rti_complete[,c("score_female_4_24", "score_female_4_72")], na.rm=FALSE)  / 2
rti_complete <- rti_complete %>% 
  mutate(C_DDS_female_4 = ifelse(score_female_4_24 > score_female_4_72, score_female_4_24, C_DDS_female_4))

rti_complete$score_female_5_24 <- rowSums(rti_complete[, c("grain_food_f_f05_f__2.y", "beans_food_f_f08_f__2.y", "dairy_food_f_f11_f__2.y", "meatfi_food_f_f14_f__2.y", "eggs_food_f_f17_f__2.y", "fruits_food_f_f20_f__2.y", "other_fruits_food_f_f23_f__2.y")], na.rm=FALSE) 
rti_complete$score_female_5_72 <- rowSums(rti_complete[,c("grain_food_f_f06_f__2.y", "beans_food_f_f09_f__2.y", "dairy_food_f_f12_f__2.y", "meatfi_food_f_f15_f__2.y", "eggs_food_f_f18_f__2.y", "fruits_food_f_f21_f__2.y", "other_fruits_food_f_f24_f__2.y")], na.rm=FALSE) 
rti_complete$C_DDS_female_5 <- rowSums(rti_complete[,c("score_female_5_24", "score_female_5_72")], na.rm=FALSE)  / 2
rti_complete <- rti_complete %>% 
  mutate(C_DDS_female_5 = ifelse(score_female_5_24 > score_female_5_72, score_female_5_24, C_DDS_female_5))

rti_complete$score_female_6_24 <- rowSums(rti_complete[, c("grain_food_f_f05_f__3.y", "beans_food_f_f08_f__3.y", "dairy_food_f_f11_f__3.y", "meatfi_food_f_f14_f__3.y", "eggs_food_f_f17_f__3.y", "fruits_food_f_f20_f__3.y", "other_fruits_food_f_f23_f__3.y")], na.rm=FALSE) 
rti_complete$score_female_6_72 <- rowSums(rti_complete[,c("grain_food_f_f06_f__3.y", "beans_food_f_f09_f__3.y", "dairy_food_f_f12_f__3.y", "meatfi_food_f_f15_f__3.y", "eggs_food_f_f18_f__3.y", "fruits_food_f_f21_f__3.y", "other_fruits_food_f_f24_f__3.y")], na.rm=FALSE) 
rti_complete$C_DDS_female_6 <- rowSums(rti_complete[,c("score_female_6_24", "score_female_6_72")], na.rm=FALSE)  / 2
rti_complete <- rti_complete %>% 
  mutate(C_DDS_female_6 = ifelse(score_female_6_24 > score_female_6_72, score_female_6_24, C_DDS_female_6))

#Create new variables for knowledge questions and convert answers to correct and wrong
rti_complete$q1 <- rti_complete$know_general_a23
rti_complete$q2 <- rti_complete$know_general_a24
rti_complete$q3 <- rti_complete$know_general_a25
rti_complete$q4 <- rti_complete$know_general_a26
rti_complete$q5 <- rti_complete$know_general_a27
rti_complete$q6 <- rti_complete$know_nutrition_a28
rti_complete$q7 <- rti_complete$know_nutrition_a29
rti_complete$q8 <- rti_complete$know_nutrition_a30
rti_complete$q9 <- rti_complete$know_nutrition_a31
rti_complete$q10 <- rti_complete$know_infant_a32
rti_complete$q11 <- rti_complete$know_infant_a33
rti_complete$q12 <- rti_complete$know_infant_a34
rti_complete$q13 <- rti_complete$know_infant_a35
rti_complete$q14 <- rti_complete$know_infant_a36
rti_complete$q15 <- rti_complete$know_infant_a37
rti_complete$q16 <- rti_complete$know_infant_a38
rti_complete$q17 <- rti_complete$know_infant_a39
rti_complete$q18 <- rti_complete$know_infant_a40
rti_complete$q19 <- rti_complete$know_infant_a41
rti_complete$q20 <- rti_complete$know_infant_a42
rti_complete$q21 <- rti_complete$know_infant_a43

true_correct <- function(x) { mapvalues(x, from = c("TRUE", "Don't Know", "FALSE"), to = c("Correct answer", "Don't know", "Wrong answer")) }
true_false <- function(x) { mapvalues(x, from = c("TRUE", "Don't Know", "FALSE"), to = c("Wrong answer", "Don't know", "Correct answer")) }
vars_true_correct <- c("q1", "q2", "q3", "q8", "q11", "q14", "q17", "q18", "q19", "q20")
vars_true_false <- c("q4", "q5", "q6", "q7", "q9", "q10", "q12", "q13", "q15", "q16", "q21")
rti_complete <- rti_complete %>% mutate_at(vars_true_correct, true_correct)
rti_complete <- rti_complete %>% mutate_at(vars_true_false, true_false)

#Create knowledge indices
rti_complete <- rti_complete %>% 
  mutate(know_general_a23 = ifelse(know_general_a23 == "TRUE", 1, 0))
rti_complete <- rti_complete %>% 
  mutate(know_general_a24 = ifelse(know_general_a24 == "TRUE", 1, 0))
rti_complete <- rti_complete %>% 
  mutate(know_general_a25 = ifelse(know_general_a25 == "TRUE", 1, 0))
rti_complete <- rti_complete %>% 
  mutate(know_nutrition_a30 = ifelse(know_nutrition_a30 == "TRUE", 1, 0))
rti_complete <- rti_complete %>% 
  mutate(know_infant_a33 = ifelse(know_infant_a33 == "TRUE", 1, 0))
rti_complete <- rti_complete %>% 
  mutate(know_infant_a36 = ifelse(know_infant_a36 == "TRUE", 1, 0))
rti_complete <- rti_complete %>% 
  mutate(know_infant_a39 = ifelse(know_infant_a39 == "TRUE", 1, 0))
rti_complete <- rti_complete %>% 
  mutate(know_infant_a40 = ifelse(know_infant_a40 == "TRUE", 1, 0))
rti_complete <- rti_complete %>% 
  mutate(know_infant_a41 = ifelse(know_infant_a41 == "TRUE", 1, 0))
rti_complete <- rti_complete %>% 
  mutate(know_infant_a42 = ifelse(know_infant_a42 == "TRUE", 1, 0))
rti_complete <- rti_complete %>% 
  mutate(know_general_a26 = ifelse(know_general_a26 == "FALSE", 1, 0))
rti_complete <- rti_complete %>% 
  mutate(know_general_a27 = ifelse(know_general_a27 == "FALSE", 1, 0))
rti_complete <- rti_complete %>% 
  mutate(know_nutrition_a28 = ifelse(know_nutrition_a28 == "FALSE", 1, 0))
rti_complete <- rti_complete %>% 
  mutate(know_nutrition_a29 = ifelse(know_nutrition_a29 == "FALSE", 1, 0))
rti_complete <- rti_complete %>% 
  mutate(know_nutrition_a31 = ifelse(know_nutrition_a31 == "FALSE", 1, 0))
rti_complete <- rti_complete %>% 
  mutate(know_infant_a32 = ifelse(know_infant_a32 == "FALSE", 1, 0))
rti_complete <- rti_complete %>% 
  mutate(know_infant_a34 = ifelse(know_infant_a34 == "FALSE", 1, 0))
rti_complete <- rti_complete %>% 
  mutate(know_infant_a35 = ifelse(know_infant_a35 == "FALSE", 1, 0))
rti_complete <- rti_complete %>% 
  mutate(know_infant_a37 = ifelse(know_infant_a37 == "FALSE", 1, 0))
rti_complete <- rti_complete %>% 
  mutate(know_infant_a38 = ifelse(know_infant_a38 == "FALSE", 1, 0))
rti_complete <- rti_complete %>% 
  mutate(know_infant_a43 = ifelse(know_infant_a43 == "FALSE", 1, 0))

#Create General Nutrition Index
rti_complete$index_k_general_nutrition <- rti_complete$know_general_a23 + rti_complete$know_general_a24 + rti_complete$know_general_a25 + rti_complete$know_general_a26 + rti_complete$know_general_a27

#Create Index on Knowledge about Nutrition for Women of Reproductive age
rti_complete$index_k_reproductive_women <- rti_complete$know_nutrition_a28 + rti_complete$know_nutrition_a29 + rti_complete$know_nutrition_a30 + rti_complete$know_nutrition_a31 

#Create Index on Knowledge about Infant Nutrition
rti_complete$index_k_infant_nutrition <- rti_complete$know_infant_a32 + rti_complete$know_infant_a33 + rti_complete$know_infant_a34 + rti_complete$know_infant_a35 + rti_complete$know_infant_a36 + rti_complete$know_infant_a37 + rti_complete$know_infant_a38 + rti_complete$know_infant_a39 + rti_complete$know_infant_a40 + rti_complete$know_infant_a41 + rti_complete$know_infant_a42 + rti_complete$know_infant_a43

#Create Index on Overall nutritional knowledge
rti_complete$index_k_nutrition_overall <- rti_complete$index_k_general_nutrition + rti_complete$index_k_reproductive_women + rti_complete$index_k_infant_nutrition

#Transform Meal frequency vars
fun_f1 <- function(x) { factor(x, levels = c("Every Day", "Three times a week", "Once a week", "Once a month", "Never"), ordered = TRUE) }
vars_meal_frequency <- c("secb_secb1_b01", "secb_secb1_b02", "secb_secb1_b03", "secb_secb1_b04", "secb_secb1_b05")
rti_complete <- rti_complete %>% mutate_at(vars_meal_frequency, fun_f1)

#Save Economic Empowerment vars
rti_complete$e1 <- rti_complete$secc_decision_c01
rti_complete$e2 <- rti_complete$secc_decision_c02
rti_complete$e3 <- rti_complete$secc_decision_c03
rti_complete$e4 <- rti_complete$secc_decision_c04
rti_complete$e5 <- rti_complete$secc_decision_c05
rti_complete$e6 <- rti_complete$secc_decision_c06
rti_complete$e7 <- rti_complete$secc_access_info_c07
rti_complete$e8 <- rti_complete$secc_access_info_c08
rti_complete$e9 <- rti_complete$secc_access_info_c09
rti_complete$e10 <- rti_complete$secc_access_info_c10
rti_complete$e11 <- rti_complete$secc_access_info_c11

#Create Women's Economic Empowerment Index
#Convert answers from factors into numbers from 0 to 4
rti_complete$secc_decision_c01 <- as.numeric(as.character(mapvalues(rti_complete$secc_decision_c01, c("Male decision", "Mostly male", "Joint", "Mostly female", "Female decision"), c(0, 1, 2, 3, 4))))
rti_complete$secc_decision_c02 <- as.numeric(as.character(mapvalues(rti_complete$secc_decision_c02, c("Male decision", "Mostly male", "Joint", "Mostly female", "Female decision"), c(0, 1, 2, 3, 4))))
rti_complete$secc_decision_c03 <- as.numeric(as.character(mapvalues(rti_complete$secc_decision_c03, c("Male decision", "Mostly male", "Joint", "Mostly female", "Female decision"), c(0, 1, 2, 3, 4))))
rti_complete$secc_decision_c04 <- as.numeric(as.character(mapvalues(rti_complete$secc_decision_c04, c("Male decision", "Mostly male", "Joint", "Mostly female", "Female decision"), c(0, 1, 2, 3, 4))))
rti_complete$secc_decision_c05 <- as.numeric(as.character(mapvalues(rti_complete$secc_decision_c05, c("Male decision", "Mostly male", "Joint", "Mostly female", "Female decision"), c(0, 1, 2, 3, 4))))
rti_complete$secc_decision_c06 <- as.numeric(as.character(mapvalues(rti_complete$secc_decision_c06, c("Male decision", "Mostly male", "Joint", "Mostly female", "Female decision"), c(0, 1, 2, 3, 4))))
rti_complete$secc_access_info_c07 <- as.numeric(as.character(mapvalues(rti_complete$secc_access_info_c07, c("never", "rarely", "sometime", "mostly", "Always"), c(0, 1, 2, 3, 4))))
rti_complete$secc_access_info_c08 <- as.numeric(as.character(mapvalues(rti_complete$secc_access_info_c08, c("never", "rarely", "sometime", "mostly", "Always"), c(0, 1, 2, 3, 4))))
rti_complete$secc_access_info_c09 <- as.numeric(as.character(mapvalues(rti_complete$secc_access_info_c09, c("never", "rarely", "sometime", "mostly", "Always"), c(0, 1, 2, 3, 4))))
rti_complete$secc_access_info_c10 <- as.numeric(as.character(mapvalues(rti_complete$secc_access_info_c10, c("never", "rarely", "sometime", "mostly", "Always"), c(0, 1, 2, 3, 4))))
rti_complete$secc_access_info_c11 <- as.numeric(as.character(mapvalues(rti_complete$secc_access_info_c11, c("never", "rarely", "sometime", "mostly", "Always"), c(0, 1, 2, 3, 4))))

rti_complete$index_empowerment <- rti_complete$secc_decision_c01 + rti_complete$secc_decision_c02 + rti_complete$secc_decision_c03 + rti_complete$secc_decision_c04 + rti_complete$secc_decision_c05 + rti_complete$secc_decision_c06 + rti_complete$secc_access_info_c07 + rti_complete$secc_access_info_c08 + rti_complete$secc_access_info_c09 + rti_complete$secc_access_info_c10 + rti_complete$secc_access_info_c11

#Transfer data on Food security
rti_complete$f1 <- factor(rti_complete$seccd_seccd3_d18, levels = c("Very scarce", "Somewhat available", "Readily available", "Available"))
rti_complete$f2 <- factor(rti_complete$seccd_seccd3_d19, levels = c("Very affordable", "Some affordable", "Expensive",  "Very expensive"))
rti_complete$f3 <- factor(rti_complete$seccd_seccd3_d20, levels = c("Very inaccessible", "Somewhat accessible", "Readily accessible", "Accessible"))
rti_complete$f4 <- factor(rti_complete$seccd_seccd3_d21, levels = c("Hardly", "Sometimes", "Most times", "All the time"))

#Create Food Insecurity Index
#Convert answers from factors into numbers from 0 to 3
rti_complete$seccd_seccd3_d18 <- as.numeric(as.character(mapvalues(rti_complete$seccd_seccd3_d18, c("Very scarce", "Somewhat available", "Readily available", "Available"), c(0, 1, 2, 3))))
rti_complete$seccd_seccd3_d19 <- as.numeric(as.character(mapvalues(rti_complete$seccd_seccd3_d19, c("Very expensive", "Expensive", "Some affordable", "Very affordable"), c(0, 1, 2, 3))))
rti_complete$seccd_seccd3_d20 <- as.numeric(as.character(mapvalues(rti_complete$seccd_seccd3_d20, c("Very inaccessible", "Somewhat accessible", "Readily accessible", "Accessible"), c(0, 1, 2, 3))))
rti_complete$seccd_seccd3_d21 <- as.numeric(as.character(mapvalues(rti_complete$seccd_seccd3_d21, c("All the time", "Most times", "Sometimes", "Hardly"), c(0, 1, 2, 3))))

rti_complete$index_food_security <- rti_complete$seccd_seccd3_d18 + rti_complete$seccd_seccd3_d19 + rti_complete$seccd_seccd3_d20 + rti_complete$seccd_seccd3_d21

#Save data for self efficacy questions
rti_complete$se1 <- factor(rti_complete$secff_f26, levels = c("Very certain", "Rather certain", "Rather uncertain", "Very uncertain"), ordered = TRUE)
rti_complete$se2 <- factor(rti_complete$secff_f27, levels = c("Very certain", "Rather certain", "Rather uncertain", "Very uncertain"), ordered = TRUE)
rti_complete$se3 <- factor(rti_complete$secff_f28, levels = c("Very certain", "Rather certain", "Rather uncertain", "Very uncertain"), ordered = TRUE)
rti_complete$se4 <- factor(rti_complete$secff_f28, levels = c("Very certain", "Rather certain", "Rather uncertain", "Very uncertain"), ordered = TRUE)
rti_complete$se5 <- factor(rti_complete$secff_f30, levels = c("Very certain", "Rather certain", "Rather uncertain", "Very uncertain"), ordered = TRUE)
rti_complete$se6 <- factor(rti_complete$secff_f31, levels = c("Very certain", "Rather certain", "Rather uncertain", "Very uncertain"), ordered = TRUE)
rti_complete$se7 <- factor(rti_complete$secff_f32, levels = c("Very certain", "Rather certain", "Rather uncertain", "Very uncertain"), ordered = TRUE)
rti_complete$se8 <- factor(rti_complete$secff_f33, levels = c("Very certain", "Rather certain", "Rather uncertain", "Very uncertain"), ordered = TRUE)

#Create Index for Self Efficacy
#Convert answers from factors into numbers from 0 to 3
rti_complete$secff_f26 <- as.numeric(as.character(mapvalues(rti_complete$secff_f26, c("Very uncertain", "Rather uncertain", "Rather certain", "Very certain"), c(0, 1, 2, 3))))
rti_complete$secff_f27 <- as.numeric(as.character(mapvalues(rti_complete$secff_f27, c("Very uncertain", "Rather uncertain", "Rather certain", "Very certain"), c(0, 1, 2, 3))))
rti_complete$secff_f28 <- as.numeric(as.character(mapvalues(rti_complete$secff_f28, c("Very uncertain", "Rather uncertain", "Rather certain", "Very certain"), c(0, 1, 2, 3))))
rti_complete$secff_f29 <- as.numeric(as.character(mapvalues(rti_complete$secff_f29, c("Very uncertain", "Rather uncertain", "Rather certain", "Very certain"), c(0, 1, 2, 3))))
rti_complete$secff_f30 <- as.numeric(as.character(mapvalues(rti_complete$secff_f30, c("Very uncertain", "Rather uncertain", "Rather certain", "Very certain"), c(0, 1, 2, 3))))
rti_complete$secff_f31 <- as.numeric(as.character(mapvalues(rti_complete$secff_f31, c("Very uncertain", "Rather uncertain", "Rather certain", "Very certain"), c(0, 1, 2, 3))))
rti_complete$secff_f32 <- as.numeric(as.character(mapvalues(rti_complete$secff_f32, c("Very uncertain", "Rather uncertain", "Rather certain", "Very certain"), c(0, 1, 2, 3))))
rti_complete$secff_f33 <- as.numeric(as.character(mapvalues(rti_complete$secff_f33, c("Very uncertain", "Rather uncertain", "Rather certain", "Very certain"), c(0, 1, 2, 3))))

rti_complete$index_self_efficacy <- rti_complete$secff_f26 + rti_complete$secff_f27 + rti_complete$secff_f28 + rti_complete$secff_f29 + rti_complete$secff_f30 + rti_complete$secff_f31 + rti_complete$secff_f32 + rti_complete$secff_f33

#create variable for total household size and replace 0 with NA
rti_complete$hh_size <- as.numeric(rowSums(rti_complete[,c("hh_below_6months_sum_less_6month", "hh_below_2yrs_sum_less_2yrs", "hh_age2_14yrs_sum_2_14yrs", "hh_age15_29yrs_sum_15_29yrs", "hh_above35yrs_sum_above_35yrs", "ch_below_6months_sum_l_6months", "ch_between_6_23_sum_6_23")], na.rm=TRUE))
rti_complete <- rti_complete %>%
  mutate(hh_size = ifelse(hh_size == 0, NA, hh_size))

#create variables for children <6months and between 6 and 23 months
rti_complete$less_6m <- factor(rowSums(rti_complete[,c("hh_below_6months_sum_less_6month", "ch_below_6months_sum_l_6months")], na.rm=TRUE))
rti_complete$betw_6m_23m <- factor(rowSums(rti_complete[,c("hh_below_2yrs_sum_less_2yrs", "ch_between_6_23_sum_6_23")], na.rm=TRUE))

#create income brackets
f_income <- function(x) {as.numeric(as.character(plyr::mapvalues(x, from = c("0", "1-1000", "1001-3000", "3001-6000", "6001-9000", "9001-12000", ">12000"),to = c(0, 500, 2000, 4500, 7500, 10500, 15000))))}
vars_income<-c("seccd_seccd1_d02", "seccd_seccd1_d03", "seccd_seccd1_d04", "seccd_seccd1_d05")
rti_complete <- rti_complete %>% mutate_at(vars_income, f_income)
rti_complete$income <- rowSums(rti_complete[,vars_income])
rti_complete$income_split <- rti_complete$income
rti_complete <- rti_complete %>% mutate(income_split=ifelse(income_split > 8500, "High income", "Low income"))

# 3. Additional Data cleaning
#Convert unrealistic answers for land size to NA
rti_complete <- rti_complete %>%
  mutate(seca_a17 = ifelse(seca_a17 > 200, NA, seca_a17))

rti_complete <- rti_complete %>%
  mutate(seca_a17a = ifelse(seca_a17a > 200, NA, seca_a17a))

#save complete dataset
write.csv(rti_complete, "clean_data.csv")
rti_complete <- read_csv("clean_data.csv")

#Convert variables from Character to Factor
rti_complete$treatment <- factor(rti_complete$treatment,levels = c("agrinutrition", "income", "agrinutrition&income", "referral"))
rti_complete$seca_a09 <- factor(rti_complete$seca_a09,levels = c("None", "Some primary", "Primary", "Some secondary", "Secondary", "Tertiary", "Vocational training", "Gumbaro/ adult education"))
rti_complete$seca_a09 <- factor(rti_complete$seca_a09, levels=rev(levels(rti_complete$seca_a09)))
rti_complete$seca_a10 <- factor(rti_complete$seca_a10, levels = c("Other (specify)", "Student", "Salaried labour", "Housewife", "Farmer", "Casual labour", "Business"))
rti_complete$seca_a12 <- factor(rti_complete$seca_a12, levels = c("above 35 years", "30-35 years", "15-29 years"))
rti_complete$seca_a13 <- factor(rti_complete$seca_a13)
rti_complete$seca_a13 <- factor(rti_complete$seca_a13, levels=rev(levels(rti_complete$seca_a13)))
rti_complete$hh_size_f <- factor(rti_complete$hh_size, levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "22"))
rti_complete$seccd_seccd3_d16 <- factor(rti_complete$seccd_seccd3_d16, levels = c("Before January", "January", "February", "March", "April"))
rti_complete$seccd_seccd3_d17 <- factor(rti_complete$seccd_seccd3_d17, levels = c("Before January", "January", "February", "March", "April"))

#4. Create subsets of Data
#Based on gender, age and hh size
rti_female<-subset(rti_complete, seca_a11 == "Female")
rti_male<-subset(rti_complete, seca_a11 == "Male")
rti_less_6m <- subset(rti_complete, less_6m != "0")
rti_6m_23m <- subset(rti_complete, betw_6m_23m != "0")
rti_hh <- subset(rti_complete, hh_size != "NA")

#Create dataset where each child's DDS corresponds to one row
rti_child <- subset(rti_complete, select = c("unique", "C_DDS_male_1", "C_DDS_male_2", "C_DDS_male_3", "C_DDS_male_4", "C_DDS_male_5", "C_DDS_female_1", "C_DDS_female_2", "C_DDS_female_3", "C_DDS_female_4", "C_DDS_female_5", "C_DDS_female_6"))
rti_child <- melt(rti_child, "unique")
rti_child <- subset(rti_child, value != "NA")
rti_child_complete <- merge(rti_child, rti_complete, by.x = c("unique"), by.y = c("unique"), all.x = TRUE)
rti_child_complete$DDS_child <- rti_child_complete$value

#Create Dummy variable for well-nourished children
rti_child_complete$c_dummy  <- as.numeric(rti_child_complete$DDS_child >= 4)
rti_child_complete$c_dummy  <- mapvalues(rti_child_complete$c_dummy, from = c("0", "1"), to = c("Non acceptable", "Minimum acceptable"))

#Create subset for children and female respondents
rti_child_female_resp <- subset(rti_child_complete, seca_a11 == "Female")

#Create subsets by training
rti_agri <- subset(rti_complete, treatment == "agrinutrition")
rti_agri_income <- subset(rti_complete, treatment == "agrinutrition&income")
rti_income <- subset(rti_complete, treatment == "income")
rti_referral <- subset(rti_complete, treatment == "referral")

#Create subsets of people who received Agrinutrition training
rti_agri <- subset(rti_complete, treatment == "agrinutrition" | treatment == "agrinutrition&income")
rti_female_agri <- subset(rti_female, treatment == "agrinutrition" | treatment == "agrinutrition&income")
rti_child_agri <- subset(rti_child_complete, treatment == "agrinutrition" | treatment == "agrinutrition&income")

#Create subsets of data for knowledge questions
rti_knowledge_general <- select(rti_complete, q1:q5)
rti_knowledge_reproductive <- select(rti_complete, q6:q9)
rti_knowledge_infant <- select(rti_complete, q10:q21)

factored <- function(x) { factor(x, levels = c("Correct answer", "Don't know", "Wrong answer"), ordered = TRUE) }
rti_knowledge_general<- mutate_all(rti_knowledge_general, factored)
rti_knowledge_reproductive<- mutate_all(rti_knowledge_reproductive, factored)
rti_knowledge_infant<- mutate_all(rti_knowledge_infant, factored)
rti_knowledge_general <- as.data.frame(rti_knowledge_general)
rti_knowledge_reproductive <- as.data.frame(rti_knowledge_reproductive)
rti_knowledge_infant <- as.data.frame(rti_knowledge_infant)

#Create subsets for knowledge question by treatment
rti_knowledge_general_income <- select(rti_income, q1:q5)
rti_knowledge_general_agri <- select(rti_agri, q1:q5)
rti_knowledge_general_agri_income <- select(rti_agri_income, q1:q5)
rti_knowledge_general_referral <- select(rti_referral, q1:q5)

rti_knowledge_reproductive_income <- select(rti_income, q6:q9)
rti_knowledge_reproductive_agri_income <- select(rti_agri_income, q6:q9)
rti_knowledge_reproductive_agri <- select(rti_agri, q6:q9)
rti_knowledge_reproductive_referral <- select(rti_referral, q6:q9)

rti_knowledge_infant_income <- select(rti_income, q10:q21)
rti_knowledge_infant_agri_income <- select(rti_agri_income, q10:q21)
rti_knowledge_infant_agri <- select(rti_agri, q10:q21)
rti_knowledge_infant_referral <- select(rti_referral, q10:q21)

factored <- function(x) { factor(x, levels = c("Correct answer", "Don't know", "Wrong answer"), ordered = TRUE) }

rti_knowledge_general_income <- mutate_all(rti_knowledge_general_income, factored)
rti_knowledge_general_agri <- mutate_all(rti_knowledge_general_agri, factored)
rti_knowledge_general_agri_income <- mutate_all(rti_knowledge_general_agri_income, factored)
rti_knowledge_general_referral <- mutate_all(rti_knowledge_general_referral, factored)

rti_knowledge_reproductive_income <- mutate_all(rti_knowledge_reproductive_income, factored)
rti_knowledge_reproductive_agri_income <- mutate_all(rti_knowledge_reproductive_agri_income, factored)
rti_knowledge_reproductive_agri <- mutate_all(rti_knowledge_reproductive_agri, factored)
rti_knowledge_reproductive_referral <- mutate_all(rti_knowledge_reproductive_referral, factored)

rti_knowledge_infant_income <- mutate_all(rti_knowledge_infant_income, factored)
rti_knowledge_infant_agri_income <- mutate_all(rti_knowledge_infant_agri_income, factored)
rti_knowledge_infant_agri <- mutate_all(rti_knowledge_infant_agri, factored)
rti_knowledge_infant_referral <- mutate_all(rti_knowledge_infant_referral, factored)

rti_knowledge_general_income <- as.data.frame(rti_knowledge_general_income)
rti_knowledge_general_agri <- as.data.frame(rti_knowledge_general_agri)
rti_knowledge_general_agri_income <- as.data.frame(rti_knowledge_general_agri_income)
rti_knowledge_general_referral <- as.data.frame(rti_knowledge_general_referral)

rti_knowledge_reproductive_income <- as.data.frame(rti_knowledge_reproductive_income)
rti_knowledge_reproductive_agri_income <- as.data.frame(rti_knowledge_reproductive_agri_income)
rti_knowledge_reproductive_agri <- as.data.frame(rti_knowledge_reproductive_agri)
rti_knowledge_reproductive_referral <- as.data.frame(rti_knowledge_reproductive_referral)

rti_knowledge_infant_income <- as.data.frame(rti_knowledge_infant_income)
rti_knowledge_infant_agri_income <- as.data.frame(rti_knowledge_infant_agri_income)
rti_knowledge_infant_agri <- as.data.frame(rti_knowledge_infant_agri)
rti_knowledge_infant_referral <- as.data.frame(rti_knowledge_infant_referral)

#Create subsets of data for empowerment questions
rti_empowerment_decision <- select(rti_female, e1:e6)
rti_empowerment_access <- select(rti_female, e7:e11)
fun_e1 <- function(x) { factor(x, levels = c("Female decision", "Mostly female", "Joint", "Mostly male", "Male decision"), ordered = TRUE) }
rti_empowerment_decision<- mutate_all(rti_empowerment_decision, fun_e1)
fun_e2 <- function(x) { factor(x, levels = c("Always", "mostly", "sometime", "rarely", "never"), ordered = TRUE) }
rti_empowerment_access <- mutate_all(rti_empowerment_access, fun_e2)
rti_empowerment_decision <- as.data.frame(rti_empowerment_decision)
rti_empowerment_access <- as.data.frame(rti_empowerment_access)

#Create subset of data for meal frequency questions
rti_meal_freq <- select(rti_female, secb_secb1_b01:secb_secb1_b05)
rti_meal_freq <- as.data.frame(rti_meal_freq)

#Create subset of data for meal frequencies by treatment group
rti_meal_freq_agri <- subset(rti_female, treatment=="agrinutrition")
rti_meal_freq_agri_income <- subset(rti_female, treatment=="agrinutrition&income")
rti_meal_freq_income <- subset(rti_female, treatment=="income")
rti_meal_freq_referral <- subset(rti_female, treatment=="referral")
rti_meal_freq_agri <- select(rti_meal_freq_agri, secb_secb1_b01:secb_secb1_b05)
rti_meal_freq_agri_income <- select(rti_meal_freq_agri_income, secb_secb1_b01:secb_secb1_b05)
rti_meal_freq_income <- select(rti_meal_freq_income, secb_secb1_b01:secb_secb1_b05)
rti_meal_freq_referral <- select(rti_meal_freq_referral, secb_secb1_b01:secb_secb1_b05)

fun_f1 <- function(x) { factor(x, levels = c("Every Day", "Three times a week", "Once a week", "Once a month", "Never"), ordered = TRUE) }

rti_meal_freq_income <- mutate_all(rti_meal_freq_income, fun_f1)
rti_meal_freq_agri <- mutate_all(rti_meal_freq_agri, fun_f1)
rti_meal_freq_agri_income <- mutate_all(rti_meal_freq_agri_income, fun_f1)
rti_meal_freq_referral <- mutate_all(rti_meal_freq_referral, fun_f1)


rti_meal_freq_agri <- as.data.frame(rti_meal_freq_agri)
rti_meal_freq_agri_income <- as.data.frame(rti_meal_freq_agri_income)
rti_meal_freq_income <- as.data.frame(rti_meal_freq_income)
rti_meal_freq_referral <- as.data.frame(rti_meal_freq_referral)

#Create subset of data for self efficacy questions
rti_self_efficacy <- as.data.frame(select(rti_complete, se1:se8))

#5. Data Analysis of Main Outcomes
#Uploading Busara Theme
font_import(pattern="Muli",prompt=FALSE)
busara_theme <- theme(
  plot.title  =  element_text(size  =  18, hjust  =  0.5, face="bold"),
  plot.subtitle  =  element_text(hjust  =  0.5),
  text  =  element_text(size = 15, family = "Muli", color = "black"),
  panel.border  =  element_blank(),
  axis.text.x  =  element_text(angle  =  0, hjust  =  0.5),
  panel.grid.major  =  element_blank(),
  panel.grid.major.x = element_blank(),
  panel.grid.major.y = element_blank(),
  axis.line.x  =  element_line(color = "black", size  =  1),
  legend.position = "right")

#A) Distribution of Women's DDS
p11 <- ggplot(rti_female) + 
  geom_density(mapping = aes(x = DDS_resp, y= ..count..), fill="#0033a1")+
  labs(y = "Observations", x = "Dietary Diversity Score", title = "Dietary diversity of female respondents")+
  theme(panel.background=element_blank(), plot.title = element_text (size  =  16, hjust  =  0.5, face="bold"))
p11

df12 <- rti_female %>% dplyr::group_by(seca_a13, DDS_resp_dummy) %>% dplyr::summarise(Count=n()) %>% dplyr::ungroup() %>% dplyr::group_by(seca_a13) %>% dplyr::mutate(Percent = 100*Count/sum(Count))
write.csv(df12, "df12.csv") #Enter value for "all counties"
df12<-read_excel("df12.xlsx")
df12$Percent <- round(df12$Percent, digits = 0)
df12$lab <- with(df12, paste(Count," (", Percent,"%)", sep = ""))
p12 <- ggplot(df12, mapping = aes(x=seca_a13, y= Percent, fill=DDS_resp_dummy)) +
  geom_bar(stat = "identity", position="stack")+
  labs(y = "Proportion of Participants (%)", x = "County", title = "Consumption of diet of minimum diversity (female respondents)", fill = "Diet", caption = "Number of participants (%)")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), panel.background=element_blank(), plot.title = element_text (size  =  16, hjust  =  0.2, face="bold")) +
  scale_fill_brewer(palette = "Dark2")+
  geom_text(aes(label = lab), size = 2.8, position = position_stack(vjust = 0.6))
p12

#B) Distribution of Children's DDS
p13 <- ggplot(rti_child_complete) + 
  geom_density(mapping = aes(x = DDS_child, y= ..count..), fill="#0033a1")+
  labs(y = "Observations", x = "Dietary Diversity Score", title = "Dietary diversity of children")+
  theme(panel.background=element_blank(), plot.title = element_text (size  =  16, hjust  =  0.5, face="bold"))
p13

df14 <- rti_child_complete %>% dplyr::group_by(seca_a13, c_dummy) %>% dplyr::summarise(Count=n()) %>% dplyr::ungroup() %>% dplyr::group_by(seca_a13) %>% dplyr::mutate(Percent = 100*Count/sum(Count))
write.csv(df14, "df14.csv") #Enter value for "all counties"
df14 <- read_excel("df14.xlsx")
df14$Percent <- round(df14$Percent, digits = 0)
df14$lab <- with(df14, paste(Count," (", Percent,"%)", sep = ""))
p14 <- ggplot(df14, mapping = aes(x=seca_a13, y= Percent, fill=c_dummy)) +
  geom_bar(stat = "identity", position="stack")+
  labs(y = "Proportion of Participants (%)", x = "County", title = "Consumption of diet of minimum diversity (children)", fill = "Diet", caption = "Number of children (%)")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), panel.background=element_blank(), plot.title = element_text (size  =  16, hjust  =  0.2, face="bold")) +
  scale_fill_brewer(palette = "Dark2")+
  geom_text(aes(label = lab), size = 2.8, position = position_stack(vjust = 0.5))
p14

#C) Impact of Training on DDS
lm15 <- lm(DDS_resp ~ treatment, rti_female)
summary(lm15)
se15 <- summarySE(rti_female, measurevar="DDS_resp", groupvars="treatment")
se15$DDS_resp <- round(se15$DDS_resp, digits = 2)
p15 <- ggplot(rti_female, mapping = aes(x = treatment, y = DDS_resp, fill = treatment)) +
  geom_bar(stat = "summary")+
  labs(y = "Dietary Diversity Score", x = "Treatment", title = "Dietary diversity by treatment (female respondents)")+
  theme(panel.background=element_blank(), plot.title = element_text (size  =  16, hjust  =  0.5, face="bold"), legend.position = "none")+
  geom_text(data = se15, nudge_y =0.1, nudge_x = -0.3, aes(x = treatment, y =DDS_resp, label =DDS_resp))+
  geom_errorbar(data = se15, aes(ymin=DDS_resp-ci, ymax=DDS_resp+ci), width=.2)+
  coord_cartesian(ylim=c(4, 7))+
  scale_fill_brewer(palette="Dark2")
p15

lm16 <- lm(DDS_child ~ treatment, rti_child_complete)
summary(lm16)
se16 <- summarySE(rti_child_complete, measurevar="DDS_child", groupvars="treatment")
se16$DDS_child <- round(se16$DDS_child, digits = 2)
p16 <- ggplot(rti_child_complete, mapping = aes(x = treatment, y = DDS_child, fill = treatment)) +
  geom_bar(stat = "summary")+
  labs(y = "Dietary Diversity Score", x = "Treatment", title = "Dietary diversity by treatment (children)")+
  theme(panel.background=element_blank(), plot.title = element_text (size  =  16, hjust  =  0.5, face="bold"), legend.position = "none")+
  geom_text(data = se16, nudge_y =0.1, nudge_x = -0.3, aes(x = treatment, y =DDS_child, label =DDS_child))+
  geom_errorbar(data = se16, aes(ymin=DDS_child-ci, ymax=DDS_child+ci), width=.2)+
  coord_cartesian(ylim=c(3, 6))+
  scale_fill_brewer(palette="Dark2")
p16       

lm17 <- lm(DDS_resp ~ treatment, rti_female_agri)
summary(lm17)
se17 <- summarySE(rti_female_agri, measurevar="DDS_resp", groupvars="treatment")
se17$DDS_resp <- round(se17$DDS_resp, digits = 2)
p17 <- ggplot(rti_female_agri, mapping = aes(x = treatment, y = DDS_resp, fill = treatment)) +
  geom_bar(stat = "summary")+
  labs(y = "Dietary Diversity Score", x = "Treatment", title = "DDS: Additional effect of income training (female respondents)")+
  theme(panel.background=element_blank(), plot.title = element_text (size  =  16, hjust  =  0.5, face="bold"))+
  geom_text(data = se17, nudge_y =0.1, nudge_x = -0.3, aes(x = treatment, y =DDS_resp, label =DDS_resp))+
  geom_errorbar(data = se17, aes(ymin=DDS_resp-ci, ymax=DDS_resp+ci), width=.2)+
  coord_cartesian(ylim=c(4, 7.4))+
  scale_fill_brewer(palette="Dark2")
p17

lm18 <- lm(DDS_child ~ treatment, rti_child_agri)
summary(lm18)
se18 <- summarySE(rti_child_agri, measurevar="DDS_child", groupvars="treatment")
se18$DDS_child <- round(se18$DDS_child, digits = 2)
p18 <- ggplot(rti_child_agri, mapping = aes(x = treatment, y = DDS_child, fill = treatment)) +
  geom_bar(stat = "summary")+
  labs(y = "Dietary Diversity Score", x = "Treatment", title = "DDS: Additional effect of income training (children)")+
  theme(panel.background=element_blank(), plot.title = element_text (size  =  16, hjust  =  0.5, face="bold"))+
  geom_text(data = se18, nudge_y =0.1, nudge_x = -0.3, aes(x = treatment, y =DDS_child, label =DDS_child))+
  geom_errorbar(data = se18, aes(ymin=DDS_child-ci, ymax=DDS_child+ci), width=.2)+
  coord_cartesian(ylim=c(3, 5.5))+
  scale_fill_brewer(palette="Dark2")
p18

#D)How does training affect knowledge indices?
lm19 <- lm(index_k_nutrition_overall ~ treatment, rti_complete)
summary(lm19)
se19 <- summarySE(rti_complete, measurevar="index_k_nutrition_overall", groupvars="treatment")
se19$index_k_nutrition_overall <- round(se19$index_k_nutrition_overall, digits = 2)
p19 <- ggplot(rti_complete, mapping = aes(x = treatment, y = index_k_nutrition_overall, fill = treatment)) +
  geom_bar(stat = "summary")+
  labs(y = "Overall knowledge about nutrition", x = "Treatment", title = "Knowledge about nutrition by treatment", caption = "Knowledge index: number of questions 
       about nutritional knowledge that respondents 
       answered correctly (out of 21)")+
  theme(panel.background=element_blank(), plot.title = element_text (size  =  16, hjust  =  0.5, face="bold"), plot.caption = element_text(size = 8, vjust = 5), legend.position = "none")+
  geom_text(data = se19, nudge_y =0.1, nudge_x = -0.3, aes(x = treatment, y =index_k_nutrition_overall, label = index_k_nutrition_overall))+
  geom_errorbar(data = se19, aes(ymin=index_k_nutrition_overall-ci, ymax=index_k_nutrition_overall+ci), width=.2)+
  coord_cartesian(ylim=c(12, 16))+
  scale_fill_brewer(palette="Dark2")
p19

lm20 <- lm(index_k_general_nutrition ~ treatment, rti_complete)
summary(lm20)
se20 <- summarySE(rti_complete, measurevar="index_k_general_nutrition", groupvars="treatment")
se20$index_k_general_nutrition <- round(se20$index_k_general_nutrition, digits = 2)
p20 <- ggplot(rti_complete, mapping = aes(x = treatment, y = index_k_general_nutrition, fill = treatment)) +
  geom_bar(stat = "summary")+
  labs(y = "Knowledge about general nutrition", x = "Treatment", title = "Knowledge about general nutrition by treatment", caption = "Knowledge index: number of questions 
       about general nutritional knowledge that 
       respondents answered correctly (out of 5)")+
  theme(panel.background=element_blank(), plot.title = element_text (size  =  16, hjust  =  0.5, face="bold"), plot.caption = element_text(size = 8, vjust = 5), legend.position = "none")+
  geom_text(data = se20, nudge_y =0.1, nudge_x = -0.3, aes(x = treatment, y =index_k_general_nutrition, label = index_k_general_nutrition))+
  geom_errorbar(data = se20, aes(ymin=index_k_general_nutrition-ci, ymax=index_k_general_nutrition+ci), width=.2)+
  coord_cartesian(ylim=c(2, 5))+
  scale_fill_brewer(palette="Dark2")
p20

lm21 <- lm(index_k_reproductive_women ~ treatment, rti_complete)
summary(lm20)
se21 <- summarySE(rti_complete, measurevar="index_k_reproductive_women", groupvars="treatment")
se21$index_k_reproductive_women <- round(se21$index_k_reproductive_women, digits = 2)
p21 <- ggplot(rti_complete,mapping = aes(x = treatment, y = index_k_reproductive_women, fill = treatment)) +
  geom_bar(stat = "summary")+
  labs(y = "Knowledge about nutrition for women of reproductive age", x = "Treatment", title = "Knowledge about nutrition for women of reproductive age", caption = "Knowledge index: number of questions 
       about nutrition for women of repr. age that 
       respondents answered correctly (out of 4)")+
  theme(panel.background=element_blank(), plot.title = element_text (size  =  16, hjust  =  0.5, face="bold"), plot.caption = element_text(size = 8, vjust = 5), legend.position = "none")+
  geom_text(data = se21, nudge_y =0.05, nudge_x = -0.3, aes(x = treatment, y =index_k_reproductive_women, label = index_k_reproductive_women))+
  geom_errorbar(data = se21, aes(ymin=index_k_reproductive_women-ci, ymax=index_k_reproductive_women+ci), width=.2)+
  coord_cartesian(ylim=c(1.5, 3))+
  scale_fill_brewer(palette="Dark2")
p21

lm22 <- lm(index_k_infant_nutrition ~ treatment, rti_complete)
summary(lm22)
se22 <- summarySE(rti_complete, measurevar="index_k_infant_nutrition", groupvars="treatment")
se22$index_k_infant_nutrition <- round(se22$index_k_infant_nutrition, digits = 2)
p22 <- ggplot(rti_complete, mapping = aes(x = treatment, y = index_k_infant_nutrition, fill = treatment)) +
  geom_bar(stat = "summary")+
  labs(y = "Knowledge about infant and young child nutrition", x = "Treatment", title = "Knowledge about infant and young child nutrition", caption = "Knowledge index: number of questions 
       about infant and young child nutrition that 
       respondents answered correctly (out of 12)")+
  theme(panel.background=element_blank(), plot.title = element_text (size  =  16, hjust  =  0.5, face="bold"), plot.caption = element_text(size = 8, vjust = 5), legend.position = "none")+
  geom_text(data = se22, nudge_y =0.1, nudge_x = -0.3, aes(x = treatment, y =index_k_infant_nutrition, label = index_k_infant_nutrition))+
  geom_errorbar(data = se22, aes(ymin=index_k_infant_nutrition-ci, ymax=index_k_infant_nutrition+ci), width=.2)+
  coord_cartesian(ylim=c(6, 9))+
  scale_fill_brewer(palette="Dark2")
p22

#E)Relationship between nutritional knowledge and Dietary Diversity
lm23 <- lm(DDS_resp ~ index_k_nutrition_overall, rti_female)
summary(lm23)
df23 <- rti_female %>% group_by(index_k_nutrition_overall) %>% dplyr::summarise(DDS_resp = round(mean(DDS_resp, na.rm = TRUE), digits = 2))
p23 <- ggplot(rti_female) +
  geom_bar(mapping = aes(x = index_k_nutrition_overall, y = DDS_resp), stat = "summary", fill = "#0033a1")+
  labs(y = "Dietary Diversity", x = "Overall knowledge about nutrition", title = "Dietary diversity by knowledge about nutrition (female respondents)", caption = "Knowledge index: number of questions 
       about general nutrition that respondents 
       answered correctly (out of 21)")+
  theme(panel.background=element_blank(), plot.title = element_text (size  =  16, hjust  =  0.5, face="bold"), plot.caption = element_text(size = 8, vjust = 5))+
  geom_text(data = df23, nudge_y =0.2, aes(x = index_k_nutrition_overall, y =DDS_resp, label =DDS_resp))+
  scale_x_continuous()
p23

lm24 <- lm(DDS_resp ~ index_k_reproductive_women, rti_female)
summary(lm24)
df24 <- rti_female %>% group_by(index_k_reproductive_women) %>% dplyr::summarise(DDS_resp = round(mean(DDS_resp, na.rm = TRUE), digits = 2))
p24 <- ggplot(rti_female) +
  geom_bar(mapping = aes(x = index_k_reproductive_women, y = DDS_resp), stat = "summary", fill="#0033a1")+
  labs(y = "Dietary Diversity", x = "Knowledge about nutrition of women of reproductive age", title = "Dietary Diversity and knowledge about 
nutrition of women of reproductive age (female respondents)", caption = "Knowledge index: number of questions 
       about nutrition for women of repr. age that 
       respondents answered correctly (out of 4)")+
  theme(panel.background=element_blank(), plot.title = element_text (size  =  16, hjust  =  0.5, face="bold"), plot.caption = element_text(size = 8, vjust = 5)) +
  geom_text(data = df24, nudge_y =0.2, aes(x = index_k_reproductive_women, y =DDS_resp, label =DDS_resp))
p24

lm25 <- lm(DDS_child ~ index_k_infant_nutrition, rti_child_complete)
summary(lm25)
df25 <- rti_child_complete %>% group_by(index_k_infant_nutrition) %>% dplyr::summarise(DDS_child = round(mean(DDS_child), digits = 2))
p25 <- ggplot(rti_child_complete) +
  geom_bar(mapping = aes(x = index_k_infant_nutrition, y = DDS_child), stat = "summary", fill="#0033a1")+
  labs(y = "Dietary Diversity", x = "Knowledge about infant and young child nutrition", title = "Dietary diversity of children 
by knowledge about infant and young child nutrition", caption = "Knowledge index: number of questions 
       about infant and young child nutrition that 
       respondents answered correctly (out of 12)")+
  theme(panel.background=element_blank(), plot.title = element_text (size  =  16, hjust  =  0.5, face="bold"), plot.caption = element_text(size = 8, vjust = 5)) +
  geom_text(data = df25, nudge_y =0.2, aes(x = index_k_infant_nutrition, y =DDS_child, label =DDS_child))
p25

  #6. Data Analysis of Additional Outcomes
#A) Demographics
df26 <- rti_complete %>% group_by(seca_a09) %>% dplyr::summarise(Count=n())
p26 <- ggplot(rti_complete, mapping = aes(x = seca_a09, fill=seca_a09)) +
  geom_bar()+
  labs(y = "Number of Respondents", x = "", title = "Respondents' highest level of education")+
  theme(panel.background=element_blank(), plot.title = element_text (size  =  16, hjust  =  0.1, face="bold"), legend.position = "none") +
  coord_flip()+
  scale_fill_brewer(palette = "Dark2")+
  geom_text(data = df26, nudge_y =8, aes(x = seca_a09, y =Count, label =Count))
p26

df27 <- rti_complete %>% group_by(seca_a10) %>% dplyr::summarise(Count=n())
p27 <- ggplot(rti_complete, mapping = aes(x = seca_a10, fill = seca_a10)) +
  geom_bar()+
  labs(y = "Number of Respondents", x = "", title = "Main occupation of respondents")+
  theme(panel.background=element_blank(), plot.title = element_text (size  =  16, hjust  =  0.1, face="bold"), legend.position = "none") +
  coord_flip()+
  scale_fill_brewer(palette = "Dark2")+
  geom_text(data = df27, nudge_y =15, aes(x = seca_a10, y =Count, label =Count))
p27

df28 <- rti_complete %>% group_by(seca_a11) %>% dplyr::summarise(Count=n())
p28 <- ggplot(rti_complete, mapping = aes(x = seca_a11, fill = seca_a11)) +
  geom_bar()+
  labs(y = "Number of Respondents", x = "", title = "Gender of respondents")+
  theme(panel.background=element_blank(), plot.title = element_text (size  =  16, hjust  =  0.1, face="bold"), legend.position = "none") +
  coord_flip()+
  scale_fill_brewer(palette = "Dark2")+
  geom_text(data = df28, nudge_y =18, aes(x = seca_a11, y =Count, label =Count))
p28

df29 <- rti_complete %>% group_by(seca_a12) %>% dplyr::summarise(Count=n())
p29 <- ggplot(rti_complete, mapping = aes(x = seca_a12, fill = seca_a12)) +
  geom_bar()+
  labs(y = "Number of Respondents", x = "", title = "Age of respondents")+
  theme(panel.background=element_blank(), plot.title = element_text (size  =  16, hjust  =  0.1, face="bold"), legend.position = "none") +
  coord_flip()+
  scale_fill_brewer(palette = "Dark2")+
  geom_text(data = df29, nudge_y =18, aes(x = seca_a12, y =Count, label =Count))
p29

df30 <- rti_complete %>% group_by(seca_a13) %>% dplyr::summarise(Count=n())
p30 <- ggplot(rti_complete, mapping = aes(x = seca_a13)) +
  geom_bar(fill = "#0033a1")+
  labs(y = "Number of Respondents", x = "", title = "Home county of respondents")+
  theme(panel.background=element_blank(), plot.title = element_text (size  =  16, hjust  =  0.1, face="bold"), legend.position = "none") +
  coord_flip()+
  scale_fill_brewer(palette = "Dark2")+
  geom_text(data = df30, nudge_y =5, aes(x = seca_a13, y =Count, label =Count))
p30

p31 <- ggplot(rti_complete) +
  geom_density(mapping = aes(x = seca_a17), fill="#0033a1")+
  labs(y = "", x = "", title = "Land size of respondents (outliers removed)")+
  scale_x_continuous(name = "Land size in hectares", limits = c(0, 25))+
  theme(panel.background=element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank(), plot.title = element_text (size  =  16, hjust  =  0.5, face="bold"))
p31

p32 <- ggplot(rti_less_6m) +
  geom_bar(mapping = aes(x = less_6m), fill="#0033a1")+
  labs(y = "Number of households", x = "Number of children", title = "Number of children younger than 6 months in respondents' households")+
  theme(panel.background=element_blank(), plot.title = element_text (size  =  16, hjust  =  0.5, face="bold"))
p32

p33 <- ggplot(rti_6m_23m) +
  geom_bar(mapping = aes(x = betw_6m_23m), fill="#0033a1")+
  labs(y = "Number of households", x = "Number of children", title = "Number of children betw. 6 and 23 months in respondents' households")+
  theme(panel.background=element_blank(), plot.title = element_text (size  =  16, hjust  =  0.5, face="bold"))
p33

p34 <- ggplot(rti_hh) +
  geom_bar(mapping = aes(x = hh_size_f), fill="#0033a1")+
  labs(y = "Number of respondents", x = "Household size", title = "Total household size of respondents")+
  theme(panel.background=element_blank(), plot.title = element_text (size  =  16, hjust  =  0.5, face="bold"))
p34

df35 <- rti_hh %>% group_by(seca_a13) %>% dplyr::summarise(hh_size = round(mean(hh_size), digits = 2))
p35 <- ggplot(rti_hh) + 
  geom_bar(mapping = aes(x = seca_a13, y= hh_size), stat = "summary", fill="#0033a1")+
  labs(y = "Average household size", x = "County", title = "Average household size by county")+
  theme(panel.background=element_blank(), plot.title = element_text (size  =  16, hjust  =  0.5, face="bold"))+
  geom_text(data = df35, nudge_y =0.2, aes(x = seca_a13, y =hh_size, label =hh_size))
p35
  
p36 <- ggplot(rti_complete) +
  geom_density(mapping = aes(x = income), fill="#0033a1")+
  labs(y = "", x = "Kenyan Shilling", title = "Montly income of respondents during last three months (approximations)")+
  theme(panel.background=element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank(), plot.title = element_text (size  =  16, hjust  =  0.5, face="bold"))
p36

#C) Creating Likert Graphs for Knowledge questions
names(rti_knowledge_general) = c("Good nutrition is important because it helps the body to grow and function properly; gives the needed energy to work and helps the body to fight off infections?", "A nutritious meal should have various types of foods from several food groups, in the right amounts?", "Children who do not get adequate nutrition are at risk of becoming stunted, wasted and underweight?", "Nutrition does not affect school performance for children?", "Poor nutrition does not cause someone to fall sick?")
p37<-likert(rti_knowledge_general)
plot(p37, include.center=FALSE, ordered=FALSE, col=c("green4", "grey", "indianred"))+
  ggtitle("Questions about general nutrition")+
  theme(plot.title = element_text (size  =  16, hjust  =  0.5, face="bold"))

names(rti_knowledge_reproductive) = c("Pregnant women should limit their food intake so that the foetus does not grow big?", "Pregnant women should not eat eggs?", "Breastfeeding mothers should eat a variety of foods every day from plant and animal sources?", "Breastfeeding women do not require additional meals as long as the 3 meals they are eating during the day are nutritious?")
p38<-likert(rti_knowledge_reproductive)
plot(p38, ordered=FALSE, col=c("green4", "grey", "indianred"))+
  ggtitle("Questions about nutrition for women of reproductive age")+
  theme(plot.title = element_text (size  =  16, hjust  =  1, face="bold"))

names(rti_knowledge_infant) = c("It is good to give a baby who is below 6 months-old milk from a cow and water?", "Babies should be breastfeeding frequently, day and night at least 8 -12 times per day for the mother to continue to produce plenty of breast milk?", "A baby should not breastfeed if the mother is sick?", "A baby who is below 6 months can be given some food (even in small amounts) especially if they keep on crying as this indicates that breastmilk is not enough?", "Babies above six months should be introduced to nutritious foods from the different food groups in addition to breast milk?", "The more the types of cereals mixed in a baby’s food, the more nutritious the food?", "A baby who is 12-23 months should stop breastfeeding and now eat family food?", "Babies at six months should eat at least 2 times a day?", "Babies 9-11 months need to eat 3 quarter a bowl of food at a time?", "Children 12-23 months should eat 6 times a day because they are bigger?", "Children with poor appetites should be fed small but frequent meals with a variety of foods from different food groups?", "When a child refuses to eat they should be forced to eat?")
p39<-likert(rti_knowledge_infant)
plot(p39, ordered=FALSE, col=c("green4", "grey", "indianred"))+
  ggtitle("Questions about infant and young child nutrition")+
  theme(plot.title = element_text (size  =  16, hjust  =  0.5, face="bold"))

#D) How does knowledge differ by demographics?
lm40<-lm(index_k_nutrition_overall ~ seca_a11, rti_complete)
summary(lm40)

lm41<-lm(index_k_nutrition_overall ~ income, rti_complete)
summary(lm41)

lm42<-lm(index_k_nutrition_overall ~ hh_size, rti_complete)
summary(lm42)

lm43<-lm(index_k_nutrition_overall ~ index_self_efficacy, rti_complete)
summary(lm43)

lm44<-lm(index_k_nutrition_overall ~ seca_a09, rti_complete)
summary(lm44)

#E) Graphs about women's empowerment
lm45 <- lm(index_empowerment ~ treatment, rti_female)
summary(lm45)
se45 <- summarySE(rti_female, measurevar="index_empowerment", groupvars="treatment")
se45$index_empowerment <- round(se45$index_empowerment, digits = 2)
p45 <- ggplot(rti_female, mapping = aes(x = treatment, y = index_empowerment, fill = treatment)) +
  geom_bar(stat = "summary")+
  labs(y = "Empowerment index", x = "Treatment", title = "Women's economic empowerment by treatment", caption = "Empowerment index: aggregated 
       answers to 11 questions regarding 
       women's economic empowerment")+
  theme(panel.background=element_blank(), plot.title = element_text (size  =  16, hjust  =  0.5, face="bold"), plot.caption = element_text(size = 8, vjust = 5), legend.position = "none")+
  geom_text(data = se45, nudge_y =0.2, nudge_x = -0.3, aes(x = treatment, y =index_empowerment, label = index_empowerment))+
  geom_errorbar(data = se45, aes(ymin=index_empowerment-ci, ymax=index_empowerment+ci), width=.2)+
  coord_cartesian(ylim=c(22, 28))+
  scale_fill_brewer(palette="Dark2")
p45

lm46 <- lm(DDS_resp ~ index_empowerment, rti_female)
summary(lm46)
p46 <- ggplot(rti_female, mapping = aes(x = index_empowerment, y = DDS_resp)) +
  geom_smooth(color="#0033a1", fill="steelblue")+
  labs(y = "Dietary Diversity", x = "Economic empowerment", title = "Women's dietary diversity by economic empowerment", caption = "Empowerment index: aggregated 
       answers to 11 questions regarding 
       women's economic empowerment")+
  theme(panel.background=element_blank(), plot.title = element_text (size  =  16, hjust  =  0.5, face="bold"), plot.caption = element_text(size = 8, vjust = 5), legend.position = "none")
p46

lm47 <- lm(DDS_child ~ index_empowerment, rti_child_female_resp)
summary(lm47)
p47 <- ggplot(rti_child_female_resp, mapping = aes(x = index_empowerment, y = DDS_child)) +
  geom_smooth(color="#0033a1", fill="steelblue")+
  labs(y = "Dietary Diversity of child", x = "Women's economic empowerment", title = "Dietary diversity of child by economic empowerment", caption = "Empowerment index: aggregated 
       answers to 11 questions regarding 
       women's economic empowerment")+
  theme(panel.background=element_blank(), plot.title = element_text (size  =  16, hjust  =  0.5, face="bold"), plot.caption = element_text(size = 8, vjust = 5), legend.position = "none")
p47

#Likert graphs empowerment
names(rti_empowerment_decision) = c("Who makes decisions on what to grow on your farm?", "Who makes decisions on what off-farm business activities to undertake and how the business is run?", "Who makes decisions on investing in bigger projects?", "Who makes decisions and takes responsibility on smaller projects?", "Who makes decisions on how much food to be sold and how much to retain for home consumption?", "Who makes decisions on workload distribution?")
p48<-likert(rti_empowerment_decision)
plot(p48, ordered=FALSE, col=c("green4", "green3", "grey", "#F7AA4E", "indianred"))+
  ggtitle("Who makes decisions on your farm? (female respondents)")+
  theme(plot.title = element_text (size  =  16, hjust  =  1, face="bold"))

names(rti_empowerment_access) = c("I can use all assets available at home for production (land, livestock, capital)", "I own the available productive assets", "I can lease any productive assets", "I can decide to sell/replace/dispose of the assets if I need to", "I access all the information I require for my economic activity")
p49 <-likert(rti_empowerment_access)
plot(p49, ordered=FALSE, col=c("green4", "green3", "grey", "#F7AA4E", "indianred"))+
  ggtitle("Access to info and productive resources (female respondents)")+
  theme(plot.title = element_text (size  =  16, hjust  =  1, face="bold"))

#F) Graphs about economic activities
lm50 <- lm(income ~ treatment, rti_complete)
summary(lm50)
se50 <- summarySE(rti_complete, measurevar="income", groupvars="treatment")
se50$income <- round(se50$income, digits = 0)
p50 <- ggplot(rti_complete, mapping = aes(x = treatment, y = income, fill = treatment)) +
  geom_bar(stat = "summary")+
  labs(y = "Income (approx.)", x = "Treatment", title = "Respondents' income by treatment", caption = "Incomes are approximated")+
  theme(panel.background=element_blank(), plot.title = element_text (size  =  16, hjust  =  0.5, face="bold"), plot.caption = element_text(size = 8, vjust = 5), legend.position = "none")+
  geom_text(data = se50, nudge_y =450, nudge_x = -0.3, aes(x = treatment, y =income, label = income))+
  geom_errorbar(data = se50, aes(ymin=income-ci, ymax=income+ci), width=.2)+
  coord_cartesian(ylim=c(0, 17000))+
  scale_fill_brewer(palette="Dark2")
p50

lm51 <- lm(income ~ treatment, rti_agri)
summary(lm51)
se51 <- summarySE(rti_agri, measurevar="income", groupvars="treatment")
se51$income <- round(se51$income, digits = 0)
p51 <- ggplot(rti_agri, mapping = aes(x = treatment, y = income, fill = treatment)) +
  geom_bar(stat = "summary")+
  labs(y = "Income (approx.)", x = "Treatment", title = "Respondents' income by treatment", caption = "Incomes are approximated")+
  theme(panel.background=element_blank(), plot.title = element_text (size  =  16, hjust  =  0.5, face="bold"), plot.caption = element_text(size = 8, vjust = 5), legend.position = "none")+
  geom_text(data = se51, nudge_y =450, nudge_x = -0.3, aes(x = treatment, y =income, label = income))+
  geom_errorbar(data = se51, aes(ymin=income-ci, ymax=income+ci), width=.2)+
  coord_cartesian(ylim=c(0, 17000))+
  scale_fill_brewer(palette="Dark2")
p51

lm52 <- lm(DDS_resp ~ income, rti_female)
summary(lm52)
p52 <- ggplot(rti_female, mapping = aes(x = income, y = DDS_resp)) +
  geom_smooth(color="#0033a1", fill="steelblue")+
  labs(x = "Income in Kenyan Shilling (approx.)", y = "Dietary Diversity", title = "Respondents' dietary diversity by income", caption = "Incomes are approximated")+
  theme(panel.background=element_blank(), plot.title = element_text (size  =  16, hjust  =  0.5, face="bold"), plot.caption = element_text(size = 8, vjust = 5), legend.position = "none")
p52

lm53 <- lm(DDS_child ~ income, rti_child_complete)
summary(lm53)
p53 <- ggplot(rti_child_complete, mapping = aes(x = income, y = DDS_child)) +
  geom_smooth(color="#0033a1", fill="steelblue")+
  labs(x = "Income in Kenyan Shilling (approx.)", y = "Dietary Diversity", title = "Dietary diversity of children by respondents' income", caption = "Incomes are approximated")+
  theme(panel.background=element_blank(), plot.title = element_text (size  =  16, hjust  =  0.5, face="bold"), plot.caption = element_text(size = 8, vjust = 5), legend.position = "none")
p53

lm54 <- lm(index_k_nutrition_overall ~ income, rti_complete)
summary(lm54)
p54 <- ggplot(rti_complete, mapping = aes(x = income, y = index_k_nutrition_overall)) +
  geom_smooth(color="#0033a1", fill="steelblue")+
  labs(x = "Income in Kenyan Shilling (approx.)", y = "Overall knowledge about nutrition", title = "Knowledge about nutrition and income", caption = "Incomes are approximated")+
  theme(panel.background=element_blank(), plot.title = element_text (size  =  16, hjust  =  0.5, face="bold"), plot.caption = element_text(size = 8, vjust = 5), legend.position = "none")
p54

#Create same graphs by income split
p55 <- ggplot(rti_complete, mapping = aes(x = treatment, y = income, fill = income_split)) +
  geom_bar(stat = "summary", position = "dodge")+
  labs(y = "Income in KSh (approx.)", x = "", title = "Respondents' income by treatment", caption = "Incomes are approximated")+
  theme(panel.background=element_blank(), plot.title = element_text (size  =  16, hjust  =  0.5, face="bold"), plot.caption = element_text(size = 8, vjust = 5), legend.title = element_blank())+
  scale_fill_brewer(palette="Dark2")
p55

#G) Graphs about Seasonality
p56 <- ggplot(rti_complete, mapping = aes(x = seccd_seccd3_d16)) +
  geom_bar(fill = "#0033a1")+
  labs(y = "Number of respondents", x = "", title = "Last planting season of respondents")+
  theme(panel.background=element_blank(), plot.title = element_text (size  =  16, hjust  =  0.5, face="bold"), plot.caption = element_text(size = 8, vjust = 5), legend.title = element_blank())
p56

p57 <- ggplot(rti_complete, mapping = aes(x = seccd_seccd3_d17)) +
  geom_bar(fill = "#0033a1")+
  labs(y = "Number of respondents", x = "", title = "Last harvesting season of respondents")+
  theme(panel.background=element_blank(), plot.title = element_text (size  =  16, hjust  =  0.5, face="bold"), plot.caption = element_text(size = 8, vjust = 5), legend.title = element_blank())
p57

p58 <- ggplot(rti_complete, mapping = aes(x=f1)) + 
  geom_bar(fill = "#0033a1")+
  labs(y = "Number of respondents", x = "", title = "How available was food in the last three months?")+
  theme(panel.background=element_blank(), plot.title = element_text (size  =  16, hjust  =  0.5, face="bold"), plot.caption = element_text(size = 8, vjust = 5), legend.title = element_blank())
p58

p59 <- ggplot(rti_complete, mapping = aes(x=f2)) + 
  geom_bar(fill = "#0033a1")+
  labs(y = "Number of respondents", x = "", title = "How affordable was food in the last three months?")+
  theme(panel.background=element_blank(), plot.title = element_text (size  =  16, hjust  =  0.5, face="bold"), plot.caption = element_text(size = 8, vjust = 5), legend.title = element_blank())
p59

p60 <- ggplot(rti_complete, mapping = aes(x=f3)) + 
  geom_bar(fill = "#0033a1")+
  labs(y = "Number of respondents", x = "", title = "How accessible was food in the last three months?")+
  theme(panel.background=element_blank(), plot.title = element_text (size  =  16, hjust  =  0.5, face="bold"), plot.caption = element_text(size = 8, vjust = 5), legend.title = element_blank())
p60

p61 <- ggplot(rti_complete, mapping = aes(x=f4)) + 
  geom_bar(fill = "#0033a1")+
  labs(y = "Number of respondents", x = "", title = "Did you experience food instability as a result of
       adverse weather in the last three months?")+
  theme(panel.background=element_blank(), plot.title = element_text (size  =  16, hjust  =  0.5, face="bold"), plot.caption = element_text(size = 8, vjust = 5), legend.title = element_blank())
p61

#Regressions with Seasonality and Food Security Index
lm62 <- lm(DDS_resp ~ seccd_seccd3_d17, rti_complete)
summary(lm62)
se62 <- summarySE(rti_complete, measurevar="DDS_resp", groupvars="seccd_seccd3_d17", na.rm = TRUE)
se62$DDS_resp <- round(se62$DDS_resp, digits = 2)
p62 <- ggplot(rti_complete, mapping = aes(x = seccd_seccd3_d17, y = DDS_resp, fill = seccd_seccd3_d17)) +
  geom_bar(stat = "summary")+
  labs(y = "Dietary Diversity", x = "Last harvesting season", title = "Dietary diversity of respondents by time of last harvesting season")+
  theme(panel.background=element_blank(), plot.title = element_text (size  =  16, hjust  =  0.5, face="bold"), plot.caption = element_text(size = 8, vjust = 5), legend.position = "none")+
  geom_text(data = se62, nudge_y =0.3, nudge_x = -0.3, aes(label = DDS_resp))+
  geom_errorbar(data = se62, aes(ymin=DDS_resp-ci, ymax=DDS_resp+ci), width=.2)+
  coord_cartesian(ylim=c(4, 8))+
  scale_fill_brewer(palette="Dark2")
p62

lm63 <- lm(DDS_child ~ seccd_seccd3_d17, rti_child_complete)
summary(lm63)
se63 <- summarySE(rti_child_complete, measurevar="DDS_child", groupvars="seccd_seccd3_d17", na.rm = TRUE)
se63$DDS_child <- round(se63$DDS_child, digits = 2)
p63 <- ggplot(rti_child_complete, mapping = aes(x = seccd_seccd3_d17, y = DDS_child, fill = seccd_seccd3_d17)) +
  geom_bar(stat = "summary")+
  labs(y = "Dietary Diversity", x = "Last harvesting season", title = "Dietary diversity of children by time of last harvesting season")+
  theme(panel.background=element_blank(), plot.title = element_text (size  =  16, hjust  =  0.5, face="bold"), plot.caption = element_text(size = 8, vjust = 5), legend.position = "none")+
  geom_text(data = se63, nudge_y =0.3, nudge_x = -0.3, aes(label = DDS_child))+
  geom_errorbar(data = se63, aes(ymin=DDS_child-ci, ymax=DDS_child+ci), width=.2)+
  coord_cartesian(ylim=c(2, 7))+
  scale_fill_brewer(palette="Dark2")
p63

lm64 <- lm(DDS_resp ~ index_food_security, rti_complete)
summary(lm64)
p64 <- ggplot(rti_complete, mapping = aes(x = index_food_security, y = DDS_resp)) +
  geom_smooth(color="#0033a1", fill="steelblue")+
  labs(y = "Dietary Diversity", x = "Food Security", title = "Food security and dietary diversity", caption = "Food security index: aggregated answers
       to 4 questions regarding food security")+
  coord_cartesian(xlim=c(0, 12))+
  theme(panel.background=element_blank(), plot.title = element_text (size  =  16, hjust  =  0.5, face="bold"), plot.caption = element_text(size = 8, vjust = 5), legend.position = "none")
p64

lm65 <- lm(DDS_child ~ index_food_security, rti_child_complete)
summary(lm65)
p65 <- ggplot(rti_child_complete, mapping = aes(x = index_food_security, y = DDS_child)) +
  geom_smooth(color="#0033a1", fill="steelblue")+
  labs(y = "Dietary Diversity of children", x = "Food Security", title = "Food security and dietary diversity of children", caption = "Food security index: aggregated answers
       to 4 questions regarding food security")+
  coord_cartesian(xlim=c(0, 12))+
  theme(panel.background=element_blank(), plot.title = element_text (size  =  16, hjust  =  0.5, face="bold"), plot.caption = element_text(size = 8, vjust = 5), legend.position = "none")
p65

#H) Likert Graph Meal Frequency
names(rti_meal_freq) = c("Milk and milk products", "Fruits and vegetables", "Legumes and nuts", "Meat and meat products", "Eggs")
p66<-likert(rti_meal_freq)
plot(p66, ordered=FALSE, centered = FALSE, col=c("green4", "green3", "grey", "#F7AA4E", "indianred"))+
  ggtitle("Frequency of consumption (female respondents)")+
  theme(plot.title = element_text (size  =  16, hjust  =  0.5, face="bold"))

#G) Graphs on Self Efficacy
names(rti_self_efficacy) = c("...even if I need a long time to develop the necessary routines", "...even if I have to try several times until it works", "...even if I have to rethink my entire way of nutrition", "...even if I do not receive a great deal of support from others when making my first attempts", "...even if I have to make a detailed plan", "...even when my finances are low", "...even when my food supply is low", "...even during this COVID season")
p67<-likert(rti_self_efficacy)
plot(p67, ordered=FALSE, centered = TRUE, col=c("green4", "green3", "#F7AA4E", "indianred"))+
  ggtitle("Self efficacy:
          I can manage to stick to healthy food...")+
  theme(plot.title = element_text (size  =  16, hjust  =  0.5, face="bold"))

lm68 <- lm(DDS_resp ~ index_self_efficacy, rti_complete)
summary(lm68)
p68 <- ggplot(rti_complete, mapping = aes(x = index_self_efficacy, y = DDS_resp)) +
  geom_smooth(color="#0033a1", fill="steelblue")+
  labs(y = "Dietary Diversity", x = "Self Efficacy", title = "Dietary diversity and self efficacy", caption = "Self efficacy index: aggregated answers 
       to 8 questions regarding self efficacy")+
  theme(panel.background=element_blank(), plot.title = element_text (size  =  16, hjust  =  0.5, face="bold"), plot.caption = element_text(size = 8, vjust = 5), legend.position = "none")
p68

lm69 <- lm(index_self_efficacy ~ treatment, rti_complete)
summary(lm69)
se69 <- summarySE(rti_complete, measurevar="index_self_efficacy", groupvars="treatment", na.rm = TRUE)
se69$index_self_efficacy <- round(se69$index_self_efficacy, digits = 2)
p69 <- ggplot(rti_complete, mapping = aes(x = treatment, y = index_self_efficacy, fill = treatment)) +
  geom_bar(stat = "summary")+
  labs(y = "Self Efficacy", x = "", title = "Self efficacy of respondents by treatment", caption = "Self efficacy index: aggregated answers 
       to 8 questions regarding self efficacy")+
  theme(panel.background=element_blank(), plot.title = element_text (size  =  16, hjust  =  0.5, face="bold"), plot.caption = element_text(size = 8, vjust = 5), legend.position = "none")+
  geom_text(data = se69, nudge_y =0.15, nudge_x = -0.3, aes(label = index_self_efficacy))+
  geom_errorbar(data = se69, aes(ymin=index_self_efficacy-ci, ymax=index_self_efficacy+ci), width=.2)+
  coord_cartesian(ylim=c(10, 15))+
  scale_fill_brewer(palette="Dark2")
p69

##H) COVID Graphs
rti_covid <- read_excel("KCDMS_covid.xlsx")
rti_covid$treatment <- factor(rti_covid$treatment,levels = c("agrinutrition", "income", "agrinutrition&income", "referral"))
rti_c_c <- select(rti_covid, covid_current, treatment)
rti_c_f <- select(rti_covid, covid_future, treatment)
rti_c_c <- subset(rti_c_c, covid_current != "NA")
rti_c_f <- subset(rti_c_f, covid_future != "NA")
rti_c_c$covid_current <- mapvalues(rti_c_c$covid_current, from = c("0", "1", "2", "3"), to = c("Not affected", "No income", "Lack of food", "No markets"))
rti_c_f$covid_future <- mapvalues(rti_c_f$covid_future, from = c("0", "1", "2", "3", "4", "5"), to = c("Not affected", "No income", "High food prices", "Limited/no access to markets", "Lack of food", "Reduced food supply"))
df= rti_c_c %>% 
  dplyr::group_by(treatment, covid_current) %>% 
  dplyr::summarise(Count=n())  %>% ungroup() %>%
  dplyr::group_by(treatment) %>%
  dplyr::mutate(Percent = 100*Count/sum(Count)) %>% ungroup()

df_f= rti_c_f %>% 
  dplyr::group_by(treatment, covid_future) %>% 
  dplyr::summarise(Count=n())  %>% ungroup() %>%
  dplyr::group_by(treatment) %>%
  dplyr::mutate(Percent = 100*Count/sum(Count)) %>% ungroup()


#How are people currently affected by COVID?
df70 <- subset(df, covid_current == "No income")
p70 <- ggplot(df70) + 
  geom_bar(aes(treatment, y = Percent, fill = treatment), stat = "identity") + 
  labs(y = "Percentage", x = "", title = "Share of people who have no income due to COVID-19", caption = "n = 283 (agrinutrition)
       n = 214 (income)
       n = 242 (agri&income)
       n = 41 (referral)")+
  theme(panel.background=element_blank(), plot.title = element_text (size  =  16, hjust  =  0, face="bold"), plot.caption = element_text(size = 8, vjust = 5), legend.position = "none") +
  scale_fill_brewer(palette="Dark2")
p70

df71 <- subset(df, covid_current == "Lack of food")
p71 <- ggplot(df71) + 
  geom_bar(aes(treatment, y = Percent, fill = treatment), stat = "identity") + 
  labs(y = "Percentage", x = "", title = "Share of people who report a lack of food due to COVID-19", caption = "n = 283 (agrinutrition)
       n = 214 (income)
       n = 242 (agri&income)
       n = 41 (referral)")+
  theme(panel.background=element_blank(), plot.title = element_text (size  =  16, hjust  =  0, face="bold"), plot.caption = element_text(size = 8, vjust = 5), legend.position = "none") +
  scale_fill_brewer(palette="Dark2")
p71

df72 <- subset(df, covid_current == "No markets")
p72 <- ggplot(df72) + 
  geom_bar(aes(treatment, y = Percent, fill = treatment), stat = "identity") + 
  labs(y = "Percentage", x = "", title = "Share of people who report low market access due to COVID-19", caption = "n = 283 (agrinutrition)
       n = 214 (income)
       n = 242 (agri&income)
       n = 41 (referral)")+
  theme(panel.background=element_blank(), plot.title = element_text (size  =  16, hjust  =  0.5, face="bold"), plot.caption = element_text(size = 8, vjust = 5), legend.position = "none") +
  scale_fill_brewer(palette="Dark2")
p72

df73 <- subset(df, covid_current == "Not affected")
p73 <- ggplot(df73) + 
  geom_bar(aes(treatment, y = Percent, fill = treatment), stat = "identity") + 
  labs(y = "Percentage", x = "", title = "Share of people who report to be not affected by COVID-19", caption = "n = 283 (agrinutrition)
       n = 214 (income)
       n = 242 (agri&income)
       n = 41 (referral)")+
  theme(panel.background=element_blank(), plot.title = element_text (size  =  16, hjust  =  0.5, face="bold"), plot.caption = element_text(size = 8, vjust = 5), legend.position = "none") +
  scale_fill_brewer(palette="Dark2")
p73

df_74 <- subset(df, covid_current == "Lack of food" | covid_current == "No income" | covid_current == "No markets" | covid_current == "Not affected")
df_74 <- subset(df_74, treatment != "referral")
df_74$Percent <- round(df_74$Percent, digits = 0)
df_74$lab <- with(df_74, paste(Count," (", Percent,"%)", sep = ""))
p74 <- ggplot(df_74, mapping = aes(x=treatment, y= Percent, fill=covid_current)) +
  geom_bar(stat = "identity", position="stack")+
  labs(y = "Percent", x = "", title = "How are people currently affected by Corona?", fill = "", caption = "Number of participants (%)
       No referral group due to low n")+
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5), panel.background=element_blank(), plot.title = element_text (size  =  16, hjust  =  0.5, face="bold")) +
  scale_fill_brewer(palette = "Dark2")+
  geom_text(aes(label = lab), size = 2.8, position = position_stack(vjust = 0.5))
p74

df_75 <- subset(df_f, covid_future == "Not affected" | covid_future == "No income" | covid_future == "High food prices" | covid_future == "Limited/no access to markets" | covid_future == "Lack of food" | covid_future == "Reduced food supply")
df_75 <- subset(df_75, treatment != "referral")
df_75$Percent <- round(df_75$Percent, digits = 0)
df_75$lab <- with(df_75, paste(Count," (", Percent,"%)", sep = ""))
p75 <- ggplot(df_75, mapping = aes(x=treatment, y= Percent, fill=covid_future)) +
  geom_bar(stat = "identity", position="stack")+
  labs(y = "Percent", x = "", title = "How do people expect to be affected by Corona in the future?", fill = "", caption = "Number of participants (%)
       No referral group due to low n")+
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5), panel.background=element_blank(), plot.title = element_text (size  =  16, hjust  =  0.0, face="bold")) +
  scale_fill_brewer(palette = "Dark2")+
  geom_text(aes(label = lab), size = 2.8, position = position_stack(vjust = 0.5))
p75

#Create likert graphs of knowledge indices by treatment
names(rti_knowledge_general_agri) = c("Good nutrition is important because it helps the body to grow and function properly; gives the needed energy to work and helps the body to fight off infections?", "A nutritious meal should have various types of foods from several food groups, in the right amounts?", "Children who do not get adequate nutrition are at risk of becoming stunted, wasted and underweight?", "Nutrition does not affect school performance for children?", "Poor nutrition does not cause someone to fall sick?")
p76<-likert(rti_knowledge_general_agri)
plot(p76, include.center=FALSE, ordered=FALSE, col=c("green4", "grey", "indianred"))+
  ggtitle("Questions about general nutrition 
          (Agrinutrition Training)")+
  theme(plot.title = element_text (size  =  16, hjust  =  0.6, face="bold"))

names(rti_knowledge_general_income) = c("Good nutrition is important because it helps the body to grow and function properly; gives the needed energy to work and helps the body to fight off infections?", "A nutritious meal should have various types of foods from several food groups, in the right amounts?", "Children who do not get adequate nutrition are at risk of becoming stunted, wasted and underweight?", "Nutrition does not affect school performance for children?", "Poor nutrition does not cause someone to fall sick?")
p77<-likert(rti_knowledge_general_income)
plot(p77, include.center=FALSE, ordered=FALSE, col=c("green4", "grey", "indianred"))+
  ggtitle("Questions about general nutrition 
          (Income Training)")+
  theme(plot.title = element_text (size  =  16, hjust  =  0.6, face="bold"))

names(rti_knowledge_general_agri_income) = c("Good nutrition is important because it helps the body to grow and function properly; gives the needed energy to work and helps the body to fight off infections?", "A nutritious meal should have various types of foods from several food groups, in the right amounts?", "Children who do not get adequate nutrition are at risk of becoming stunted, wasted and underweight?", "Nutrition does not affect school performance for children?", "Poor nutrition does not cause someone to fall sick?")
p78<-likert(rti_knowledge_general_agri_income)
plot(p78, include.center=FALSE, ordered=FALSE, col=c("green4", "grey", "indianred"))+
  ggtitle("Questions about general nutrition 
          (Agri&Income Training)")+
  theme(plot.title = element_text (size  =  16, hjust  =  0.6, face="bold"))

names(rti_knowledge_general_referral) = c("Good nutrition is important because it helps the body to grow and function properly; gives the needed energy to work and helps the body to fight off infections?", "A nutritious meal should have various types of foods from several food groups, in the right amounts?", "Children who do not get adequate nutrition are at risk of becoming stunted, wasted and underweight?", "Nutrition does not affect school performance for children?", "Poor nutrition does not cause someone to fall sick?")
p79<-likert(rti_knowledge_general_referral)
plot(p79, include.center=FALSE, ordered=FALSE, col=c("green4", "grey", "indianred"))+
  ggtitle("Questions about general nutrition 
          (Referral group)")+
  theme(plot.title = element_text (size  =  16, hjust  =  0.6, face="bold"))

names(rti_knowledge_reproductive_agri) = c("Pregnant women should limit their food intake so that the foetus does not grow big?", "Pregnant women should not eat eggs?", "Breastfeeding mothers should eat a variety of foods every day from plant and animal sources?", "Breastfeeding women do not require additional meals as long as the 3 meals they are eating during the day are nutritious?")
p80<-likert(rti_knowledge_reproductive_agri)
plot(p80, ordered=FALSE, col=c("green4", "grey", "indianred"))+
  ggtitle("Questions about nutrition for women of 
          reproductive age (Agrinutrition Training)")+
  theme(plot.title = element_text (size  =  16, hjust  =  1.0, face="bold"))

names(rti_knowledge_reproductive_agri_income) = c("Pregnant women should limit their food intake so that the foetus does not grow big?", "Pregnant women should not eat eggs?", "Breastfeeding mothers should eat a variety of foods every day from plant and animal sources?", "Breastfeeding women do not require additional meals as long as the 3 meals they are eating during the day are nutritious?")
p81<-likert(rti_knowledge_reproductive_agri_income)
plot(p81, ordered=FALSE, col=c("green4", "grey", "indianred"))+
  ggtitle("Questions about nutrition for women of 
          reproductive age (Agri&Income Training)")+
  theme(plot.title = element_text (size  =  16, hjust  =  0.8, face="bold"))

names(rti_knowledge_reproductive_income) = c("Pregnant women should limit their food intake so that the foetus does not grow big?", "Pregnant women should not eat eggs?", "Breastfeeding mothers should eat a variety of foods every day from plant and animal sources?", "Breastfeeding women do not require additional meals as long as the 3 meals they are eating during the day are nutritious?")
p82<-likert(rti_knowledge_reproductive_income)
plot(p82, ordered=FALSE, col=c("green4", "grey", "indianred"))+
  ggtitle("Questions about nutrition for women of 
          reproductive age (Income Training)")+
  theme(plot.title = element_text (size  =  16, hjust  =  0.8, face="bold"))

names(rti_knowledge_reproductive_referral) = c("Pregnant women should limit their food intake so that the foetus does not grow big?", "Pregnant women should not eat eggs?", "Breastfeeding mothers should eat a variety of foods every day from plant and animal sources?", "Breastfeeding women do not require additional meals as long as the 3 meals they are eating during the day are nutritious?")
p83<-likert(rti_knowledge_reproductive_referral)
plot(p83, ordered=FALSE, col=c("green4", "grey", "indianred"))+
  ggtitle("Questions about nutrition for women of 
          reproductive age (Referral group)")+
  theme(plot.title = element_text (size  =  16, hjust  =  0.8, face="bold"))

names(rti_knowledge_infant_agri) = c("It is good to give a baby who is below 6 months-old milk from a cow and water?", "Babies should be breastfeeding frequently, day and night at least 8 -12 times per day for the mother to continue to produce plenty of breast milk?", "A baby should not breastfeed if the mother is sick?", "A baby who is below 6 months can be given some food (even in small amounts) especially if they keep on crying as this indicates that breastmilk is not enough?", "Babies above six months should be introduced to nutritious foods from the different food groups in addition to breast milk?", "The more the types of cereals mixed in a baby’s food, the more nutritious the food?", "A baby who is 12-23 months should stop breastfeeding and now eat family food?", "Babies at six months should eat at least 2 times a day?", "Babies 9-11 months need to eat 3 quarter a bowl of food at a time?", "Children 12-23 months should eat 6 times a day because they are bigger?", "Children with poor appetites should be fed small but frequent meals with a variety of foods from different food groups?", "When a child refuses to eat they should be forced to eat?")
p84<-likert(rti_knowledge_infant_agri)
plot(p84, ordered=FALSE, col=c("green4", "grey", "indianred"))+
  ggtitle("Questions about infant and young 
          child nutrition (Agrinutrition Training)")+
  theme(plot.title = element_text (size  =  16, hjust  =  0.9, face="bold"))

names(rti_knowledge_infant_agri_income) = c("It is good to give a baby who is below 6 months-old milk from a cow and water?", "Babies should be breastfeeding frequently, day and night at least 8 -12 times per day for the mother to continue to produce plenty of breast milk?", "A baby should not breastfeed if the mother is sick?", "A baby who is below 6 months can be given some food (even in small amounts) especially if they keep on crying as this indicates that breastmilk is not enough?", "Babies above six months should be introduced to nutritious foods from the different food groups in addition to breast milk?", "The more the types of cereals mixed in a baby’s food, the more nutritious the food?", "A baby who is 12-23 months should stop breastfeeding and now eat family food?", "Babies at six months should eat at least 2 times a day?", "Babies 9-11 months need to eat 3 quarter a bowl of food at a time?", "Children 12-23 months should eat 6 times a day because they are bigger?", "Children with poor appetites should be fed small but frequent meals with a variety of foods from different food groups?", "When a child refuses to eat they should be forced to eat?")
p85<-likert(rti_knowledge_infant_agri_income)
plot(p85, ordered=FALSE, col=c("green4", "grey", "indianred"))+
  ggtitle("Questions about infant and young 
          child nutrition (Agri&Income Training)")+
  theme(plot.title = element_text (size  =  16, hjust  =  0.9, face="bold"))

names(rti_knowledge_infant_income) = c("It is good to give a baby who is below 6 months-old milk from a cow and water?", "Babies should be breastfeeding frequently, day and night at least 8 -12 times per day for the mother to continue to produce plenty of breast milk?", "A baby should not breastfeed if the mother is sick?", "A baby who is below 6 months can be given some food (even in small amounts) especially if they keep on crying as this indicates that breastmilk is not enough?", "Babies above six months should be introduced to nutritious foods from the different food groups in addition to breast milk?", "The more the types of cereals mixed in a baby’s food, the more nutritious the food?", "A baby who is 12-23 months should stop breastfeeding and now eat family food?", "Babies at six months should eat at least 2 times a day?", "Babies 9-11 months need to eat 3 quarter a bowl of food at a time?", "Children 12-23 months should eat 6 times a day because they are bigger?", "Children with poor appetites should be fed small but frequent meals with a variety of foods from different food groups?", "When a child refuses to eat they should be forced to eat?")
p86<-likert(rti_knowledge_infant_income)
plot(p86, ordered=FALSE, col=c("green4", "grey", "indianred"))+
  ggtitle("Questions about infant and young 
          child nutrition (Income Training)")+
  theme(plot.title = element_text (size  =  16, hjust  =  0.8, face="bold"))

names(rti_knowledge_infant_referral) = c("It is good to give a baby who is below 6 months-old milk from a cow and water?", "Babies should be breastfeeding frequently, day and night at least 8 -12 times per day for the mother to continue to produce plenty of breast milk?", "A baby should not breastfeed if the mother is sick?", "A baby who is below 6 months can be given some food (even in small amounts) especially if they keep on crying as this indicates that breastmilk is not enough?", "Babies above six months should be introduced to nutritious foods from the different food groups in addition to breast milk?", "The more the types of cereals mixed in a baby’s food, the more nutritious the food?", "A baby who is 12-23 months should stop breastfeeding and now eat family food?", "Babies at six months should eat at least 2 times a day?", "Babies 9-11 months need to eat 3 quarter a bowl of food at a time?", "Children 12-23 months should eat 6 times a day because they are bigger?", "Children with poor appetites should be fed small but frequent meals with a variety of foods from different food groups?", "When a child refuses to eat they should be forced to eat?")
p87<-likert(rti_knowledge_infant_referral)
plot(p87, ordered=FALSE, col=c("green4", "grey", "indianred"))+
  ggtitle("Questions about infant and young 
          child nutrition (Referral group)")+
  theme(plot.title = element_text (size  =  16, hjust  =  0.9, face="bold"))

#Likert Graphs for meal frequencies by treatment
names(rti_meal_freq_agri) = c("Milk and milk products", "Fruits and vegetables", "Legumes and nuts", "Meat and meat products", "Eggs")
p88<-likert(rti_meal_freq_agri)
plot(p88, ordered=FALSE, centered = FALSE, col=c("green4", "green3", "grey", "#F7AA4E", "indianred"))+
  ggtitle("Frequency of consumption 
          (female respondents, agrinutrition training)")+
  theme(plot.title = element_text (size  =  16, hjust  =  0.5, face="bold"))

names(rti_meal_freq_agri_income) = c("Milk and milk products", "Fruits and vegetables", "Legumes and nuts", "Meat and meat products", "Eggs")
p89<-likert(rti_meal_freq_agri_income)
plot(p89, ordered=FALSE, centered = FALSE, col=c("green4", "green3", "grey", "#F7AA4E", "indianred"))+
  ggtitle("Frequency of consumption 
          (female respondents, agri&income training)")+
  theme(plot.title = element_text (size  =  16, hjust  =  0.5, face="bold"))

names(rti_meal_freq_income) = c("Milk and milk products", "Fruits and vegetables", "Legumes and nuts", "Meat and meat products", "Eggs")
p90<-likert(rti_meal_freq_income)
plot(p90, ordered=FALSE, centered = FALSE, col=c("green4", "green3", "grey", "#F7AA4E", "indianred"))+
  ggtitle("Frequency of consumption 
          (female respondents, income training)")+
  theme(plot.title = element_text (size  =  16, hjust  =  0.6, face="bold"))

names(rti_meal_freq_referral) = c("Milk and milk products", "Fruits and vegetables", "Legumes and nuts", "Meat and meat products", "Eggs")
p91<-likert(rti_meal_freq_referral)
plot(p91, ordered=FALSE, centered = FALSE, col=c("green4", "green3", "grey", "#F7AA4E", "indianred"))+
  ggtitle("Frequency of consumption 
          (female respondents, referral group)")+
  theme(plot.title = element_text (size  =  16, hjust  =  0.6, face="bold"))

#Check correlation between empowerment questions and DDS
lm92 <- lm(DDS_resp ~ secc_access_info_c07, rti_female)
summary(lm92)

#Average Dietary Dieversity score by county for women
se93 <- summarySE(rti_female, measurevar="DDS_resp", groupvars="seca_a13")
se93$DDS_resp <- round(se93$DDS_resp, digits = 2)
p93 <- ggplot(rti_female, mapping = aes(x = seca_a13, y = DDS_resp, fill = seca_a13)) +
  geom_bar(stat = "summary")+
  labs(y = "Dietary Diversity Score", x = "County", title = "Dietary diversity by county (female respondents)")+
  theme(panel.background=element_blank(), plot.title = element_text (size  =  16, hjust  =  0.5, face="bold"), legend.position = "none")+
  geom_text(data = se93, nudge_y =0.2, nudge_x = -0.3, aes(x = seca_a13, y =DDS_resp, label =DDS_resp))+
  geom_errorbar(data = se93, aes(ymin=DDS_resp-ci, ymax=DDS_resp+ci), width=.2)+
  coord_cartesian(ylim=c(0, 10))
p93

se94 <- summarySE(rti_child_complete, measurevar="DDS_child", groupvars="seca_a13")
se94$DDS_child <- round(se94$DDS_child, digits = 2)
p94 <- ggplot(rti_child_complete, mapping = aes(x = seca_a13, y = DDS_child, fill = seca_a13)) +
  geom_bar(stat = "summary")+
  labs(y = "Dietary Diversity Score", x = "County", title = "Dietary diversity by county (children)")+
  theme(panel.background=element_blank(), plot.title = element_text (size  =  16, hjust  =  0.5, face="bold"), legend.position = "none")+
  geom_text(data = se94, nudge_y =0.15, nudge_x = -0.3, aes(x = seca_a13, y =DDS_child, label =DDS_child))+
  geom_errorbar(data = se94, aes(ymin=DDS_child-ci, ymax=DDS_child+ci), width=.2)+
  coord_cartesian(ylim=c(0, 7))
p94


##Additional analysis without graphs and less meticulously
"Grains", 1600/1630*100
"Beans", (399+576)/1630*100
"Nuts", (132+214)/1630*100
"Dairy", (653+713)/1630*100
"Eggs", (165+344)/1630*100
"Meat", (298+434)/1630*100
"Vegetables", (741+781)/1630*100
"Fruits", (599+681)/1630*100
"Other fruits", (420+519)/1630*100
"Other vegetables", (663+712)/1630*100

df<-data.frame("Grains",  1600/1630*100)
names(df) <- c("food_type", "Percentage")
dh <- data.frame("Beans", (399+576)/1630*100)
names(dh) <- c("food_type", "Percentage")
di <- data.frame("Nuts", (132+214)/1630*100)
names(di) <- c("food_type", "Percentage")
dj <- data.frame("Dairy", (653+713)/1630*100)
names(dj) <- c("food_type", "Percentage")
dk <- data.frame("Eggs", (165+344)/1630*100)
names(dk) <- c("food_type", "Percentage")
dl <- data.frame("Meat", (298+434)/1630*100)
names(dl) <- c("food_type", "Percentage")
dm <- data.frame("Vegetables", (741+781)/1630*100)
names(dm) <- c("food_type", "Percentage")
dn <- data.frame("Fruits", (599+681)/1630*100)
names(dn) <- c("food_type", "Percentage")
do <- data.frame("Other fruits", (420+519)/1630*100)
names(do) <- c("food_type", "Percentage")
dp <- data.frame("Other vegetables", (663+712)/1630*100)
names(dp) <- c("food_type", "Percentage")

df <- rbind(df, dh, di, dj, dk, dl, dm, dn, do, dp)

df$Percentage <- round(df$Percentage, digits = 2)
p96 <- ggplot(df, mapping = aes(x = food_type, y = Percentage, fill = food_type)) +
  geom_bar(stat = "summary")+
  labs(y = "Percentage", x = "Food type", title = "Consumption of different food types (female respondents)")+
  theme(axis.text.x = element_text(angle = 30, hjust = 0.5), panel.background=element_blank(), plot.title = element_text (size  =  16, hjust  =  0.5, face="bold"), legend.position = "none")+
  geom_text(nudge_y =3, nudge_x = -0, aes(x = food_type, y =Percentage, label =Percentage))
p96



#Check the same for children
dds_5 <- subset(rti_child_complete, value >= 0)

dds_5$grain1 <- rowSums(dds_5[, c("grain_food_f05__1.x", "grain_food_f06__1.x")], na.rm=FALSE) 
dds_5$grain2 <- rowSums(dds_5[,c("grain_food_f_f05_f__1.x", "grain_food_f_f06_f__1.x")], na.rm=FALSE) 
dds_5$grain3 <- rowSums(dds_5[,c("grain_food_f_f05_f__1.y", "grain_food_f_f06_f__1.y")], na.rm=FALSE) 
dds_5$grain4 <- rowSums(dds_5[, c("grain_food_f05__1.y", "grain_food_f06__1.y")], na.rm=FALSE)
dds_5$grain5 <- rowSums(dds_5[, c("grain_food_f05__2.x", "grain_food_f06__2.x")], na.rm=FALSE) 
dds_5$grain6 <- rowSums(dds_5[,c("grain_food_f_f05_f__2.x", "grain_food_f_f06_f__2.x")], na.rm=FALSE) 
dds_5$grain7 <- rowSums(dds_5[,c("grain_food_f_f05_f__2.y", "grain_food_f_f06_f__2.y")], na.rm=FALSE) 
dds_5$grain8 <- rowSums(dds_5[, c("grain_food_f05__2.y", "grain_food_f06__2.y")], na.rm=FALSE) 
dds_5$grain9 <- rowSums(dds_5[,c("grain_food_f_f05_f__3.x", "grain_food_f_f06_f__3.x")], na.rm=FALSE) 
dds_5$grain10 <- rowSums(dds_5[,c("grain_food_f_f05_f__3.y", "grain_food_f_f06_f__3.y")], na.rm=FALSE) 
test$grain_c <- 348.5/375

dds_5$beans1 <- rowSums(dds_5[, c("beans_food_f08__1.x", "beans_food_f09__1.x")], na.rm=FALSE) 
dds_5$beans2 <- rowSums(dds_5[,c("beans_food_f_f08_f__1.x", "beans_food_f_f09_f__1.x")], na.rm=FALSE) 
dds_5$beans3 <- rowSums(dds_5[,c("beans_food_f_f08_f__1.y", "beans_food_f_f09_f__1.y")], na.rm=FALSE) 
dds_5$beans4 <- rowSums(dds_5[, c("beans_food_f08__1.y", "beans_food_f09__1.y")], na.rm=FALSE)
dds_5$beans5 <- rowSums(dds_5[, c("beans_food_f08__2.x", "beans_food_f09__2.x")], na.rm=FALSE) 
dds_5$beans6 <- rowSums(dds_5[,c("beans_food_f_f08_f__2.x", "beans_food_f_f09_f__2.x")], na.rm=FALSE) 
dds_5$beans7 <- rowSums(dds_5[,c("beans_food_f_f08_f__2.y", "beans_food_f_f09_f__2.y")], na.rm=FALSE) 
dds_5$beans8 <- rowSums(dds_5[, c("beans_food_f08__2.y", "beans_food_f09__2.y")], na.rm=FALSE)
dds_5$beans9 <- rowSums(dds_5[,c("beans_food_f_f08_f__3.x", "beans_food_f_f09_f__3.x")], na.rm=FALSE) 
dds_5$beans10 <- rowSums(dds_5[,c("beans_food_f_f08_f__3.y", "beans_food_f_f09_f__3.y")], na.rm=FALSE) 
test$beans_c <- 197/375

dds_5$dairy1 <- rowSums(dds_5[, c("dairy_food_f11__1.x", "dairy_food_f12__1.x")], na.rm=FALSE) 
dds_5$dairy2 <- rowSums(dds_5[,c("dairy_food_f_f11_f__1.x", "dairy_food_f_f12_f__1.x")], na.rm=FALSE) 
dds_5$dairy3 <- rowSums(dds_5[,c("dairy_food_f_f11_f__1.y", "dairy_food_f_f12_f__1.y")], na.rm=FALSE) 
dds_5$dairy4 <- rowSums(dds_5[, c("dairy_food_f11__1.y", "dairy_food_f12__1.y")], na.rm=FALSE)
dds_5$dairy5 <- rowSums(dds_5[, c("dairy_food_f11__2.x", "dairy_food_f12__2.x")], na.rm=FALSE) 
dds_5$dairy6 <- rowSums(dds_5[,c("dairy_food_f_f11_f__2.x", "dairy_food_f_f12_f__2.x")], na.rm=FALSE) 
dds_5$dairy7 <- rowSums(dds_5[,c("dairy_food_f_f11_f__2.y", "dairy_food_f_f12_f__2.y")], na.rm=FALSE) 
dds_5$dairy8 <- rowSums(dds_5[, c("dairy_food_f11__2.y", "dairy_food_f12__2.y")], na.rm=FALSE)
dds_5$dairy9 <- rowSums(dds_5[,c("dairy_food_f_f11_f__3.x", "dairy_food_f_f12_f__3.x")], na.rm=FALSE) 
dds_5$dairy10 <- rowSums(dds_5[,c("dairy_food_f_f11_f__3.y", "dairy_food_f_f12_f__3.y")], na.rm=FALSE) 
test$dairy_c <- 317.5/375

dds_5$meat1 <- rowSums(dds_5[, c("meatfi_food_f14__1.x", "meatfi_food_f15__1.x")], na.rm=FALSE) 
dds_5$meat2 <- rowSums(dds_5[,c("meatfi_food_f_f14_f__1.x", "meatfi_food_f_f15_f__1.x")], na.rm=FALSE) 
dds_5$meat3 <- rowSums(dds_5[,c("meatfi_food_f_f14_f__1.y", "meatfi_food_f_f15_f__1.y")], na.rm=FALSE) 
dds_5$meat4 <- rowSums(dds_5[, c("meatfi_food_f14__1.y", "meatfi_food_f15__1.y")], na.rm=FALSE)
dds_5$meat5 <- rowSums(dds_5[, c("meatfi_food_f14__2.x", "meatfi_food_f15__2.x")], na.rm=FALSE) 
dds_5$meat6 <- rowSums(dds_5[,c("meatfi_food_f_f14_f__2.x", "meatfi_food_f_f15_f__2.x")], na.rm=FALSE) 
dds_5$meat7 <- rowSums(dds_5[,c("meatfi_food_f_f14_f__2.y", "meatfi_food_f_f15_f__2.y")], na.rm=FALSE) 
dds_5$meat8 <- rowSums(dds_5[, c("meatfi_food_f14__2.y", "meatfi_food_f15__2.y")], na.rm=FALSE)
dds_5$meat9 <- rowSums(dds_5[,c("meatfi_food_f_f14_f__3.x", "meatfi_food_f_f15_f__3.x")], na.rm=FALSE) 
dds_5$meat10 <- rowSums(dds_5[,c("meatfi_food_f_f14_f__3.y", "meatfi_food_f_f15_f__3.y")], na.rm=FALSE) 
test$meat_c <- 210.5/375

dds_5$eggs1 <- rowSums(dds_5[, c("eggs_food_f17__1.x", "eggs_food_f18__1.x")], na.rm=FALSE) 
dds_5$eggs2 <- rowSums(dds_5[,c("eggs_food_f_f17_f__1.x", "eggs_food_f_f18_f__1.x")], na.rm=FALSE) 
dds_5$eggs3 <- rowSums(dds_5[,c("eggs_food_f_f17_f__1.y", "eggs_food_f_f18_f__1.y")], na.rm=FALSE) 
dds_5$eggs4 <- rowSums(dds_5[, c("eggs_food_f17__1.y", "eggs_food_f18__1.y")], na.rm=FALSE)
dds_5$eggs5 <- rowSums(dds_5[, c("eggs_food_f17__2.x", "eggs_food_f18__2.x")], na.rm=FALSE) 
dds_5$eggs6 <- rowSums(dds_5[,c("eggs_food_f_f17_f__2.x", "eggs_food_f_f18_f__2.x")], na.rm=FALSE) 
dds_5$eggs7 <- rowSums(dds_5[,c("eggs_food_f_f17_f__2.y", "eggs_food_f_f18_f__2.y")], na.rm=FALSE) 
dds_5$eggs8 <- rowSums(dds_5[, c("eggs_food_f17__2.y", "eggs_food_f18__2.y")], na.rm=FALSE)
dds_5$eggs9 <- rowSums(dds_5[,c("eggs_food_f_f17_f__3.x", "eggs_food_f_f18_f__3.x")], na.rm=FALSE) 
dds_5$eggs10 <- rowSums(dds_5[,c("eggs_food_f_f17_f__3.y", "eggs_food_f_f18_f__3.y")], na.rm=FALSE) 
test$eggs_c <- 130.5/375

dds_5$fruits1 <- rowSums(dds_5[, c("fruits_food_f20__1.x", "fruits_food_f21__1.x")], na.rm=FALSE) 
dds_5$fruits2 <- rowSums(dds_5[,c("fruits_food_f_f20_f__1.x", "fruits_food_f_f21_f__1.x")], na.rm=FALSE) 
dds_5$fruits3 <- rowSums(dds_5[,c("fruits_food_f_f20_f__1.y", "fruits_food_f_f21_f__1.y")], na.rm=FALSE) 
dds_5$fruits4 <- rowSums(dds_5[, c("fruits_food_f20__1.y", "fruits_food_f21__1.y")], na.rm=FALSE)
dds_5$fruits5 <- rowSums(dds_5[, c("fruits_food_f20__2.x", "fruits_food_f21__2.x")], na.rm=FALSE) 
dds_5$fruits6 <- rowSums(dds_5[,c("fruits_food_f_f20_f__2.x", "fruits_food_f_f21_f__2.x")], na.rm=FALSE) 
dds_5$fruits7 <- rowSums(dds_5[,c("fruits_food_f_f20_f__2.y", "fruits_food_f_f21_f__2.y")], na.rm=FALSE) 
dds_5$fruits8 <- rowSums(dds_5[, c("fruits_food_f20__2.y", "fruits_food_f21__2.y")], na.rm=FALSE)
dds_5$fruits9 <- rowSums(dds_5[,c("fruits_food_f_f20_f__3.x", "fruits_food_f_f21_f__3.x")], na.rm=FALSE) 
dds_5$fruits10 <- rowSums(dds_5[,c("fruits_food_f_f20_f__3.y", "fruits_food_f_f21_f__3.y")], na.rm=FALSE) 
test$fruits_c <- 315/375

dds_5$other_fruits1 <- rowSums(dds_5[, c("other_fruits_food_f23__1.x", "other_fruits_food_f24__1.x")], na.rm=FALSE) 
dds_5$other_fruits2 <- rowSums(dds_5[,c("other_fruits_food_f_f23_f__1.x", "other_fruits_food_f_f24_f__1.x")], na.rm=FALSE) 
dds_5$other_fruits3 <- rowSums(dds_5[,c("other_fruits_food_f_f23_f__1.y", "other_fruits_food_f_f24_f__1.y")], na.rm=FALSE) 
dds_5$other_fruits4 <- rowSums(dds_5[, c("other_fruits_food_f23__1.y", "other_fruits_food_f24__1.y")], na.rm=FALSE)
dds_5$other_fruits5 <- rowSums(dds_5[, c("other_fruits_food_f23__2.x", "other_fruits_food_f24__2.x")], na.rm=FALSE) 
dds_5$other_fruits6 <- rowSums(dds_5[,c("other_fruits_food_f_f23_f__2.x", "other_fruits_food_f_f24_f__2.x")], na.rm=FALSE) 
dds_5$other_fruits7 <- rowSums(dds_5[,c("other_fruits_food_f_f23_f__2.y", "other_fruits_food_f_f24_f__2.y")], na.rm=FALSE) 
dds_5$other_fruits8 <- rowSums(dds_5[, c("other_fruits_food_f23__2.y", "other_fruits_food_f24__2.y")], na.rm=FALSE)
dds_5$other_fruits9 <- rowSums(dds_5[,c("other_fruits_food_f_f23_f__3.x", "other_fruits_food_f_f24_f__3.x")], na.rm=FALSE) 
dds_5$other_fruits10 <- rowSums(dds_5[,c("other_fruits_food_f_f23_f__3.y", "other_fruits_food_f_f24_f__3.y")], na.rm=FALSE) 
test$other_fruits_c <- 304/375

"Grains", 348.5/375*100
"Beans", 197/375*100
"Dairy", 317.5/375*100
"Eggs", 130.5/375*100
"Meat", 210.5/375*100
"Fruits", 315/375*100
"Other fruits", 304/375*100

df<-data.frame("Grains", 348.5/375*100)
names(df) <- c("food_type", "Percentage")
dh <- data.frame("Beans", 197/375*100)
names(dh) <- c("food_type", "Percentage")
dj <- data.frame("Dairy", 317.5/375*100)
names(dj) <- c("food_type", "Percentage")
dk <- data.frame("Eggs", 130.5/375*100)
names(dk) <- c("food_type", "Percentage")
dl <- data.frame("Meat", 210.5/375*100)
names(dl) <- c("food_type", "Percentage")
dn <- data.frame("Fruits", 315/375*100)
names(dn) <- c("food_type", "Percentage")
do <- data.frame("Other fruits", 304/375*100)
names(do) <- c("food_type", "Percentage")

df <- rbind(df, dh, dj, dk, dl, dn, do)

df$Percentage <- round(df$Percentage, digits = 2)
p97 <- ggplot(df, mapping = aes(x = food_type, y = Percentage, fill = food_type)) +
  geom_bar(stat = "summary")+
  labs(y = "Percentage", x = "Food type", title = "Consumption of different food types (children)")+
  theme(axis.text.x = element_text(angle = 30, hjust = 0.5), panel.background=element_blank(), plot.title = element_text (size  =  16, hjust  =  0.5, face="bold"), legend.position = "none")+
  geom_text(nudge_y =3, nudge_x = -0, aes(x = food_type, y =Percentage, label =Percentage))
p97
