library(dplyr)
library(ggplot2)
library(corrplot)
library(vcd)
library(forcats)

bdhs <- read.csv('Data/bdhs.csv')

# remove dead children - child_alive = B5
bdhs_cl <- bdhs[bdhs$B5 == 1, ]

bdhs_cl <- bdhs_cl %>%
  select(
    breastfeeding_duration = M4, #97 is inconsistent, 98 is don't know
    twin = B0,
    birth_order = BORD,
    diarrhea = H11, #8 is don't know
    fever = H22, #8 is don't know
    cough = H31, #8 is don't know
    vit_a = H34, #8 is don't know
    
    maternal_age = V012,
    first_birth = V212,
    maternal_BMI = V445, #9998 is flagged cases
    maternal_education = V106,
    maternal_job = V717, #98 is don't know
    c_section = M17,
    
    paternal_education = V701, #8 is don't know
    residence = V025,
    region = V024,
    religion = V130, #96 is others
    household_size = V136,
    wealth_index = V190,
    toilet = V116,
    water = V113,
    
    child_weight_kg = HW2, #9994 not present, 9995 refused, 9996 other
    waz = HW71 #weight adjusted for age/sex according to median, 9996 height is outside plausible limits, 9997 age in days is out of plausible limits, 9998 is flagged cases
  )

nrow(bdhs_cl)
# a total of 8508 respondents before data cleaning

# breastfeeding_duration
# probably shouldn't keep breastfeeding since ~39% is NA and less than 3 percent in the Never category
bdhs_cl$breastfeeding_duration <- factor(bdhs_cl$breastfeeding_duration,
                                           levels = c(93, 94, 95),
                                           labels = c("Previously", "Never", "Still"))

# birth_order
bdhs_cl$birth_order <- fct_collapse(factor(bdhs_cl$birth_order),
                                      "4+" = c("4", "5", "6", "7", "8", "9", "10"))

# diarrhea - had diarrhea in last two weeks
# missing 0.15% of the dataset - could consider imputation?
bdhs_cl$diarrhea <- factor(bdhs_cl$diarrhea,
                             levels = c(0, 2),
                             labels = c("No", "Yes"))

# fever
# missing 0.17% of the dataset - could consider imputation?
bdhs_cl$fever <- factor(bdhs_cl$fever,
                             levels = c(0, 1),
                             labels = c("No", "Yes"))
# cough
# missing 0.19% of the dataset - could consider imputation?
bdhs_cl$cough <- factor(bdhs_cl$cough,
                          levels = c(0, 2),
                          labels = c("No", "Yes"))

# twin
# low frequencies - probably can't use
bdhs_cl$twin <- factor(bdhs_cl$twin,
                          levels = c(0, 1, 2),
                          labels = c("Single Birth", "1st of Mult", "2nd of Mult"))

# vit_a
# missing 0.5% of data
bdhs_cl$vit_a <- factor(bdhs_cl$vit_a,
                         levels = c(0, 1),
                         labels = c("No", "Yes"))

# maternal_BMI
bdhs_cl$maternal_BMI[bdhs_cl$maternal_BMI %in% c(9998)] <- NA

# maternal_education
bdhs_cl$maternal_education <- factor(bdhs_cl$maternal_education,
                          levels = c(0, 1, 2, 3),
                          labels = c("None", "Primary", "Secondary", "Higher"))

# c_section
# probably shouldn't keep c_section since ~39% is NA
bdhs_cl$c_section <- factor(bdhs_cl$c_section,
                              levels = c(0, 1),
                              labels = c("No", "Yes"))
# maternal_job
bdhs_cl$maternal_job <- fct_collapse(factor(bdhs_cl$maternal_job),
                              "No" = c("0"),
                              "Yes" = c("1", "2", "3", "4", "5",
                                        "6", "7", "8", "9"))

# paternal_education
# missing 1.4% of the dataset - could consider imputation?
bdhs_cl$paternal_education <- factor(bdhs_cl$paternal_education,
                                 levels = c(0, 1, 2, 3),
                                 labels = c("None", "Primary", "Secondary", "Higher"))

# residence
bdhs_cl$residence <- factor(bdhs_cl$residence,
                                 levels = c(1, 2),
                                 labels = c("Urban", "Rural"))

# religion
bdhs_cl$religion <- fct_collapse(factor(bdhs_cl$religion),
                                      "Muslim" = c("1"),
                                      "Non-Muslim" = c("2", "3", "4", "96"))

# household_size
bdhs_cl$household_size <- fct_collapse(factor(bdhs_cl$household_size),
                                          "<=3" = c("1", "2", "3"),
                                          "4" = c("4"),
                                          "5" = c("5"),
                                          "6" = c("6"),
                                          "7" = c("7"),
                                          "8-9" = c("8", "9"),
                                          ">=10" = c("10", "11", "12", "13", "14", "15", 
                                                     "16", "17", "18", "19", "20", "21",
                                                     "22", "25"))

# wealth_index
bdhs_cl$wealth_index <- factor(bdhs_cl$wealth_index,
                                  levels = c(1, 2, 3, 4, 5),
                                  labels = c("Poorest", "Poorer", "Middle", "Richer", "Richest"))

# toilet
bdhs_cl$toilet <- fct_collapse(factor(bdhs_cl$toilet),
                                  "Improved" = c("11", "12", "13", "21", "22", "41"),
                                  "Unimproved" = c("14", "23", "31", "42", "43"))
bdhs_cl$toilet <- factor(bdhs_cl$toilet,
                            levels = c("Improved", "Unimproved"),
                            labels = c("Improved", "Unimproved"))

# water
bdhs_cl$water <- fct_collapse(factor(bdhs_cl$water),
                                  "Improved" = c("11", "12", "13", "14", "21", "31", 
                                                 "41", "43", "51", "61", "62", "71"),
                                  "Unimproved" = c("32", "42"))
bdhs_cl$water <- factor(bdhs_cl$water,
                            levels = c("Improved", "Unimproved"),
                            labels = c("Improved", "Unimproved"))

# region
bdhs_cl$region <- factor(bdhs_cl$region,
                                  levels = c(1, 2, 3, 4, 5, 6, 7, 8),
                                  labels = c("Barishal", "Chattogram", "Dhaka", "Khulna",
                                             "Mymensingh", "Rajshahi", "Rangpur", "Sylhet"))
# child_weight_kg
bdhs_cl$child_weight_kg[bdhs_cl$child_weight_kg %in% c(9994, 9995, 9996)] <- NA

# waz
# missing 0.05% of data
bdhs_cl$waz[bdhs_cl$waz %in% c(9996, 9997, 9998)] <- NA

# cleaning continuous vars a little - need to edit to match above data conventions
bdhs_cl$maternal_BMI <- bdhs_cl$maternal_BMI/100
bdhs_cl$child_weight_kg <- bdhs_cl$child_weight_kg/10
bdhs_cl$waz <- bdhs_cl$waz/100

# add underweight var to cleaned dataset
bdhs_cl$underweight <- ifelse(bdhs_cl$waz < -2, 1, 0)
bdhs_cl$underweight <- factor(bdhs_cl$underweight)

write.csv(bdhs_cl, file = "Data/01-result.csv")